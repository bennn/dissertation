#lang racket/base

(provide
  typeset
)

;; ----------------------------------------------------------------------------

(require
  "../base/untyped.rkt"
  "../base/core.rkt"
  "../base/quad.rkt"
  (only-in racket/list append* empty empty? split-at drop-right)
  racket/class
  (only-in racket/sequence sequence->list)
  (only-in math/flonum fl+ fl fl>)
)
(require (only-in "quads.rkt"
  quads->doc
  quads->page
  quads->block
  quad-attrs
  line
  quad-car
  quad-name
  quad-attr-ref
  group-quad-list
  quad-list
  quad-has-attr?
  quads->column
  page
  column))
(require (only-in "wrap.rkt"
  insert-spacers-in-line
  wrap-adaptive
  wrap-best
  wrap-first
  fill
  add-horiz-positions))
(require (only-in "world.rkt"
  world:line-looseness-key
  world:allow-hyphenated-last-word-in-paragraph
  world:line-looseness-tolerance
  world:line-index-key
  world:measure-key
  world:use-hyphenation?
  world:max-quality
  world:total-lines-key
  world:draft-quality
  world:quality-key
  world:quality-key-default
  world:paper-width-default
  world:column-count-key
  world:column-count-key-default
  world:column-gutter-key
  world:column-gutter-key-default
  world:column-index-key
  world:min-first-lines
  world:min-last-lines
  world:minimum-lines-per-column
  world:default-lines-per-column))
(require (only-in "measure.rkt"
  round-float
  load-text-cache-file
  update-text-cache-file))
(require (only-in "utils.rkt"
  merge-attrs
  split-last
  join-quads
  hyphenate-quad
  quad-map
  group-quad-attr-set*
  quad-attr-set*
  attr-change
  compute-line-height
  add-vert-positions
  split-quad))
(require (only-in "sugar-list.rkt"
 slice-at))
;; bg: should maybe import this
(require (only-in "../base/csp/csp.rkt"
  problem%))

;; =============================================================================

(define (typeset x)
  (load-text-cache-file)
  (define pages (append*
                 (for/list
                   ([multipage (in-list (input->nested-blocks x))])
                   (columns->pages (append*
                                    (for/list
                                      ([multicolumn (in-list multipage)])
                                      (lines->columns (append*
                                                       (for/list
                                                         ([block-quads (in-list multicolumn)])
                                                         (block-quads->lines block-quads))))))))))
  (define doc (pages->doc pages))
  (update-text-cache-file)
  doc)

;; -----------------------------------------------------------------------------

(define (cons-reverse xs ys)
  (cons (reverse xs) ys))


(define (input->nested-blocks i)
  (define-values (mps mcs bs b)
    (for/fold ([multipages empty]
               [multicolumns empty]
               [blocks empty]
               [block-acc empty])
              ([q (in-list (split-quad i))])
      (cond
        [(page-break? q) (values (cons-reverse (cons-reverse (cons-reverse block-acc blocks) multicolumns) multipages) empty empty empty)]
        [(column-break? q) (values multipages (cons-reverse (cons-reverse block-acc blocks) multicolumns) empty empty)]
        [(block-break? q) (values multipages multicolumns (cons-reverse block-acc blocks) empty)]
        [else (values multipages multicolumns blocks (cons q block-acc))])))
  (reverse (cons-reverse (cons-reverse (cons-reverse b bs) mcs) mps)))

(define (merge-adjacent-within q)
  (quad (quad-name q) (quad-attrs q) (join-quads (assert (quad-list q) listof-quad?))))

(define (hyphenate-quad-except-last-word q)
  (define-values (first-quads last-quad) (split-last (quad-list q)))
  (quad (quad-name q) (quad-attrs q) (append (map hyphenate-quad first-quads) (list last-quad))))

(define (average-looseness lines)
  (if (<= (length lines) 1)
      0.0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (round-float (/ (foldl fl+ 0.0 (map (λ(line) (assert (quad-attr-ref line world:line-looseness-key 0.0) flonum?)) lines-to-measure)) (- (fl (length lines)) 1.0))))))

;; todo: introduce a Quad subtype where quad-list is guaranteed to be all Quads (no strings)
(define (block->lines b)
  (define quality (assert (quad-attr-ref b world:quality-key (world:quality-key-default)) index?))
  (define (wrap-quads qs)
    (define wrap-proc (cond
                        [(>= quality world:max-quality) wrap-best]
                        [(<= quality world:draft-quality) wrap-first]
                        [else wrap-adaptive]))
    (wrap-proc qs))
  (define wrapped-lines-without-hyphens (wrap-quads (assert (quad-list b) listof-quad?))) ; 100/150
  (define avg-looseness (average-looseness wrapped-lines-without-hyphens))
  (define gets-hyphenation? (and world:use-hyphenation?
                                 (fl> avg-looseness world:line-looseness-tolerance)))
  (define wrapped-lines (if gets-hyphenation?
                            (wrap-quads (split-quad (assert ((if world:allow-hyphenated-last-word-in-paragraph
                                                               hyphenate-quad
                                                               hyphenate-quad-except-last-word) (merge-adjacent-within b))
                                                            quad?)))
                            wrapped-lines-without-hyphens))
  (map insert-spacers-in-line
       (for/list ([line-idx (in-naturals)][the-line (in-list wrapped-lines)])
         (apply line (attr-change (quad-attrs the-line) (list 'line-idx line-idx 'lines (length wrapped-lines))) (group-quad-list the-line)))))


(define (number-pages ps)
  (for/list ([i (in-naturals)][p (in-list ps)])
    (apply page (merge-attrs (quad-attrs p) `(page ,i)) (group-quad-list p))))

(define (pages->doc ps)
  ;; todo: resolve xrefs and other last-minute tasks
  ;; todo: generalize computation of widths and heights, recursively
  (define (columns-mapper page-in)
    (apply page (quad-attrs page-in)
           (map add-vert-positions (for/list ([col (in-list (quad-list page-in))])
             (assert col column?)
             (apply column (quad-attrs col) (map (λ(ln) (assert ln line?) (compute-line-height (add-horiz-positions (fill ln)))) (group-quad-list col)))))))
  (define mapped-pages (map columns-mapper (number-pages ps)))
  (define doc (quads->doc mapped-pages))
  doc)


(define (lines->columns lines)
  (define prob (new problem% [solver #f]))
  (define max-column-lines world:default-lines-per-column)
  (define-values (columns ignored-return-value)
    (for/fold ([columns empty][lines-remaining lines])
              ([col-idx  (stop-before (in-naturals) (λ(x) (empty? lines-remaining)))])
      ;; domain constraint is best way to simplify csp, because it limits the search space.
      ;; search from largest possible value to smallest.
      ;; largest possible is the minimum of the max column lines, or
      ;; the number of lines left (modulo minimum page lines) ...
      (define viable-column-range
        (sequence->list (in-range (min max-column-lines (max
                                      (length lines-remaining)
                                      (- (length lines-remaining) world:minimum-lines-per-column)))
               ;; ... and the smallest possible is 1, or the current minimum lines.
               ;; (sub1 insures that range is inclusive of last value.)
               (sub1 (min 1 world:minimum-lines-per-column)) -1)))
      (send prob add-variable "column-lines" viable-column-range)
      ;; greediness constraint: leave enough lines for next page, or take all
      (define (greediness-constraint pl)
        (define leftover (- (length lines-remaining) pl))
        (or (= leftover 0) (>= leftover world:minimum-lines-per-column)))
      (send prob add-constraint greediness-constraint '("column-lines"))
      ;; last lines constraint: don't take page that will end with too few lines of last paragraph.
      (define (last-lines-constraint pl)
        (define last-line-of-page (list-ref lines-remaining (sub1 pl)))
        (define lines-in-this-paragraph (assert (quad-attr-ref last-line-of-page world:total-lines-key) index?))
        (define line-index-of-last-line (assert (quad-attr-ref last-line-of-page world:line-index-key) index?))
        (define (paragraph-too-short-to-meet-constraint?)
          (< lines-in-this-paragraph world:min-last-lines))
        (or (paragraph-too-short-to-meet-constraint?)
            (>= (add1 line-index-of-last-line) world:min-last-lines)))
      (send prob add-constraint last-lines-constraint '("column-lines"))
      ;; first lines constraint: don't take page that will leave too few lines at top of next page
      (define (first-lines-constraint pl lines-remaining)
        (define last-line-of-page (list-ref lines-remaining (sub1 pl)))
        (define lines-in-this-paragraph (assert (quad-attr-ref last-line-of-page world:total-lines-key) integer?))
        (define line-index-of-last-line (assert (quad-attr-ref last-line-of-page world:line-index-key) integer?))
        (define lines-that-will-remain (- lines-in-this-paragraph (add1 line-index-of-last-line)))
        (define (paragraph-too-short-to-meet-constraint?)
          (< lines-in-this-paragraph world:min-first-lines))
        (or (paragraph-too-short-to-meet-constraint?)
            (= 0 lines-that-will-remain) ; ok to use all lines ...
            (>= lines-that-will-remain world:min-first-lines))) ; but if any remain, must be minimum number.
      (send prob add-constraint (λ(x) (first-lines-constraint (assert x index?) lines-remaining)) '("column-lines"))
      (define s (send prob get-solution))
      (define how-many-lines-to-take (assert (hash-ref s "column-lines") index?))
      (define-values (lines-to-take lines-to-leave) (split-at lines-remaining how-many-lines-to-take))
      (send prob reset)
      (define new-column (quads->column lines-to-take))
      (values (cons (apply column (attr-change (quad-attrs new-column) (list world:column-index-key col-idx)) (group-quad-list new-column)) columns) lines-to-leave)))
  (reverse columns))


(define (columns->pages cols)
  (define columns-per-page (assert (quad-attr-ref (car cols) world:column-count-key (world:column-count-key-default)) exact-positive-integer?))
  (define column-gutter (assert (quad-attr-ref (car cols) world:column-gutter-key (world:column-gutter-key-default)) flonum?))
  ;; don't use default value here. If the col doesn't have a measure key,
  ;; it deserves to be an error, because that means the line was composed incorrectly.
  (when (not (quad-has-attr? (car cols) world:measure-key))
    (error 'columns->pages "column attrs contain no measure key: ~a ~a" (quad-attrs (car cols)) (quad-car (car cols))))
  (define column-width (assert (quad-attr-ref (car cols) world:measure-key) flonum?))
  (define width-of-printed-area (+ (* columns-per-page column-width) (* (sub1 columns-per-page) column-gutter)))
  (define result-pages
    (map (λ(cols) (quads->page cols))
                                       (for/list ([page-cols (in-list (slice-at cols columns-per-page))])
                                         (define-values (last-x cols)
                                           (for/fold ([current-x  (/ (- (world:paper-width-default) width-of-printed-area) 2.0)]
                                                      [cols empty])
                                                     ([col (in-list page-cols)][idx (in-naturals)])
                                             (values (foldl fl+ 0.0 (list current-x column-width column-gutter)) (cons (quad-attr-set* col (list 'x current-x 'y 40.0 world:column-index-key idx)) cols))))
                                         (reverse cols))))
  result-pages)

(define (block-quads->lines qs)
  (block->lines (quads->block qs)))

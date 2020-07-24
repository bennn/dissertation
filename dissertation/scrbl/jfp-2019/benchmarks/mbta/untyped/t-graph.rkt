#lang racket

;; implements the model for the T path finder 

(provide 
 ;; type MBTA% = 
 ;; (Class mbta% 
 ;;        [find-path (-> Station Station [Listof Path])] 
 ;;        [render (-> [Setof Station] String)
 ;;        [station?  (-> String Boolean)]
 ;;        [station   (-> String (U Station [Listof Station])])
 ;; type Path  = [Listof [List Station [Setof Line]]]
 ;; interpretation: take the specified lines to the next station from here 
 ;; type Station = String
 ;; type Line is one of: 
 ;; -- E
 ;; -- D 
 ;; -- C
 ;; -- B 
 ;; -- Mattapan
 ;; -- Braintree
 ;; -- orange
 ;; -- blue 
 ;; as Strings
 
 ;; ->* [instance-of MBTA%]
 ;; read the specification of the T map from file and construct an object that can
 ;; -- convert a string to a (list of) station(s) 
 ;; -- find a path from one station to another
 read-t-graph)

;; ===================================================================================================
(require "../base/untyped.rkt" "../base/my-graph.rkt")

;; type Lines       = [Listof [List String Connections]]
;; type Connections = [Listof Connection]
;; type Connection  = [List Station Station]

(define SOURCE-DIRECTORY "../base/~a.dat")
(define COLORS '("blue" "orange" "green" "red"))

;; ---------------------------------------------------------------------------------------------------
;; String -> [Maybe [Listof String]]
(define (line-specification? line)
  (define r (regexp-match #px"--* (.*)" line))
  (and r
       ;; the outer cast is for conversting strings to lines
       (map assert-line (string-split (assert (cadr r) string?)))))

(define assert-line
  (let ((line* (list "green" "E" "D" "C" "B" "red" "Mattapan" "Braintree" "orange" "blue")))
    (lambda (x)
      (or
        (for/or
                ((y (in-list line*))
                 #:when (string=? x y))
          y)
        (error 'assert-line)))))

#| ASSUMPTIONS about source files:

   A data file has the following format: 
   LineSpecification 
   [Station
    | 
    LineSpecification
    ]* 

   A LineSpecification consists of dashes followed by the name of lines, separated by blank spaces. 

   A Station is the string consisting of an entire line, minus surrounding blank spaces. 
|#

;; ---------------------------------------------------------------------------------------------------
(define (read-t-graph)
  (define-values (stations connections lines color->lines) (read-parse-organize))
  (define-values (graph connection-on) (generate-graph connections lines))
  (new mbta% [G graph][stations stations][bundles color->lines][connection-on connection-on]))

;; ---------------------------------------------------------------------------------------------------
(define (read-parse-organize)
  (for/fold
    ((s* '()) (c* '()) (lc '()) (cl '()))
    ((color COLORS))
    (define-values (new-s* new-lc) (read-t-line-from-file color))
    (define new-c* (map second new-lc))
    (define new-cl (list color (apply set (map first  new-lc))))
    (values (append new-s* s*) (apply append c* new-c*) (append new-lc lc) (cons new-cl cl))))

;; ---------------------------------------------------------------------------------------------------
(define (generate-graph connections lines)
  (define graph (unweighted-graph/directed connections))
  (define-values (connection-on _  connection-on-set!)
    (attach-edge-property graph #:init (set)))
  (for ((line (in-list lines)))
    (define name (first line))
    (for ((c (second line)))
      (define from (first c))
      (define to (second c))
      (connection-on-set! from to (set-add (connection-on from to) name))))
  (values graph connection-on))

;; ---------------------------------------------------------------------------------------------------
(struct partial-line (pred connections))

(define (read-t-line-from-file line-file)
  (define file*0 (file->lines (format SOURCE-DIRECTORY line-file)))
  ;; this re-ordering matches the type-oriented decomposition I used in the untyped world
  ;; -------------------------------------------------------------------------------------------------
  (define (lines->hash lines0)
    (define mt-partial-line '())
    (define first-station (second file*0))
    (define hlc0 
      (make-immutable-hash
       (for/list ([line lines0])
         (cons line (partial-line first-station mt-partial-line)))))
    (define-values (stations hlc) (process-file-body (cddr file*0) (list first-station) lines0 hlc0))
    (define line->connection*
      (for/list ([(line partial-line) (in-hash hlc)])
        (list line (partial-line-connections partial-line))))
    (values (reverse stations) line->connection*))
  ;; -------------------------------------------------------------------------------------------------
  (define (process-file-body file* stations lines hlc)
    (cond
      [(empty? file*) (values stations hlc)]
      [else 
       (define ?station (string-trim (first file*)))
       (cond
         [(line-specification? ?station) 
          => (lambda (lines) (process-file-body (rest file*) stations lines hlc))]
         [else ;; now we know ?station is a station
          (define new-hlc (add-station-to-lines hlc lines ?station))
          (process-file-body (rest file*) (cons ?station stations) lines new-hlc)])]))
  ;; -------------------------------------------------------------------------------------------------
  (define (add-station-to-lines hlc lines station)
    (for/fold ([hlc1 hlc]) ([line lines])
      (define the-partial-line (hash-ref hlc1 line (lambda () (error "KNOLWEDGE: impossible"))))
      (define predecessor (partial-line-pred the-partial-line))
      (define prefix (partial-line-connections the-partial-line))
      (define connections (list* (list predecessor station) (list station predecessor) prefix))
      (hash-set hlc1 line (partial-line station connections))))
  ;; -------------------------------------------------------------------------------------------------
  ;; IN: check file format (prefix), then process proper file content 
  (define lines0 (line-specification? (first file*0)))
  (unless lines0 (error "KNOWLEDGE: we know that the file is properly formatted"))
  (lines->hash lines0))

;; ---------------------------------------------------------------------------------------------------

(define mbta%
  (class object% 
    (init-field
     ;; Graph 
     G
     ;; [Listof Station]
     stations
     ;; [Station Station -> Line]
     connection-on 
     ;; [Listof [List String [Setof Line]]]
     bundles)

    (define stations-set (apply set stations))

    (super-new)

    (define/public (render b)
      (define r (memf (lambda (c) (subset? (second c) b)) bundles))
      (if r (first (first r)) (string-join (set-map b values) " ")))

    (define/public (station word)
      (define word# (regexp-quote word))
      (define candidates
        (for/list ([s stations-set] #:when (regexp-match word# s))
          s))
      (if (and (cons? candidates) (empty? (rest candidates)))
          (first candidates)
          candidates))

    (define/public (station? s)
      (set-member? stations-set s))

    (define/public (find-path from0 to)
      (define paths* (find-path/aux from0 to))
      (for/list ((path paths*))
        (define start (first path))
        (cond
          [(empty? (rest path)) (list (list start (set)))]
          [else             
           (define next (connection-on start (second path)))
           (define-values (_ result)
             (for/fold ([predecessor start][r (list (list start next))]) ((station (rest path)))
               (values station (cons (list station (connection-on station predecessor)) r))))
           (reverse result)])))

    ;; Node Node -> [Listof Path]
    (define/private (find-path/aux from0 to)
      (let search ([from from0][visited '()])
        (cond
          [(equal? from to) (list (list from))]
          [(member from visited) (list)]
          [else
           (define visited* (cons from visited))
           (for/fold ((all-paths '())) ([n (in-neighbors G from)])
             (define paths-from-from-to-to
               (map (lambda (p) (cons from p)) (search n visited*)))
             (append all-paths paths-from-from-to-to))])))))

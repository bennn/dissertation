#lang at-exp racket/base

;; Mostly copied from classicthesis/lang,
;;  except I wanted to give a latex-defaults+replacements
;;  ugh!

(require scribble/doclang
         (rename-in scribble/core [part sc:part])
         (except-in scribble/base table-of-contents)
         scribble/decode
         scribble/latex-prefix
         scribble/latex-properties
         racket/contract
         racket/list
         racket/match
         scribble/private/defaults
         setup/collects
         (for-syntax racket/base
                     syntax/parse))
(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin])

         chapter
         chapter-ref
         Chapter-ref
         part-ref
         Part-ref
         include-part

         (contract-out
          [part (->* []
                     [#:tag string?
                      #:preamble content?]
                     #:rest pre-content?
                     any)]))

;; define keywords for #lang options
(define-syntax-rule (define-keywords k ...)
  (begin (define-syntax k (syntax-rules ())) ...
         (provide k) ...))

(define-keywords drafting parts nochapters linedheaders eulerchapternumbers
                 beramono eulermath pdfspacing minionprospacing
                 tocaligned dottedtoc manychapters)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ doc:id . body)
     ;; parse options like scribble/sigplan and similar languages
     ;; this doesn't need to be a hash (can be a set), but a hash lets us
     ;; have keywords with arguments like font sizing in the future
     (define options (make-hash))
     (let loop ([contents #'body])
       (syntax-parse contents
         [(ws . body)
          ;; stolen from scribble/sigplan
          #:when (and (string? (syntax-e #'ws))
                      (regexp-match? #rx"^ *$" (syntax-e #'ws)))
          (loop #'body)]
         [((~and kw (~or (~literal drafting)
                         (~literal parts)
                         (~literal nochapters)
                         (~literal linedheaders)
                         (~literal eulerchapternumbers)
                         (~literal beramono)
                         (~literal eulermath)
                         (~literal pdfspacing)
                         (~literal minionprospacing)
                         (~literal tocaligned)
                         (~literal dottedtoc)
                         (~literal manychapters)))
           . body)
          (hash-set! options
                     (identifier-binding-symbol #'kw)
                     #t)
          (loop #'body)]
         [body
          #`(#%module-begin doc (post-process #,options) () . body)]))]))

;; from the example in the classicthesis package, parameterized
;; with the options provided on the #lang line
(define (document-class options)
  @string-append{
    \documentclass[ twoside,open=right,titlepage,numbers=noenddot,headinclude,%1headlines,% letterpaper a4paper
                    footinclude=true,cleardoublepage=empty,abstract=off,
                    BCOR=5mm,paper=a4,fontsize=11pt,%11pt,a4paper,%
                    ngerman,american,%
                    @(apply string-append
                            (add-between (map symbol->string (hash-keys options))
                                         ","))]{scrreprt}
    %% from http://tex.stackexchange.com/a/39418
    \makeatletter
    \newcommand{\dontusepackage}[2][]{%
      \@"@"namedef{ver@"@"#2.sty}{9999/12/31}%
      \@"@"namedef{opt@"@"#2.sty}{#1}}
    \makeatother

    %% Disable Scribble's default TOC configuration
    \dontusepackage{tocstyle}
    \newcommand{\usetocstyle}[1]{\relax}
  })

(define ((post-process options) doc)
  (add-defaults doc
                (string->bytes/utf-8 (document-class options))
                (collection-file-path "style.tex" "greenman-thesis" "classicthesis")
                (list (collection-file-path "classicthesis.sty"
                                            "greenman-thesis"))
                #f
                #:replacements (hash "scribble-load-replace.tex"
                                     (collection-file-path "scribble-load-replace.tex"
                                                           "greenman-thesis"))))

;; thesis command wrappers
;; taken from John Rafkind's uuthesis wrapper
(define-syntax-rule (define-wrappers (name style) ...)
  (begin
    (define (name . str)
      (make-element (make-style style '()) (decode-content str)))
    ...
    (provide name ...)))
(define-syntax-rule (define-pre-title-wrappers (name style) ...)
  (begin
    (define (name . str)
      (make-paragraph
       (make-style 'pretitle '())
       (make-multiarg-element
        (make-style style '())
        (decode-content str))))
    ...
    (provide name ...)))

(define-syntax-rule (define-section-like name style)
  (begin
    (define (name #:tag [tag (symbol->string (gensym))] . str)
      (make-multiarg-element (make-style style '())
                             (list (decode-content (list tag))
                                   (decode-content str))))
    (provide name)))

(define-pre-title-wrappers
  (degree "Sdegree")
  (department "Sdepartment")
  (university "Suniversity")
  (location "Slocation")
  (submit-date "Ssubmitdate"))

(define-wrappers
  (approval "Sapproval")
  (abstract "Sabstract")
  (acknowledgements "Sacknowledgements")
  (table-of-contents "Stableofcontents")
  (end-front-matter "Sendfrontmatter")
  (graffito "graffito"))

;; Scribble handles top-level sections specially as chapters anyway, so take
;; advantage of that to do chapter references. See style.tex
(define chapter-ref secref)
(define Chapter-ref Secref)
(define part-ref secref)
(define Part-ref Secref)

(define (part #:tag [tag (symbol->string (gensym))]
              #:preamble [pre #f]
              . str)
  (define sec
    (apply section #:tag tag #:style (make-style #f '(grouper)) str))
  (if pre
      ;; The ctparttext has to come *before* the actual part command. Also it
      ;; has to be in its own part so that it's not ignored by the decode pass.
      (list (make-part
             #f null #f (make-style #f null) null
             (list (make-paragraph
                    (make-style #f null)
                    (make-element (make-style "ctparttext" null) pre)))
             null)
            sec)
      sec))

(define chapter section)

(define-syntax-rule (define-includer name style)
  (begin
    (define-syntax (name stx)
      (syntax-case stx ()
        [(_ module)
         (let ()
           (define name* (gensym 'name))
           #'(begin
               (require (rename-in module [doc name*]))
               (make-nested-flow (make-style style '(command))
                                 (part-blocks name*))))]))
    (provide name)))

(define-includer include-abstract "Sabstract")
(define-includer include-acknowledgements "Sacknowledgements")

;; A variant of include-section that adds a grouper style to make it a part
(define-syntax (include-part stx)
  (syntax-parse stx
    [(_ mod)
     (define doc-from-mod (datum->syntax #'mod 'doc))
     (unless (module-path? (syntax->datum #'mod))
       (raise-syntax-error 'include-chapter
                           "not a module path"
                           stx
                           #'mod))
     #`(begin
         (require (only-in mod [#,doc-from-mod doc]))
         (to-part doc))]))

;; Helper for above macro
(define (to-part doc)
  (define old-style (part-style doc))
  (match-define (sc:part tp tags tc style collect blocks part) doc)
  (sc:part tp tags tc
           (make-style (style-name style)
                       (cons 'grouper (style-properties style)))
           collect blocks part))

;; TODO possible additions (supported by classicthesis)
;;  - subtitles
;;  - dedications
;;  - list of figures, tables, etc. (may require same trickery as for sections to get labels to agree)


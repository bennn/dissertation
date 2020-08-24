#lang typed/racket

(require (prefix-in topq: "pfds-real-time.rkt"))
(require (prefix-in disjq: "pfds-real-time.rkt"))
;(require (rename-in (only-in "mk.rkt" var reify == symbolo numbero absento =/=)
;                    [== ==-goal]
;                    [symbolo symbolo-goal]
;                    [numbero numbero-goal]
;                    [absento absento-goal]
;                    [=/= =/=-goal]))
;
;(provide == run run* fresh conde symbolo numbero absento =/=)


(define-type Constraint-Store
  (List (Listof Any)
        (Listof Any)
        (Listof Any)
        (Listof Any)
        (Listof Any)
        (Listof Any)
        (Listof Any)))

(define-type Disj*
  (Listof disj))

(define-type Constraint
  (U conj constraint))

(define-type Unsorted
  (Listof Constraint-Like))

(define-type Constraint-Like
  (U Constraint (-> TODO)))

(define-type TODO Any)

; Constraints

(: empty-constraint-store : (-> Constraint-Store))
(define (empty-constraint-store)
  '(() () () () () () ()))

;(struct unification ([term1 : TODO]
;                     [term2 : TODO])
;        #:transparent)
;
;(define-syntax-rule (wrap-constraints [(name [args : tys] ...) goal] ...)
;  (begin
;    (define (name [args : tys] ...) : Constraint
;      (constraint (goal args ...)))
;    ...))
;
;(wrap-constraints
;  [(== [term1 : TODO] [term2 : TODO]) ==-goal]
;  [(symbolo [u : TODO]) symbolo-goal]
;  [(numbero [u : TODO]) numbero-goal]
;  [(absento [u : TODO] [v : TODO]) absento-goal]
;  [(=/= [u : TODO] [v : TODO]) =/=-goal])

(: apply-constraint (-> (-> Constraint-Store (U #f Constraint-Store))
                        Constraint-Store
                        (U #f Constraint-Store)))
(define (apply-constraint constraint constraint-store)
  (constraint constraint-store))

;; Search

(struct conj ([constraint-store : (U #f Constraint-Store)]
              [unsorted : Unsorted]
              [disjunctions : (U #f Disj*)])
        #:transparent)
(struct disj ([children : (Listof Constraint)])
        #:transparent)
(struct constraint ([c : TODO])
        #:transparent)
(struct state ([unreduced : TODO]
               [reduced : TODO])
        #:transparent)

;(: reduce-conj (-> Constraint-Store
;                   Unsorted
;                   Disj*
;                   (Values (U #f Constraint-Store) (U #f Disj*))
;                   ))
;(define (reduce-conj constraint-store unsorted disjunctions)
;  (if (null? unsorted)
;    (values constraint-store disjunctions)
;    (match (first unsorted)
;      [(constraint c)
;       (let ([new-constraint-store (apply-constraint c constraint-store)])
;         (cond
;           [(not new-constraint-store)
;            (values #f #f)]
;           [(null? (rest unsorted))
;            (values new-constraint-store disjunctions)]
;           [else (reduce-conj new-constraint-store (rest unsorted) disjunctions)]))]
;      [(conj #f nested-unsorted #f)
;       (let-values ([(new-constraint-store new-disjunctions)
;                     (reduce-conj constraint-store nested-unsorted disjunctions)])
;         (if new-constraint-store
;           (reduce-conj new-constraint-store (rest unsorted) new-disjunctions)
;           (values #f #f)))]
;      ; Only disjunctions are inverse-eta delayed, so evaluate the procedure
;      ; and enqueue the resulting disjunction.
;      [(and p (? procedure?))
;       (reduce-conj
;         constraint-store
;         (rest unsorted)
;         (disjq:enqueue (p) disjunctions))]
;      )))

(struct filter-res ([constraint-store : Constraint-Store]
                    [new-disjunctions : Disj*]
                    [original-term : TODO]))

;(: filter-branches (-> disj Constraint-Store Disj* (Listof TODO)))
;(define (filter-branches disjunction constraint-store disjunctions)
;  (filter-map
;    (match-lambda
;      [(conj #f children #f)
;       (define-values (new-constraint-store new-disjunctions)
;         (reduce-conj constraint-store children disjunctions))
;       (and new-constraint-store
;            (filter-res new-constraint-store new-disjunctions (conj #f children #f)))])
;    (disj-children disjunction)))

;(: reduce-filter (-> Constraint-Store
;                     Unsorted
;                     Disj*
;                     (Values (U #f Constraint-Store) (U #f Disj*))))
;(define (reduce-filter constraint-store unsorted disjunctions)
;  (define-values (reduced-constraint-store reduced-disjunctions)
;    (reduce-conj constraint-store unsorted disjunctions))
;  (if reduced-disjunctions
;   (let loop : (Values (U #f Constraint-Store) (U #f Disj*))
;     ([remaining : Disj* reduced-disjunctions]
;      [processed : (disjq:Queue disj) ((inst disjq:queue disj))]
;      [constraint-store : (U #f Constraint-Store) reduced-constraint-store])
;     (if (disjq:empty? remaining)
;       (begin
;         ;(displayln (map (lambda (i) (length (disj-children i))) (disjq:queue->list processed)))
;         (values constraint-store processed))
;       (let ([filtered (filter-branches (disjq:head remaining) constraint-store processed)])
;         (cond
;           [(null? filtered) (values #f #f)]
;           [(null? (rest filtered))
;            (loop (disjq:tail remaining)
;                  (filter-res-new-disjunctions (first filtered))
;                  (filter-res-constraint-store (first filtered)))]
;           [else
;             (loop (disjq:tail remaining)
;                   (disjq:enqueue
;                     (disj (map (lambda ([i : TODO]) (filter-res-original-term i)) filtered)) processed)
;                   constraint-store)]))))
;   (values reduced-constraint-store reduced-disjunctions)))

(: distribute (-> disj Disj* Constraint-Store (Listof Constraint)))
(define (distribute into-disjunction other-disjunctions constraint-store)
  (map
    (match-lambda
      [(conj #f nested-unsorted #f)
       (conj constraint-store nested-unsorted other-disjunctions)])
    (disj-children into-disjunction)))

;(define (step-state current-state)
;  (match-define (state unreduced reduced) current-state)
;  (if (pair? unreduced)
;    (match-let ([(conj constraint-store unsorted disjunctions)
;                 (first unreduced)])
;      (define-values (new-constraint-store filtered-disjunctions)
;        (reduce-filter constraint-store unsorted disjunctions))
;      (cond
;        [(not filtered-disjunctions)
;         (values #f (state (rest unreduced) reduced))]
;        [(disjq:empty? filtered-disjunctions)
;         (values new-constraint-store (state (rest unreduced) reduced))]
;        [else
;         (values #f
;                 (state (rest unreduced)
;                        (topq:enqueue (conj new-constraint-store '() filtered-disjunctions)
;                                      reduced)))]))
;    (match-let ([(conj constraint-store '() disjunctions)
;                 (topq:head reduced)])
;      (values #f (state
;                   (distribute (disjq:head disjunctions)
;                               (disjq:tail disjunctions)
;                               constraint-store)
;                   (topq:tail reduced))))))

(: inject (-> conj * TODO))
(define (inject . conjuncts)
  (state (list (conj (empty-constraint-store) conjuncts (disjq:queue))) (topq:queue)))

;(define (run-internal n query-vars current-state acc)
;  (if (and (or (pair? (state-unreduced current-state))
;               (not (topq:empty? (state-reduced current-state))))
;           (or (false? n) (> n 0)))
;    (let-values ([(result new-state) (step-state current-state)])
;      (if result
;        (run-internal (and n (- n 1)) query-vars new-state (cons result acc))
;        (run-internal n query-vars new-state acc)))
;    (map (lambda (result)
;           ((reify (if (= (length query-vars) 1)
;                     (car query-vars)
;                     query-vars))
;            result))
;         (reverse acc))))
;
;
;; Interface macros
;
;(define-syntax (fresh stx)
;  (syntax-case stx ()
;    [(_ (x* ...) g g* ...)
;     #'(let ([x* (var (quote x*))] ...)
;         (conj #f (list g g* ...) #f))]))
;
;(define-syntax (conde stx)
;  (syntax-case stx ()
;    [(_ [g1 g1* ...] [g g* ...] ...)
;     #'(lambda ()
;         (disj
;           (list
;             (conj #f (list g1 g1* ...) #f)
;             (conj #f (list g g* ...) #f)
;             ...)))]))
;
;
;
;(define-syntax (run stx)
;  (syntax-case stx ()
;    [(_ ne (x x* ...) g g* ...)
;     #'(let ([n ne]
;             [x (var (quote x))]
;             [x* (var (quote x*))] ...)
;         (run-internal n
;                       (list x x* ...)
;                       (inject g g* ...)
;                       '()))]))
;
;(define-syntax (run* stx)
;  (syntax-case stx ()
;    [(_ (x x* ...) g g* ...)
;     #'(run #f (x x* ...) g g* ...)]))
;
;
;; Debugging tools
;
;(define (state-display s)
;  (list (state-unreduced s) (topq:queue->list (state-reduced s))))
;
;(define (show-step-internal n q [r '()])
;  (if (> n 0)
;    (let-values ([(result new-tree) (step-state q)])
;      (show-step-internal (- n 1) new-tree (cons result r)))
;    (values r (state-display q))))
;
;(define (show-step n tree)
;  (show-step-internal n (inject tree)))
;
;

#lang racket

;;; By Anthony Carrico <acarrico@memebeam.org>
;;;
;;; scc = strongly connected component.
;;;
;;; http://en.wikipedia.org/wiki/Path-based_strong_component_algorithm

(module+ test (require rackunit))

(require racket/generic)
(require racket/match)

(provide gen:sccable scc initial-scc-state)

;;; Interface:

(define-generics sccable
  (sccable-index sccable)
  (set-sccable-index! sccable index)
  (sccable-fold-edges fn state sccable)
  (sccable-component sccable)
  (set-sccable-component! sccable component))

(define (scc node (state (initial-scc-state)))
  (dfs node state))

(define (initial-scc-state) (scca-state 0 '() '()))

(struct scca-state (index ; seen-so-far-index, aka preorder number
                    S ; not-yet-assigned-to-an-scc
                    P ; not-yet-determined-to-belong-to-different-sccs
                    ))

;;; Algorithm:

;; should probably move the seen? test up to the entry point of the
;; recursion.

(define (dfs node state)
  ;; dfs = depth first search part of the algorithm.
  (maybe-pop-component
   node
   (check-edges
    node
    (assign-index-and-push-stacks
     node
     state))))

(define/match (assign-index-and-push-stacks node state)
  ((node (struct* scca-state ((index index) (S S) (P P))))
   (set-sccable-index! node index)
   (scca-state (+ 1 index) (cons node S) (cons node P))))

(define (check-edges node state)
  (sccable-fold-edges
   (lambda (node state)
     (cond ((not (sccable-index node))
            (dfs node state))
           ((not (sccable-component node))
            (let ((i (sccable-index node)))
              (struct-copy
               scca-state
               state
               (P (dropf (scca-state-P state) (lambda (n) (> (sccable-index n) i)))))))
           (else
            state)))
   state
   node))

(define (maybe-pop-component node state)
  (match state
    ((struct* scca-state ((S S) (P P)))
     (if (eq? node (car P))
         (let-values (((component S) (pop-component node S)))
           (for-each
            (lambda (node) (set-sccable-component! node component))
            component)
           (struct-copy scca-state state (S S) (P (cdr P))))
         state))))

(define (pop-component node stack)
  (let loop ((component '())
             (stack stack))
    (let* ((n (car stack))
           (component (cons n component))
           (stack (cdr stack)))
      (if (eq? n node)
          (values component stack)
          (loop component stack)))))

(module+ test
  (require racket/set)
  (require (only-in srfi/43 vector-fold))

  (struct node (label edges index component)
          #:mutable
          #:transparent
          #:methods gen:sccable
          ((define (scca-label sccable) (node-label sccable))
           (define (sccable-index sccable)
             (node-index sccable))
           (define (set-sccable-index! sccable index)
             (set-node-index! sccable index))
           (define (sccable-fold-edges fn state sccable)
             (vector-fold (lambda (i state element) (fn element state))
                          state
                          (node-edges sccable)))
           (define (sccable-component sccable)
             (node-component sccable))
           (define (set-sccable-component! sccable component)
             (set-node-component! sccable component))))

  (shared ((A (node 'A (vector B F) #f #f))
           (B (node 'B (vector D) #f #f))
           (C (node 'C (vector G) #f #f))
           (D (node 'D (vector A G) #f #f))
           (E (node 'E (vector F C) #f #f))
           (F (node 'F (vector E) #f #f))
           (G (node 'G (vector H) #f #f))
           (H (node 'H (vector C I) #f #f))
           (I (node 'I (vector) #f #f)))

          ;; compute the components:
          (scc A)

          ;; print the components:
          (for-each
           (lambda (node)
             (printf "node: ~s component: ~s\n"
                     (node-label node)
                     (map node-label (sccable-component node))))
           (list A B C D E F G H I))

          ;; run the tests:
          (check set=? (seteq A B D) (list->seteq (sccable-component A)))
          (check set=? (seteq A B D) (list->seteq (sccable-component B)))
          (check set=? (seteq G H C) (list->seteq (sccable-component C)))
          (check set=? (seteq A B D) (list->seteq (sccable-component D)))
          (check set=? (seteq E F) (list->seteq (sccable-component E)))
          (check set=? (seteq E F) (list->seteq (sccable-component F)))
          (check set=? (seteq G H C) (list->seteq (sccable-component G)))
          (check set=? (seteq G H C) (list->seteq (sccable-component H)))
          (check set=? (seteq I) (list->seteq (sccable-component I)))
          )
  )

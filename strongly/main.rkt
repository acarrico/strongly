#lang racket

;;; By Anthony Carrico <acarrico@memebeam.org>
;;;
;;; scc = strongly connected component.
;;;
;;; http://en.wikipedia.org/wiki/Path-based_strong_component_algorithm

(module+ test (require rackunit))

(require racket/generic)
(require racket/match)

(provide gen:sccable scc initial-scc-state scc-state-component-table)

;;; Interface:

(define-generics sccable
  (sccable-fold-edges fn state sccable))

(define (scc node (state (initial-scc-state)))
  (dfs node state))

(struct scc-state
  (index ; seen-so-far-index, aka preorder number
   index-table ; node->index
   S ; not-yet-assigned-to-an-scc
   P ; not-yet-determined-to-belong-to-different-sccs
   component-table ; node->component
   ))

(define (initial-scc-state)
  (scc-state 0 (hasheq) '() '() (hasheq)))

(define (sccable-component sccable state)
  (hash-ref (scc-state-component-table state) sccable (lambda () #f)))

(define (sccable-index sccable state)
  (hash-ref (scc-state-index-table state) sccable (lambda () #f)))

;;; Algorithm:

(define (dfs node state)
  ;; dfs = depth first search part of the algorithm.
  (cond ((not (sccable-index node state))
         ;; first time we see a node:
         (maybe-pop-component
          node
          (sccable-fold-edges
           dfs
           (assign-index-and-push-stacks node state)
           node)))
        ((not (sccable-component node state))
         ;; strongly connected:
         (struct-copy
          scc-state
          state
          (P (let ((i (sccable-index node state)))
               (dropf (scc-state-P state)
                      (lambda (n) (> (sccable-index n state) i)))))))
        (else
         ;; this part of the graph is already done:
         state)))

(define (assign-index-and-push-stacks node state)
  (match state
    ((struct* scc-state ((index i) (index-table index-table) (S S) (P P)))
     (struct-copy
      scc-state
      state
      (index (+ i 1))
      (index-table (hash-set index-table node i))
      (S (cons node S))
      (P (cons node P))))))

(define (maybe-pop-component node state)
  (match state
    ((struct* scc-state ((S S) (P P) (component-table component-table)))
     (if (eq? node (car P))
         (let-values (((component S) (pop-component node S)))
           (struct-copy
            scc-state state
            (S S)
            (P (cdr P))
            (component-table
             (foldl
              (lambda (node component-table)
                (hash-set component-table node component))
              component-table
              component))))
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

  ;; define an sccable graph node:
  (struct node (label edges)
          #:mutable
          #:transparent
          #:methods gen:sccable
          ((define (sccable-fold-edges fn state sccable)
             (vector-fold (lambda (i state element) (fn element state))
                          state
                          (node-edges sccable)))))

  ;; define the test graph:
  (define-values (A B C D E F G H I)
    (shared ((A (node 'A (vector B F)))
             (B (node 'B (vector D)))
             (C (node 'C (vector G)))
             (D (node 'D (vector A G)))
             (E (node 'E (vector F C)))
             (F (node 'F (vector E)))
             (G (node 'G (vector H)))
             (H (node 'H (vector C I)))
             (I (node 'I (vector))))
      (values A B C D E F G H I)))

  ;; compute the components:
  (define component-table (scc-state-component-table (scc A)))

  ;; print the components:
  (for-each
   (lambda (node)
     (printf "node: ~s component: ~s\n"
             (node-label node)
             (map node-label (hash-ref component-table node))))
   (list A B C D E F G H I))

  ;; run the tests:
  (check set=? (seteq A B D) (list->seteq (hash-ref component-table A)))
  (check set=? (seteq A B D) (list->seteq (hash-ref component-table B)))
  (check set=? (seteq G H C) (list->seteq (hash-ref component-table C)))
  (check set=? (seteq A B D) (list->seteq (hash-ref component-table D)))
  (check set=? (seteq E F) (list->seteq (hash-ref component-table E)))
  (check set=? (seteq E F) (list->seteq (hash-ref component-table F)))
  (check set=? (seteq G H C) (list->seteq (hash-ref component-table G)))
  (check set=? (seteq G H C) (list->seteq (hash-ref component-table H)))
  (check set=? (seteq I) (list->seteq (hash-ref component-table I)))
)

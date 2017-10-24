(define (btree)
  (define node 0)
  (define (setnode x) (set! node x))
  (define lchild 0)
  (define (setl x) (set! lchild x))
  (define rchild 0)
  (define (setr x) (set! rchild x))
  (lambda (selector . arg)
    (case selector
      ((getnode) node)
      ((setnode) (apply setnode arg))
      ((getl) lchild)
      ((setl) (apply setl arg))
      ((getr) rchild)
      ((setr) (apply setr arg))
      (else (display "Can't understand!")))))
(define p (btree))
(define (bst-insert tree node)
  (cond
    ((number? tree) 
     (begin 
       (set! tree (btree)) 
       (tree 'setnode node)))
    ((= 0 (tree 'getnode)) (tree 'setnode node))
    ((> node (tree 'getnode)) 
     (begin 
       (if (not (procedure? (tree 'getr)))
		(tree 'setr (btree))) 
       (bst-insert (tree 'getr) node)))
    ((< node (tree 'getnode)) 
     (begin
       (if (not (procedure? (tree 'getl)))
		(tree 'setl (btree)))
       (bst-insert (tree 'getl) node)))
    ((= node (tree 'getnode)) (display "Duplicated"))
    (else (display "Error!"))))
(define (bst-show tree)
    (if (procedure? tree) 
      (begin 
	(display (tree 'getnode))
        (newline)
        (bst-show (tree 'getl))
        (bst-show (tree 'getr))))
      '())

(define zero '(diff (one) (one)))

(define successor
  (lambda (diff-tree)
    `(diff ,diff-tree
	   (diff (diff (one) (one)) (one)))))

(define predecessor
  (lambda (diff-tree)
    `(diff ,diff-tree (one))))

(define diff-eval
  (lambda (diff-tree)
    (if (eqv? (car diff-tree) 'one)
	1
	(- (diff-eval (cadr diff-tree)) (diff-eval (caddr diff-tree))))))

(define is-zero?
  (lambda (diff-tree)
    (zero? (diff-eval diff-tree))))

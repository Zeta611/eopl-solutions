(define number->bintree
  (lambda (n)
    `(root (,n () ()) ())))

(define current-element
  (lambda (bintree) (caadr bintree)))

(define left-son
  (lambda (bintree) (cadadr bintree)))

(define right-son
  (lambda (bintree) (caddr (cadr bintree))))

(define up-trace
  (lambda (bintree) (caddr bintree)))

(define move-to-left-son
  (lambda (bintree)
    (list 'left
	  (left-son bintree)
	  (list (car bintree)
		(list (current-element bintree)
		      (up-trace bintree)
		      (right-son bintree))))))

(define move-to-right-son
  (lambda (bintree)
    (list 'right
	  (right-son bintree)
	  (list (car bintree)
		(list (current-element bintree)
		      (left-son bintree)
		      (up-trace bintree))))))

(define at-leaf?
  (lambda (bintree) (null? (cadr bintree))))

(define is-root?
  (lambda (bintree)
    (eqv? (car bintree) 'root)))

(define is-left-son?
  (lambda (bintree)
    (eqv? (car bintree) 'left)))

(define is-right-son?
  (lambda (bintree)
    (eqv? (car bintree) 'right)))

(define insert-to-left
  (lambda (n bintree)
    (list (car bintree)
	  (list (current-element bintree)
		(list n (left-son bintree) '())
		(right-son bintree))
	  (caddr bintree))))

(define insert-to-right
  (lambda (n bintree)
    (list (car bintree)
	  (list (current-element bintree)
		(left-son bintree)
		(list n '() (right-son bintree)))
	  (caddr bintree))))

(define move-up
  (lambda (bintree)
    (let ((up (up-trace bintree))
	  (this (cadr bintree)))
      (cond ((is-left-son? bintree)
	     (list (car up)
		   (list (caadr up)
			 this
			 (caddr (cadr up)))
		   (cadr (cadr up))))
	    ((is-right-son? bintree)
	     (list (car up)
		   (list (caadr up)
			 (cadr (cadr up))
			 this)
		   (caddr (cadr up))))
	    (else (error "Cannot move up"))))))

(define at-root?
  (lambda (bintree)
    (eqv? (car bintree) 'root)))

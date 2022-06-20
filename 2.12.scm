					; The notion of 'constructors' and 'observers' is quite vague.
					; We shall use the term 'constructor' when it can be transformed of the form `lambda (msg . params) (dispatch using msg)`.
					; empty-stack, push, pop, top, empty-stack?
(define empty-stack
  (lambda ()
    (lambda (msg)
      (cond
       ((eqv? msg 'top) (error "Empty stack: cannot top"))
       ((eqv? msg 'empty-stack?) #t)
       ((eqv? msg 'pop) (error "Empty stack: cannot pop"))
       (else (error "Unknown msg"))))))

(define push
  (lambda (stack val)
    (lambda (msg)
      (cond
       ((eqv? msg 'top) val)
       ((eqv? msg 'empty-stack?) #f)
       ((eqv? msg 'pop) stack)
       (else (error "Unknown msg"))))))

(define pop
  (lambda (stack) (stack 'pop)))

(define top
  (lambda (stack) (stack 'top)))

(define empty-stack?
  (lambda (stack) (stack 'empty-stack?)))
					; probably the hardest problem in this chapter!

; Using notions from the following section, empty-stack? is a predicate and pop and top are extractors.

;; Defining some basic types : an atomic sentence, and what it is built of

(defstruct (compound
	     (:conc-name nil)
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "~A~A"
			(compound-op struct)
			(compound-args struct)))))
	     op args)

(defun unify (x y +theta+)
  "returns a substitution to make x and y identical"
  (print "====UNIFY====")(print x)(print y)(print +theta+)
  (cond ((eq +theta+ 'failure) 'failure)
	((eql x y) +theta+)
	((var? x) (unify-var x y +theta+))
	((var? y) (unify-var y x +theta+))
	((and (compound-p x) (compound-p y))
	 (unify (args x) (args y)
		(unify (op x) (op y) +theta+)))
	((and (listp x) (listp y))
	 (unify (cdr x) (cdr y)
		(unify (car x) (car y) +theta+)))
	(t 'failure)))

(defun unify-var (var x +theta+)
  "returns a substitution"
  (print "====VARR====")(print var)(print x)(print +theta+)
; need some association code here
; we're going to have a data structure : list with two elements
; ( variable value ) that represents {var/val}
  (cond ((assoc var +theta+) (unify (cadr (assoc var +theta+)) x +theta+))
	((assoc x +theta+) (unify var (cadr (assoc x +theta+)) +theta+))
	(t (cons (list var x) +theta+))))

(defun var? (a)
    (when (symbolp a) (eq (char (string a) 0) #\?)))
;; Defining some basic types : an atomic sentence, and what it is built of

(defstruct (compound
	     ;(:conc-name nil)
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "~A~A"
			(compound-op struct)
			(compound-args struct)))))
	     op args)

(defun unify (x y +theta+)
  (cond ((eq +theta+ 'failure) 'failure)
	((eql x y) +theta+)
	((var? x) (unify-var x y +theta+))
	((var? y) (unify-var y x +theta+))
	((and (compound-p x) (compound-p y))
	 (unify (compound-args x) (compound-args y)
		(unify (compound-op x) (compound-op y) +theta+)))
	((and (listp x) (listp y))
	 (unify (cdr x) (cdr y)
		(unify (car x) (car y) +theta+)))
	(t 'failure)))

(defun unify-var (var x +theta+)
  (cond ((assoc var +theta+) (unify (cadr (assoc var +theta+)) x +theta+))
	((assoc x +theta+) (unify var (cadr (assoc x +theta+)) +theta+))
	(t (cons (list var x) +theta+))))

(defun var? (a)
    (when (symbolp a) (eq (char (string a) 0) #\?)))

;(defun substite (clause +theta+)
;  "substitutes variables in the clause with the substitution"
; (let ((sub (cadr (assoc clause +theta+))))
;   (cond ((compound-p clause) ; Compound? Keep the name, go inside to substitute.
;	   (make-compound :op (compound-op clause)
;			  :args (substite (compound-args clause) +theta+)))
;	  ((var? clause) (cond ((and sub(compound-p sub))
;				(substite sub +theta+))
;			       (sub sub)
;			       (t clause)))
;	  ((listp clause) (cons (substite (car clause) +theta+) (substite (cdr clause) +theta+)))
;	  (t clause))))

(defun subs (clause +theta+)
  (print "=====SUBS=====")(print clause)(print +theta+)
  (cond ((null clause) nil)
	((compound-p clause)
	 (make-compound :op (compound-op clause)
			:args (subs (compound-args clause) +theta+)))
	((var? clause)
	 (let ((sub (cadr (assoc clause +theta+))))
	   (cond ((and sub (compound-p sub))
		  (subs sub +theta+))
		 (sub sub)
		 (t clause))))
	((listp clause)(cons (subs (car clause) +theta+) (subs (cdr clause) +theta+)))
	(t clause)))
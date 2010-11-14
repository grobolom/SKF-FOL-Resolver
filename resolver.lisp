;; Defining some basic types : an atomic sentence, and what it is built of

(defstruct (compound
	     ;(:conc-name nil)
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "~A~A"
			(compound-op struct)
			(compound-args struct)))))
	     op &rest args)

(defun m-c (a &rest b)
  (make-compound :op a :args b))

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

(defun subs (clause +theta+)
;  (print "=====SUBS=====")(print clause)(print +theta+)
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

(defun sd-apart (KB)
  (loop
     for stmt in KB
     for i from 1
     collect (var-replace stmt i)))

(defun var-replace (stmt i)
  (cond ((compound-p stmt)
	 (make-compound :op (compound-op stmt) :args (var-replace (compound-args stmt) i)))
	((null stmt) nil)
	((list1p stmt)
	 (cons (var-replace (car stmt) i) nil))
	((listp stmt)
	 (cons (var-replace (car stmt) i) (var-replace (cdr stmt) i)))
	((var? stmt)
	 (intern (concatenate 'string (string stmt)(write-to-string i))))
	(t stmt)))

; (setf A (list (m-c 'Animal (m-c 'F '?x)) (m-c 'Loves (m-c 'G '?x) '?x)))
; (setf B (list (m-c 'Loves '?x (m-c 'F '?x)) (m-c 'Loves (m-c 'G '?x) '?x)))
; (setf C (list (m-c 'Loves '?y '?x) (m-c 'Animal '?z) (m-c 'Kills '?x '?z)))
; (setf D (list (m-c 'Animal '?x) (m-c 'Loves 'Jack '?x)))
; (setf E (list (m-c 'Kills 'Jack 'Tuna) (m-c 'Kills 'Curiosity 'Tuna)))
; (setf F (list (m-c 'Cat 'Tuna)))
; (setf G (list (m-c 'Cat '?x) (m-c 'Animal '?x)))

; (setf A (list '(.Animal (F (?x))) '(.Loves(.G(?x) ?x))))
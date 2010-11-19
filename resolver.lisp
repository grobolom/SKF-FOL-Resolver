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

(defun atp (kb a)
  (let ((skb (sd-apart kb)))
    (reslv skb (list a))))

(defun reslv (nres pres)
  (let ((node (car pres))
	(nextr (get-next-res (car pres) (append nres pres))))
    (format t "~%~%~A~%~A" node nextr)
    (cond ((not (null nextr))
	   (reslv (sort (append nres pres) #'(lambda (x y) (< (list-length x) (list-length y))))
		  (list (r-s node nextr))))
	  ((null node) 'DONE)
	  (t
	   (reslv (cdr (append nres pres))
		  (car nres))))))

(defun get-next-res (node fringe)
  (loop for e in fringe
     do (unless (equal (unify-facts e node) 'failure)(return e))))

(defun r-s (x y)
  (subs (r+s x y)(unify-facts x y)))
	 
(defun r+s (x y)
  (loop for px in x append
       (uni-nf px y)
     into unified
     finally (return (uni-rem unified (append x y)))))

(defun uni-rem (unified lst)
  (loop for l in lst append
       (if (find l unified) nil (list l))))

(defun uni-nf (n f)
  (loop for pf in f
     do (when (not (equal (unify-clauses n pf) 'failure))
	  (return (append (list n)(list pf))))))

(defun unify-facts (x y)
  (let ((u (unify-f x y)))
    (if (null u) 'failure u)))

(defun unify-f (x y)
  "returns all substitutions for two full facts"
  (loop for px in x append
       (loop for py in y append
	    (cond ((eql (unify-clauses px py) 'failure) 'nil)
		  ((eql (unify-clauses px py) 'nil) (list '(+N+)))
		  (t (unify-clauses px py))))
     into app
     finally (return (remove-dupes app))))

(defun remove-dupes (x)
  (loop for px in x append
       (cond ((null px) nil)
	     ((eql px 'failure)
	      (if (find px app) nil 'failure))
	     ((assoc (car px) app) nil)
	     (t (list px)))
     into app
     finally (return app)))

(defun unify-clauses (x y)
  (let ((opx (string (compound-op x)))
	(opy (string (compound-op y)))
	(opcx (char (string (compound-op x)) 0))
	(opcy (char (string (compound-op y)) 0)))
    (cond ((and (eql opcx '#\!)(equal (subseq opx 1) opy))
	   (unify (make-compound :op (intern (subseq opx 1)) :args (compound-args x)) y))
	  ((and (eql opcy '#\!)(equal (subseq opy 1) opx))
	   (unify x (make-compound :op (intern (subseq opy 1)) :args (compound-args y))))
	  (t 'failure))))    

(defun unify (x y &optional +theta+)
;  (format t "~%~A  ~A  ~A  {~A}" "UNIFY" x y +theta+)
  (cond ((eq +theta+ 'failure) 'failure)
	((eql x y) +theta+)
	((var? x) (unify-var x y +theta+))
	((var? y) (unify-var y x +theta+))
	((and (compound-p x)
	      (compound-p y))
	 (unify (compound-args x) (compound-args y)
		(unify (compound-op x) (compound-op y) +theta+)))
	((and (listp x) (listp y))
	 (unify (cdr x) (cdr y)
		(unify (car x) (car y) +theta+)))
	(t 'failure)))

(defun unify-var (var x +theta+)
  (let ((xsub (subs x +theta+)))
    (cond ((assoc var +theta+) (unify (cadr (assoc var +theta+)) x +theta+))
	  ((assoc x +theta+) (unify var (cadr (assoc x +theta+)) +theta+))
	  ((occurs? var xsub) 'failure)
	  (t (cons (list var xsub) +theta+)))))

(defun var? (a)
    (when (symbolp a) (eq (char (string a) 0) #\?)))

(defun occurs? (var x)
  (cond ((equal var x) t)
	((or (null x)(null var)) nil)
	((compound-p x) (occurs? var (compound-args x)))
	((listp x) (or (occurs? var (car x))
		       (occurs? var (cdr x))))
	(t nil)))

(defun subs (clause +theta+)
  (let ((vsub (cadr (assoc clause +theta+))))
    (cond  ((null clause) nil)
	   ((compound-p clause)
	    (make-compound :op (compound-op clause)
			   :args (subs (compound-args clause) +theta+)))
	   ((var? clause)(if (null vsub) clause vsub))
	   ((listp clause)(cons (subs (car clause) +theta+) (subs (cdr clause) +theta+)))
	   (t clause))))

(defun sd-apart (KB)
  (loop
     for stmt in KB
     for i from 1
     collect (var-replace stmt i)))

(defun var-replace (stmt i)
  (cond ((compound-p stmt)
	 (make-compound :op (compound-op stmt) :args (var-replace (compound-args stmt) i)))
	((null stmt) nil)
	((listp stmt)
	 (cons (var-replace (car stmt) i) (var-replace (cdr stmt) i)))
	((var? stmt)
	 (intern (concatenate 'string (string stmt)(write-to-string i))))
	(t stmt)))

; (setf A (list (m-c 'Animal (m-c 'F '?x)) (m-c 'Loves (m-c 'G '?x) '?x)))
; (setf B (list (m-c '!Loves '?x (m-c 'F '?x)) (m-c 'Loves (m-c 'G '?x) '?x)))
; (setf C (list (m-c '!Loves '?y '?x) (m-c '!Animal '?z) (m-c '!Kills '?x '?z)))
; (setf D (list (m-c '!Animal '?x) (m-c 'Loves 'Jack '?x)))
; (setf E (list (m-c 'Kills 'Jack 'Tuna) (m-c 'Kills 'Curiosity 'Tuna)))
; (setf F (list (m-c 'Cat 'Tuna)))
; (setf G (list (m-c '!Cat '?x) (m-c 'Animal '?x)))

; (setf p (list (m-c '!Kills 'Curiosity 'Tuna)))

; (setf KB (list f g e b d a c))
; (setf skb (sd-apart kb))

(defparameter *tuna-kb*
  (list
   (list (m-c 'Animal (m-c 'F '?x))
	 (m-c 'Loves (m-c 'G '?x) '?x))
   (list (m-c '!Loves '?x (m-c 'F '?x))
	 (m-c 'Loves (m-c 'G '?x) '?x))
   (list (m-c '!Loves '?y '?x)
	 (m-c '!Animal '?z)
	 (m-c '!Kills '?x '?z))
   (list (m-c '!Animal '?x)
	 (m-c 'Loves 'Jack '?x))
   (list (m-c 'Kills 'Jack 'Tuna)
	 (m-c 'Kills 'Curiosity 'Tuna))
   (list (m-c 'Cat 'Tuna))
   (list (m-c '!Cat '?x)
	 (m-c 'Animal '?x))))

(defparameter *tuna-query*
   (list (m-c '!Kills 'Curiosity 'Tuna)))

(defparameter *west-kb*
  (list
   (list (m-c '!American '?x)
	 (m-c '!Weapon '?y)
	 (m-c '!Sells '?x '?y '?z)
	 (m-c '!Hostile '?z)
	 (m-c 'Criminal '?x))
   (list (m-c '!Missle '?x)
	 (m-c '!Owns 'Nono '?x)
	 (m-c 'Sells 'West '?x 'Nono))
   (list (m-c '!Enemy '?x 'America)
	 (m-c 'Hostile '?x))
   (list (m-c '!Missle '?x)
	 (m-c 'Weapon '?x))
   (list (m-c 'Owns 'Nono 'M1))
   (list (m-c 'Missle 'M1))
   (list (m-c 'American 'West))
   (list (m-c 'Enemy 'Nono 'America))))

(defparameter *west-query*
  (list (m-c '!Criminal 'West)))

(defparameter *smuggler-kb*
  (list
   (list (m-c '!E '?x)
	 (m-c 'V '?x)
	 (m-c 'S '?x (m-c 'F '?x)))
   (list (m-c '!E '?x)
	 (m-c 'V '?x)
	 (m-c 'C (m-c 'F '?x)))
   (list (m-c 'P 'C))
   (list (m-c 'E 'C))
   (list (m-c '!S 'C '?y)
	 (m-c 'P '?y))
   (list (m-c '!P '?z)
	 (m-c '!V '?z))))

(defparameter *smuggler-query*
  (list (m-c '!P '?w)
	(m-c '!C '?w)))

(defparameter *coral-club-kb*
  (list
   (list (m-c 'WS '?x)
	 (m-c 'SD '?x))
   (list (m-c '!SD '?y)
	 (m-c '!Likes '?y 'Waves))
   (list (m-c '!WS '?z)
	 (m-c 'Likes '?z 'Warm))
   (list (m-c '!Likes 'Laura '?w)
	 (m-c '!Likes 'Jacob '?w))
   (list (m-c 'Likes 'Jacob '?w)
	 (m-c 'Likes 'Laura '?w))
   (list (m-c 'Likes 'Jacob 'Warm))
   (list (m-c 'Likes 'Jacob 'Waves))))

(defparameter *coral-club-query*
  (list (m-c '!SD '?v)
	(m-c 'WS '?v)))
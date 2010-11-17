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

; defun m-c (a &rest b)
; defun atp (kb a)
; defun resolve (skb depth)
; defun get-next-res (node fringe)
; defun r-s (x y)
; defun unify-facts (x y)
; defun unify-clauses (x y)
; defun unify (x y &optional +theta+)
; defun unify-var (var x +theta+)
; defun subs (clause +theta+)
; defun sd-apart (KB)
; defun var-replace (stmt i)

(defun m-c (a &rest b)
  (make-compound :op a :args b))

(defun atp (kb a)
  (let ((skb (sd-apart kb)))
    (resolve (append skb (list a)) 20)))

(defun resolve (skb depth)
  (format t "~%~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A"
	  "==RESOLVE==" (nth 0 skb)(nth 1 skb)
	  (nth 2 skb) (nth 3 skb) (nth 4 skb)
	  (nth 5 skb) (nth 6 skb) (nth 7 skb))
  (let ((node (car skb)) ;current node to try matching
	(fringe (cdr skb)) ;the rest of the list
	(next-res (get-next-res (car skb)(cdr skb)))) ;the node to resolve
    (cond ((eql depth 0) nil)
	  ((not (null next-res))
	   (resolve (append (remove next-res fringe)
			    (list (r-s node next-res))) (1- depth)))
	  (t (resolve (append fringe (list node)) (1- depth))))))

(defun reslv (nres &optional pres depth)
  (let ((node (car (append nres pres)))
;	(fringe (append pres nres))
	(nextr (get-next-res (car (append nres pres))
			     (cdr (append pres nres)))))
;    (print (last pres))(print "=Reslv")(print node)(print nextr)
    (format t "~%~A~%~A~%~A~%~%~A~%~%~A~%"
	    "=Resolve" node nextr nres pres)
    (cond ((eql depth 0) nil)
	  ((not (null nextr))
	   (reslv (remove nextr (cdr nres))
		  (append (remove node pres) (list (r-s node nextr)))
		  (1- depth)))
	  (t
	   (reslv (cdr nres)
		  (append (cdr pres) (list (car pres)))
		  (1- depth))))))

(defun get-next-res (node fringe)
  (loop for e in fringe
     do (unless (equal (unify-facts e node) 'failure)(return e))))

;(defun r-s (x y)
;  (format t "~%~A~%" "==Resolve-Sentences==")
;  (let ((tset (append x y))
;	(sset (unify-facts x y)))
;    (loop for e in tset
;       do (format t "~%~%~A ~A ~A" restt (find e x) (find e y))
;       do (format t "~%~A ~A" (unify-facts (list e) x) (unify-facts (list e) y))
;       collect (cond ((find e x)
;		      (cond ((eql (unify-facts (list e) y) '(+NULL+)) 'nil)
;			    ((eql (unify-facts (list e) y) 'nil) (subs e sset))
;			    (t 'nil)))
;		     ((find e y)
;		      (cond ((eql (unify-facts (list e) x) '(+NULL+)) 'nil)
;			    ((eql (unify-facts (list e) x) 'nil) (subs e sset))
;			    (t 'nil))))
;       into restt
;       finally (return (remove-if 'null restt)))))

(defun r-s (x y)
;  (format t "~%=R-S=~%~A ~A" x y)
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
;  (format t "~%~A  ~A  ~A  {~A}" "    UNIFY-VAR" var x +theta+)
  (cond ((assoc var +theta+) (unify (cadr (assoc var +theta+)) x +theta+))
	((assoc x +theta+) (unify var (cadr (assoc x +theta+)) +theta+))
	((occurs? var x +theta+) 'failure)
	(t (cons (list var x) +theta+))))

(defun var? (a)
    (when (symbolp a) (eq (char (string a) 0) #\?)))

(defun occurs? (var x +theta+)
;  (format t "~&~A" "        OCCURS")
  (cond ((equal var x) t)
	((or (null x)(null var)) nil)
	((and (var? x)
	      (assoc x +theta+))
	 (occurs? var (cadr (assoc x +theta+)) +theta+))
	((compound-p x) (occurs? var (compound-args x) +theta+))
	((listp x) (or (occurs? var (car x) +theta+)
		       (occurs? var (cdr x) +theta+)))
	(t nil)))

(defun subs (clause +theta+)
  (cond  ((null clause) nil)
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

; (setf KB (list f g e p b d a c))

; (setf SAB (list (m-c '!Loves '?x2 (m-c 'F '?x2)) (m-c 'Loves (m-c 'G '?x2) '?x2)))
; (setf SAD (list (m-c '!Animal '?x4) (m-c 'Loves 'Jack '?x4)))

; (setf A1 (list (m-c '!LOVES '?Y3 '?X3) (m-c '!KILLS '?X3 'TUNA)))
; (setf A2 (list (m-c 'KILLS 'JACK 'TUNA) (m-c 'KILLS 'CURIOSITY 'TUNA)))
; (setf A3 (list (m-c '!KILLS 'CURIOSITY 'TUNA)))

; (setf B1 (list (m-c 'Animal (m-c 'F '?X7)) (m-c 'Loves (m-c 'G '?x7) '?x7)))
; (setf B2 (list (m-c '!Loves '?y8 '?x8) (m-c '!Animal '?z8) (m-c '!kills '?x8 '?z8)))

; x / Father(y)
; y / Father(x)
; x / Father(Father(x))
;
; Loves(x,Father(x)
; ~Loves(Father(y),y) + Hates(y,Mother(y))
; Hates(Father(Father(y))
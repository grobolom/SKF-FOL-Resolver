;; Defining some basic types : an atomic sentence, and what it is built of

;; statements, functions, and variables are represented as structs
;; constants will simply be represented with atoms
;; terms are 'imaginary' constructs - if strict definition is necessary, then
;; we will create a struct for them that has two variables : type and value


;; An Atomic Sentence - Predicate + Terms

;!!! Might Not Be Necessary
(defstruct (stmt) pred terms)


;; A Function - this is similar to an atomic sentence

;!!! Might Not Be Necessary
;(defstruct (func) pred terms)


;; This is now any compound statement : King(x) | Knows(y,Mother(y))
(defstruct (compound) op args)


;; A variable - it has a default value of nil, but if can be assigned
;; the value of a constant
(defstruct (u-var) name)


;; Defining a complex sentence here

;; It consists of atomic sentences separated by connectives
;; connectives are normally !, =>, /\, \/, <=>
;; but since we are assumed to be in SNF, we only need to worry about
;; the NOT symbol, or ! - this is because SNF sentences must be split
;; by ORS, and complex sentences are always split by ANDS.
(defstruct (sentence) statements)

;; Defining a substitution as below
(defstruct (substitution) vars values)

;; Okay.. so now the Unification function?

;; This function will take two complex sentences (struct sentence)
;; it will output the combined statement with the proper substitutions

;; We first need a substitution finder function that returns the substitution
;; +theta+ that is a set of {x1/y1 .. xn/yn} substitutions that fits the two
;; clauses X and Y

;(defun build-substitution (A B) ;Takes two clauses
;  (let ((a-terms (stmt-terms A))
;	(b-terms (stmt-terms B)))
;    (print a-terms)
;    (print b-terms)
;    t))

;(defun get-substitution (A B) ; These are Clauses AKA atomic sentences AKA stmt
;  (if (and (stmt-p A) ; if A is a statement
;	   (stmt-p B) ; and B is too
;	   (eq (stmt-pred A) (stmt-pred B))) ;and the predicates are equal
;      (build-substitution A B) ; build the substitution
;      nil))

;; Commented out This bullshit so I can build according to the book's pseudocode.
;; May require some modification of our structs.
;; Guess we'll see.

(defun unify (x y +theta+)
  "returns a substitution to make x and y identical"
  (cond ((eq +theta+ 'failure) 'failure)
	((eql x y) +theta+)
	((u-var-p x) (unify-var x y +theta+))
	((u-var-p y) (unify-var y x +theta+))
	((and (compound-p x) (compound-p y))
	 (unify (compound-args x) (compound-args y)
		(unify (compound-op x) (compound-op y) +theta+)))
	((and (listp x) (listp y))
	 (unify (cdr x) (cdr y)
		(unify (cadr x) (cadr y) +theta+)))
	(t 'failure)))

(defun unify-var (var x +theta+)
  "returns a substitution"
; need some association code here
; we're going to have a data structure : list with two elements
; ( variable value ) that represents {var/val}
  (cond ((assoc var +theta+) (unify (cadr (assoc var +theta+)) x +theta+))
	((assoc x +theta+) (unify var (cadr (assoc x +theta+)) +theta+))
	(t (cons (list var x) +theta+))))
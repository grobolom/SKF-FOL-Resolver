;; Defining some basic types : an atomic sentence, and what it is built of

;; statements, functions, and variables are represented as structs
;; constants will simply be represented with atoms
;; terms are 'imaginary' constructs - if strict definition is necessary, then
;; we will create a struct for them that has two variables : type and value


;; An Atomic Sentence - Predicate + Terms
(defstruct (stmt) pred terms)


;; A Function - this is similar to an atomic sentence
(defstruct (func) pred terms)


;;  This is a Skolem Function - I expect this to have slightly different functionality
;;  than the standard function, so it is a different struct. EX: F(x)
(defstruct (skfunc) pred vars)


;; A variable - it has a default value of nil, but if can be assigned
;; the value of a constant
(defstruct (var) (value nil))


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

(defun build-substitution (A B) ;Takes two clauses
  (let ((a-terms (stmt-terms A))
	(b-terms (stmt-terms B)))
    (print a-terms)
    (print b-terms)
    t))

(defun get-substitution (A B) ; These are Clauses AKA atomic sentences AKA stmt
  (if (and (stmt-p A) ; if A is a statement
	   (stmt-p B) ; and B is too
	   (eq (stmt-pred A) (stmt-pred B))) ;and the predicates are equal
      (build-substitution A B) ; build the substitution
      nil))


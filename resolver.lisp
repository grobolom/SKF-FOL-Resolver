;; Defining some basic types : an atomic sentence, and what it is built of

;; statements, functions, and variables are represented as structs
;; constants will simply be represented with atoms
;; terms are 'imaginary' constructs - if strict definition is necessary, then
;; we will create a struct for them that has two variables : type and value

(defstruct (stmt)
  "An Atomic Sentence - Predicate + Terms" 
  pred terms)

(defstruct (func)
  "A Function - this is similar to an atomic sentence"
  pred terms)

(defstruct (skfunc)
  "This is a Skolem Function - I expect this to have slightly different functionality
   than the standard function, so it is a different struct. EX: F(x)"
  pred vars)

(defstruct (var)
  "A variable - it has a default value of nil, but if can be assigned
   the value of a constant"
  (value nil))

;; Defining a complex sentence here

;; It consists of atomic sentences separated by connectives
;; connectives are normally !, =>, /\, \/, <=>
;; but since we are assumed to be in SNF, we only need to worry about
;; the NOT symbol, or ! - this is because SNF sentences must be split
;; by ORS, and complex sentences are always split by ANDS.

(defstruct (sentence)
  statements)
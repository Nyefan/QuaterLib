#lang racket

(provide quaternion-add 
         quaternion-subtract 
         quaternion-multiply 
         quaternion-divide 
         quaternion-exp
         quaternion-expt
         quaternion-log
         quaternion-sin
         quaternion-cos
         quaternion-tan
         quaternion-asin
         quaternion-acos
         quaternion-atan
         quaternion-sinh
         quaternion-cosh
         quaternion-tanh
         quaternion-asinh
         quaternion-acosh
         quaternion-atanh)

; Define the vector operations that are supported
(define (vector? vectorA)
  (and (list? vectorA) (equal? (length vectorA) 3) ) )
; end of vector?

(define (InputTypeException) (error 'vector-len "One or more inputs is not of the proper form.") )
; end of InputTypeException

(define (vector-len vectorA)
  (cond
    ( (vector? vectorA)
      (sqrt (vector-dotProduct vectorA vectorA) ) )
    (else (InputTypeException) ) ) ); report error if the input(s) are not correctly formatted
; end of vector-len

(define (vector-unit vectorA)
  (cond 
    ( (vector? vectorA)
      (vector-scalarProduct (/ 1 (vector-len vectorA) ) vectorA) )
    (else (InputTypeException) ) ) ); report error if the inputs(s) are not correctly formatted
; end of vector-unit

(define (vector-dotProduct vectorA vectorB)
  (cond 
    ( (and (vector? vectorA) (vector? vectorB) )
      (apply + (map * vectorA vectorB) ) )
    (else (InputTypeException) ) ) ); report error if the inputs(s) are not correctly formatted
; end of vector-dotProduct

(define (vector-scalarProduct numberA vectorB)
  (cond
    ( (and (number? numberA) (vector? vectorB) ) 
      (map (curry * numberA) vectorB) )
    (else (InputTypeException) ) ) ); report error if the inputs(s) are not correctly formatted
; end of vector-scalarProduct

(define (vector-crossProduct vectorA vectorB)
  (cond 
    ( (and (vector? vectorA) (vector? vectorB) )
      (list (- (* (list-ref vectorA 1) (list-ref vectorB 2) ) (* (list-ref vectorA 2) (list-ref vectorB 1) ) )
            (- (* (list-ref vectorA 2) (list-ref vectorB 0) ) (* (list-ref vectorA 0) (list-ref vectorB 2) ) )
            (- (* (list-ref vectorA 0) (list-ref vectorB 1) ) (* (list-ref vectorA 1) (list-ref vectorB 0) ) ) ) )
    (else (InputTypeException) ) ) ); report error if the inputs(s) are not correctly formatted
; end of vector-crossProduct
                
; Define the quaternion operations that are supported
;-----------------------------------------------------
; AS OF NOW: All quaternion functions assume the 
; inputs are of the proper for with NO ERROR CHECKING.
;-----------------------------------------------------
; Quaternions are represented by a 4-element list
; Using quaternion-realPart and quaternion-vectorPart, 
; quaternions can be treated ast the sum of a real 
; number and an imaginary vector, mathematically 
; represented as r + v

; Returns the number representation, r, of the real part of the quaternion
(define (quaternion-realPart quatA)
  (first quatA) ); end quaternion-realPart

; Returns the vector representation, v, of the imaginary part of the quaternion
(define (quaternion-vectorPart quatA)
  (list-tail quatA 1) ); end quaternion-vectorPart

; Returns a quaternion containing the unit vector part of the input quaternion
; If the vector part of the input quaternion is (0 0 0), it returns (0 0 0 0)
; This behavior is expected by all the functions that call quaternion-unitVector
(define (quaternion-unitVector quatA)
  (cond 
    ( (equal? 
       (quaternion-vectorPart quatA) 
       (list 0 0 0) )
      (list 0 0 0 0) )
    (else 
     (cons
      0
      (vector-unit
       (quaternion-vectorPart
        quatA ) ) ) ) ) )
; end quaternion-unitVector

; Extended to work with an arbitrary number of inputs
; Returns the result of quaternion addition, (rA+rB, vA+vB)
(define (quaternion-add quatA quatB . args)
  (apply (curry map +) (append (list quatA quatB) args) ) ); end quaternion-add

; Extended to work with an arbitrary number of inputs
; Returns the result of quaternion subtraction, (rA-rB, vA-vB)
(define (quaternion-subtract quatA quatB . args)
  (apply (curry map -) (append (list quatA quatB) args) ) ); end quaternion-subtract

; Extended to work with an arbitrary number of inputs
; Returns the result of quaternion multiplication, (rA*rB - vA.vB, rA*vB + rB*vA + vAxvB)
(define (quaternion-multiply quatA quatB . args)
  (quaternion-multiplyHelper ; passes args in a recursive-friendly format to the helper
   quatA 
   quatB 
   args ) )
; end quaternion-multiply

; A recursive extension to the quaternion-multiply function that allows an arbitrary number of arguments to be used
; This should remain private and NOT be provided, except perhaps in the extras
(define (quaternion-multiplyHelper quatA quatB listOfArgs)
  (let (
        (quatAB ; define quatAB as quatA*quatB
         (cons ; return (newRealPart, newVectorPart)
          (- ; rA*rB - vA.vB
           (* ; rA*rB
            (quaternion-realPart quatA) 
            (quaternion-realPart quatB) ) 
           (vector-dotProduct ; vA.vB
            (quaternion-vectorPart quatA)
            (quaternion-vectorPart quatB) ) ) ; newRealPart
          (map + 
               (vector-scalarProduct ; rA*vB
                (quaternion-realPart quatA)
                (quaternion-vectorPart quatB) )
               (vector-scalarProduct ; rB*vA
                (quaternion-realPart quatB)
                (quaternion-vectorPart quatA) )
               (vector-crossProduct ; vAxvB
                (quaternion-vectorPart quatA)
                (quaternion-vectorPart quatB) ) ) ) ) )
    (cond
      ( (empty? listOfArgs) quatAB )
      (else 
       (quaternion-multiplyHelper
        quatAB
        (first listOfArgs)
        (rest listOfArgs) ) ) ) )  )
; end quaternion-multiplyHelper

; Returns the result of scalar multiplication of a quaternion
; This is equivalent to (but faster than) :
; (quaternion-multiply '(numberA 0 0 0) quatB)
; This operation is also a special commutative case, so it 
; is also equivalent to :
; (quaternion-multiply quatB '(numberA 0 0 0) )
(define (quaternion-scalarMultiply numberA quatB)
  (map (curry * numberA) quatB) )
; end quaternion-scalarMultiply
  

; Returns the result of a vector rotation by applying a
; quaternion to the vector as an operator,
; (vB + 2*rA*(vAxvB) + 2*(vAx(vAxvB)))
; NOTE: This functionality is unlikely to be needed and
;       it may be appropriate to place it in the
;       "extras" portion of the module.
(define (quaternion-vectorMultiply quatA vectorB)
  (map + 
       vectorB
       (vector-scalarProduct 
        (quaternion-realPart quatA) 
        (vector-scalarProduct 
         2 
         (vector-crossProduct 
          (quaternion-vectorPart quatA) 
          vectorB) ) )
       (vector-crossProduct 
        (quaternion-vectorPart quatA) 
        (vector-scalarProduct 
         2 
         (vector-crossProduct 
          (quaternion-vectorPart quatA) 
          vectorB) ) ) ) )
; end quaternion-vectorMultiply

; Returns the result of quaternion division, 
; assuming the intended input is A*(B^-1)  (option 1)
; Note that this is distinct from (B^-1)*A (option 2)
; If option 2 is required, it can be achieved by swapping 
; the inputs to the quaternion-multiply function,
; i.e. - (quaternion-multiply (quaternion-invert quatB) quatA)
; EDIT: option 2 has been implemented in quaternion-divideLeft
;       if this is renamed to quaternion-divideRight, some 
;       other functions may need to be changed to match.
(define (quaternion-divide quatA quatB . args)
  (cond 
    ( (null? args) (quaternion-multiply quatA (quaternion-invert quatB) ) )
    (else (quaternion-multiplyHelper quatA (quaternion-invert quatB) (map quaternion-invert args) ) ) ) )
; end quaternion-divide

; Returns the result of left quaternion division,
; mathematically represented as (A^-1)*B
(define (quaternion-divideLeft quatA quatB)
  (quaternion-multiply (quaternion-invert quatA) quatB) )

; Returns the result of quaternion normalization,
; This is also commonly expressed notationally as ||A||
; The same code can be used here as for the length of 
; a vector, applied to 4 elements instead of 3.
; operation
; (r^2 + v.v)^(1/2)
(define (quaternion-norm quatA)
  (sqrt (apply + (map * quatA quatA) ) ) )
; end quaternion-norm

; Returns the result of quaternion inversion,
; (A*) / ||A||^2
(define (quaternion-invert quatA)
  (quaternion-scalarMultiply 
   (/ 1 (apply + (map * quatA quatA) ) ) ; 1/||A||^2
   (quaternion-conjugate quatA) ) ) ; A*
;end quaternion-invert
   

; Returns the result of quaternion conjugation
; (r-v)
(define (quaternion-conjugate quatA)
  (cons 
   (quaternion-realPart quatA)
   (vector-scalarProduct 
    -1 
    (quaternion-vectorPart quatA) ) ) )
; end quaternion-conjugate

; Returns the distance between two quaternions
; ||A-B||
(define (quaternion-distance quatA quatB) 
  (quaternion-norm ; ||A-B||
   (quaternion-subtract ; A-B
    quatA ; A
    quatB) ) ) ; B
; end quaternion-distance

; Returns the result of Euler quaternion exponentiation,
; e^(A) = ( e^(rA)*cos(|vA|), (vA/|vA|)*sin(|vA|)*e^(rA) )
(define (quaternion-exp quatA) 
  (cons ; (realPart, vectorPart)
   
   (* ; (e^rA) * cos(|vA|)
    (exp (quaternion-realPart quatA) ) ; e^rA
    (cos (vector-len (quaternion-vectorPart quatA) ) ) ; cos(|vA|)
    ) ; realPart   
   (vector-scalarProduct ; (vA/|vA|)*sin(|vA|)
    (*
     (sin (vector-len (quaternion-vectorPart quatA) ) ) ; sin(|vA|)
     (exp (quaternion-realPart quatA) ) ) ; e^rA
    (quaternion-vectorPart (quaternion-unitVector quatA) ) ; vA/|vA|
    ) ; vectorPart
   
   ) ; end cons
  ) ; end quaternion-exp

; Returns the natural log of an input quaternion
; ln(A) = ( ln(||A||), acos(rA/||A||)*(vA/|vA|)
(define (quaternion-log quatA) ; this is the natural log, in keeping with the racket standard
  (cons ; (realPart, vectorPart)
   ; The scalar part
   (log (quaternion-norm quatA) ) ; ln(||A||) = realPart
   ; The vector part
   (quaternion-vectorPart ; acos(rA/||A||)*(vA/|vA|) = vectorPart
    (quaternion-scalarMultiply ; scalarMultiple*(vA/|vA|)
     (acos ; acos(innerPart) = scalarMultiple
      (/ ; rA/||A|| = innerPart
       (quaternion-realPart quatA) ; rA
       (quaternion-norm quatA) ) ) ; ||A||
     (quaternion-unitVector quatA) ) ) ) ) ; vA/|vA|
; end quaternion-log

; Returns the result of arbitrary quaternion exponentiation,
; A^B = e^(ln(A)*B)
(define (quaternion-expt quatA quatB); this raises quatA to the power of quatB
  (quaternion-exp ; e^exponent
   (quaternion-multiply ; ln(A)*B = exponent
    (quaternion-log quatA) ; ln(A)
    quatB) ) ) ; B
; end quaternion-expt

; Returns the sin of an input quaternion,
; ( sin(rA)*cosh(|vA|), cos(rA)*sinh(|vA|)*(vA/|vA|) )
(define (quaternion-sin quatA)
  (cons 
   ; The scalar part
   (* ; sin(rA) * cosh(|vA|)
    (sin (quaternion-realPart quatA) ) ; sin(rA)
    (cosh (vector-len (quaternion-vectorPart quatA) ) ) ) ; cosh(|vA|)
   ; The vector part
   (quaternion-vectorPart 
    (quaternion-scalarMultiply ; scalarMultiple * vA/|vA|
     (*  ; cos(rA) * sinh(|vA|) = scalarMultiple
      (cos (quaternion-realPart quatA) ) ; cos(rA)
      (sinh (vector-len (quaternion-vectorPart quatA) ) ) ) ; sinh(|vA|)
     (quaternion-unitVector quatA) ) ) ) ) ; vA/|vA|
; end quaternion-sin

; Returns the cos of an input quaternion,
; ( cos(rA)*cosh(|vA|), sin(rA)*sinh(|vA|)*(-vA/|vA|) )
(define (quaternion-cos quatA)
  (cons
   ; The scalar term
   (* ; cos(rA) * cos(|vA|)
    (cos (quaternion-realPart quatA) ); cos(rA)
    (cosh (vector-len (quaternion-vectorPart quatA) ) ) ) ; cosh(|vA|)
   ; The vector term
   (quaternion-vectorPart
    (quaternion-scalarMultiply ; scalarMultiple * vA/|vA|
     (* ; sinh(rA) * sin(|vA|) = scalarMultiple
      (sin (quaternion-realPart quatA) ) ; sin(rA)
      (sinh (vector-len (quaternion-vectorPart quatA) ) ) ) ; sinh(|vA|)
     (quaternion-conjugate (quaternion-unitVector quatA) ) ) ) ) ) ; -vA/|vA|
; end quaternion-cos

; Returns the tan of an input quaternion,
; sin(A)/cos(A)
(define (quaternion-tan quatA)
  (quaternion-divide ; sin(A)/cos(A)
   (quaternion-sin quatA) ; sin(A)
   (quaternion-cos quatA) ) ) ; cos(A)
; end quaternion-tan

; Returns the asin of an input quaternion,
; asinh( A * [0,vA/|vA|] ) * (-vA/|vA|)
(define (quaternion-asin quatA)
  (cond
    ( (andmap zero? (quaternion-vectorPart quatA) ) ; if the quaternion is purely real
      (cons ; then return (asin(rA), 0, 0, 0)
       (asin (quaternion-realPart quatA) ) ; asin(rA)
       (list 0 0 0) ) ) ; '(0 0 0)
    (else
     (quaternion-multiply ; asinh(A*vA/|vA|) * (-vA/|vA|)
      (quaternion-conjugate (quaternion-unitVector quatA) ) ; (-vA/|vA|)
      (quaternion-asinh ; asing(innerPart)
       (quaternion-multiply ; A*vA/|vA| = innerPart
        quatA ; A
        (quaternion-unitVector quatA) ) ) ) ) ) ) ; vA/|vA|
; end quaternion-asin

; Returns the acos of an input quaternion,
; acosh( A * [0,vA/|vA|] ) * (-vA/|vA|)
(define (quaternion-acos quatA)
  (cond
    ( (andmap zero? (quaternion-vectorPart quatA) ) ; if the quaternion is purely real
      (cons ; then return (acos(rA), 0, 0, 0)
       (acos (quaternion-realPart quatA) ) ; acos(rA)
       (list 0 0 0) ) ) ; '(0 0 0)
    (else
     (quaternion-multiply ; acosh(A*vA/|vA|) * (-vA/|vA|)
      (quaternion-conjugate (quaternion-unitVector quatA) ) ; (-vA/|vA|)
      (quaternion-acosh ; acosh(innerPart)
       ;(quaternion-multiply ; A*vA/|vA| = innerPart
        quatA ; A
        ;(quaternion-unitVector quatA) ) ) ) ) ) ) ; vA/|vA|
        ) ) ) ) )
; end quaternion-acos

; Returns the atan of an input quaterion
; atanh( A * [0,vA/|vA|] ) * (-vA/|vA|)
(define (quaternion-atan quatA)
  (cond
    ( (andmap zero? (quaternion-vectorPart quatA) ) ; if the quaternion is purely real
      (cons ; then return (atan(rA), 0, 0, 0)
       (atan (quaternion-realPart quatA) ) ; atan(rA)
       (list 0 0 0) ) ) ; '(0 0 0)
    (else
     (quaternion-multiply ; atanh(A*vA/|vA|) * (-vA/|vA|)
      (quaternion-conjugate (quaternion-unitVector quatA) ) ; (-vA/|vA|)
      (quaternion-atanh ; atanh(innerPart)
       (quaternion-multiply ; A*vA/|vA| = innerPart
        quatA ; A
        (quaternion-unitVector quatA) ) ) ) ) ) ) ; vA/|vA|
; end quaternion-atan

; Returns the sinh of an input quaternion,
; ( sinh(rA)*cos(|vA|), cosh(rA)*sin(|vA|)*(vA/|vA|) )
(define (quaternion-sinh quatA)
  (cons ; (realPart, vectorPart)
   ; The real part
   (* ; sinh(rA)*cos(|vA|)
    (sinh (quaternion-realPart quatA) ) ; sinh(rA)
    (cos (vector-len (quaternion-vectorPart quatA) ) ) ) ; cos(|vA|)
   ; The vector part
   (quaternion-vectorPart 
    (quaternion-scalarMultiply
     (*
      (cosh (quaternion-realPart quatA) )
      (sin (vector-len (quaternion-vectorPart quatA) ) ) )
     (quaternion-unitVector quatA) ) ) ) )
; end quaternion-sinh

; Returns the cosh of an input quaternion,
; ( cosh(rA)*cos(|vA|), sinh(rA)*sin(|vA|)*(vA/|vA|) )
(define (quaternion-cosh quatA) 
  (cons 
   ; The scalar term
   (* ; cos(rA) * cos(|vA|)
    (cosh (quaternion-realPart quatA) ) ; cosh(rA)
    (cos (vector-len (quaternion-vectorPart quatA) ) ) ) ; cos(|vA|)
   ; The vector term
   (quaternion-vectorPart
    (quaternion-scalarMultiply ; scalarMultiple * vA/|vA|
     (* ; sinh(rA) * sin(|vA|) = scalarMultiple
      (sinh (quaternion-realPart quatA) ) ; sinh(rA)
      (sin (vector-len (quaternion-vectorPart quatA) ) ) ) ; sin(|vA|)
     (quaternion-unitVector quatA) ) ) ) ) ; vA/|vA|
; end quaternion-cosh

; Returns the tanh of an input quaternion,
; sinh(A)/cosh(A)
(define (quaternion-tanh quatA)
  (quaternion-divide 
   (quaternion-sinh quatA) 
   (quaternion-cosh quatA) ) )
; end quaternion-tanh

; Returns the asinh of an input quaternion,
; ln( A + (1+A^2)^(1/2) )
(define (quaternion-asinh quatA)
  (quaternion-log ; ln( 
   (quaternion-add 
    quatA ; A +
    (quaternion-expt 
     (quaternion-add 
      (quaternion-expt 
       quatA 
       (list 2 0 0 0) ) ; (A^2
      (list 1 0 0 0) ) ; + 1)
     (list 0.5 0 0 0) ) ) ) ) ; ^(1/2) )
; end quaternion-asinh

; Returns the acosh of an input quaternion,
; ln( A +/- (A^2-1)^(1/2) )
(define (quaternion-acosh quatA)
  (let (
        (internSum ; locally bind internSum
         (quaternion-add ; to be A+(A^2-1)^0.5
          quatA ; A
          (quaternion-expt ; (A^2-1)^0.5
           (quaternion-subtract ; A^2-1
            (quaternion-expt ; A^2
             quatA ; A
             (list 2 0 0 0) ) ; ^2
            (list 1 0 0 0) ) ; - 1
           (list 0.5 0 0 0) ) ) ) ; ^(1/2)
        (internDiff ; locally bind internDiff
         (quaternion-subtract ; to be A-(A^2-1)^0.5
          quatA ; A
          (quaternion-expt ; (A^2-1)^0.5
           (quaternion-subtract ; A^2-1
            (quaternion-expt ; A^2
             quatA ; A
             (list 2 0 0 0) ) ; ^2
            (list 1 0 0 0) ) ; - 1
           (list 0.5 0 0 0) ) ) ) ) ; ^(1/2)
    (cond ; return the ln of the quaternion (internSum or internDiff) that has the greater norm (magnitude)
      ( (> (quaternion-norm internSum) (quaternion-norm internDiff) )
        (quaternion-log internSum) )
      (else
       (quaternion-log internDiff) ) ) ) )
; end quaternion-acosh

; Returns the atanh of an input quaternion
; 0.5*ln( (1+A)/(1-A) )
; NOTE: There is an issue with this function at the moment - namely,
;       it doesn't account for the case where vA == 0 and rA > 1.
;       In this case, the i component that is returned should be
;       multiplied by -1 to be correct.  A solution to this must be
;       considered.
(define (quaternion-atanh quatA)
  (quaternion-scalarMultiply ; 0.5*quatTerm
   0.5
   (quaternion-log
    (quaternion-divide
     (quaternion-add
      (list 1 0 0 0)
      quatA)
     (quaternion-subtract
      (list 1 0 0 0)
      quatA) ) ) ) )
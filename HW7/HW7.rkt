;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname HW7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))

(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

(define boolean-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '<) (< num1 num2))
      ((eq? op '>) (> num1 num2))
      ((eq? op '==) (= num1 num2))
      ((eq? op '>=) (>= num1 num2))
      ((eq? op '<=) (<= num1 num2))
      ((eq? op '!=) (not (= num1 num2))))))

; ***** PARSERS *****
(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))

(define no-code-ArithmeticBoolean-parser
  (lambda (no-code-bool)
    (list 'bool-exp (cadr no-code-bool) (no-parser (caddr no-code-bool))
          (no-parser (cadddr no-code-bool)))))
;(call (function (x y) (ask (< 1 2) (do-mathy-stuff + a b) otherwise 5) (do-mathy-stuff * a b) 15)

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (no-code-ArithmeticBoolean-parser (caadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code))))))) 
 
; ***** Interpreters *****
(define run-parsed-function-code
  (lambda (parsed-no-code-function env)
    (run-parsed-code (cadr (caddr parsed-no-code-function)) env)))

(define run-parsed-ArithmeticBoolean-code
  (lambda (parsed-no-code-bool-if parsed-no-code-bool-ifResult parsed-no-code-else env)
    (if (boolean-toaster (parsed-no-code-bool-if) 'true)
        (run-parsed-code (parsed-no-code-bool-ifResult) env)
    (if ((boolean-toaster (parsed-no-code-bool-if)) 'false)
        (run-parsed-code (parsed-no-code-else) env)))))
     

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
           (run-parsed-ArithmeticBoolean-code (cadr parsed no-code) (caddr parsed no-code)
                                              (cadr (cdddr parsed-no-code) env))
      (else
         (run-parsed-function-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define env '((age 21) (a 7) (b 5) (c 23)))
(define sample-no-code '(call (function (x y) (ask (bool < 1 2) (do-mathy-stuff + a b) otherwise 5)) (do-mathy-stuff * a b) 15))
(define parsed-no-code (no-parser sample-no-code))
;parsed-no-code
;(run-parsed-code parsed-no-code env)
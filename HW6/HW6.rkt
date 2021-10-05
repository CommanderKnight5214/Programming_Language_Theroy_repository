;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname HW6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define resolve
  (lambda (varName  env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      ;caar = car car, cadar = car cdr car.
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

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'function)
       (list 'func-exp
             (append (list 'params) (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      ((eq? (car no-code) 'call)
       (list 'call-exp
             (no-parser (cadr no-code))
             (map no-parser (cddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (no-parser (caadr no-code))
             (no-parser (caddr no-code))
             (no-parser (cadddr no-code))))))) 
;(ask (1) (do-mathy-stuff + 1 2) (do-mathy-stuff * 4 4))
;(ask-exp (num-lit-exp 1) (math-exp + (num-lit-exp 1) (num-lit-exp 2)) (math-exp * (num-lit-exp 4) (num-lit-exp 4)))

  ;(define sample-no-condition '(no-con (no-if = a 7 1) (no-else = a 8))))
(define env '((age 21) (a 7) (b 5) (c 23)))

(define sample-no-code '(call (function (x y) (do-mathy-stuff + x y)) (do-mathy-stuff * a b) 15))

;(math-exp + (var-exp a) (var-exp b))

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'ask-exp)
       (cond
         ((= (cadr (cadr parsed-no-code)) 1) (run-parsed-code (caddr parsed-no-code) env))
         ((= (cadr (cadr parsed-no-code)) 0) (run-parsed-code (cadddr parsed-no-code) env))
         (else '(Invalid Call, Suck Less!))))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      (else
       (run-parsed-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define parsed-no-code (no-parser sample-no-code))
parsed-no-code
(run-parsed-code parsed-no-code env)


(define sample-no-condition '(ask (1) (do-mathy-stuff + 1 2) (do-mathy-stuff * 4 4)))
(run-parsed-code (no-parser sample-no-condition) env)
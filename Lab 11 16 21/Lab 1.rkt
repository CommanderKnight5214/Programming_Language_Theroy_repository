;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Lab 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;2.
(define count-block-exp
  (lambda (block-exp)
    (count-block-exp-helper (Blockflattener block-exp) 0)))

(define Blockflattener
  (lambda (los)
    (cond
      ((null? los) '())
      ((list? (car los)) (append (Blockflattener (car los)) (Blockflattener (cdr los))))
      (else (cons (car los) (Blockflattener (cdr los)))))))

(define count-block-exp-helper
  (lambda (block-exp sum)
    (cond
      ((null? block-exp) sum)
      ((number? (car block-exp)) (count-block-exp-helper (cdr block-exp) (+ (car block-exp) sum)))
      (else (count-block-exp-helper (cdr block-exp) sum)))))
    
;3.
;Blockflattener is used for this problem as well. 
(define collect-symbols
  (lambda (block-exp)
    (symbol-block-exp-helper (Blockflattener block-exp))))

(define symbol-block-exp-helper
  (lambda (flattened-block-exp)
    (cond
      ((null? flattened-block-exp) '())
      ((symbol? (car flattened-block-exp)) (cons (car flattened-block-exp) (symbol-block-exp-helper (cdr flattened-block-exp))))
      (else (symbol-block-exp-helper (cdr flattened-block-exp))))))



(collect-symbols '(1 A (2 B (3 C (4 D (5 D (6 E (7 F (8 G)))))))))
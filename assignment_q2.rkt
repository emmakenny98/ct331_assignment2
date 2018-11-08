#lang racket

(provide ins_beg)
(provide ins_end)
(provide cout_top_level)
(provide count_instances)
(provide count_instances_deep)

;Question A
(define (ins_beg el lst)
   (cons el lst))

;Question B
(define (ins_end el lst)
     (cons lst el))

;Question C
(define (cout_top_level lst)
  (cond ((null? lst) 0)                 
  (else (+ 1 (cout_top_level (cdr lst))))))

;Question D
(define (count_instances item lst)
  (cond [(null? lst) 0]
        [(equal? item (car lst))(+ 1(count_instances item(cdr lst)))]
        [else (count_instances item (cdr lst))]))

;Question E


;Question F
(define (count_instances_deep item lst)
  (cond [(null? lst) 0]
        [(equal? item (car lst)) (+ 1 (count_instances_deep item (cdr lst)))]
        [(list? (car lst)) (+ (count_instances_deep item (car lst))
                              (count_instances_deep item (cdr lst)))]
        [else (count_instances_deep item (cdr lst))]))


(printf "ins_beg: ~a~n" (ins_beg '1 '(2 3 4 5)))
(printf "ins_beg: ~a~n"(ins_beg '(1 2) '(3 4 5 6)))

(printf "ins_end: ~a~n"(ins_end '1 '(2 3 4 5)))
(printf "ins_end: ~a~n"(ins_end '(1 2) '(3 4 5 6)))

(printf "cout_top_level: ~a~n" (cout_top_level '(1 2 3 4 5)))

(printf "count_instances: ~a~n" (count_instances '1 '(1 1 2 3 1 2)))

(printf "count_instances_deep: ~a~n" (count_instances_deep '1 '((1 2 3) (1 2) 1 (2 3 4))))




 

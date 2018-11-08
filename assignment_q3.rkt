#lang racket

(provide tree)
(provide to_Sort)
(provide left_child)
(provide right_child)
(provide sort)
(provide search)
(provide insert_tree)
(provide add)
(provide higher_order_add)
(provide tree_sort)
(provide higher_order_sort)
(provide higher_order_add_item)

(define tree '(((() 2 ()) 3 (() 15 ())) 26 ((() 29 ()) 34 (() 49 ()))))
(define to_Sort '(41 2 17 30 15 24 46 14))

(define (left_child tree)
  (car tree))

(define (right_child tree)
  (caddr tree))

;Question A
(define (sort tree)
  (begin(cond [(not (null?(left_child tree))) (sort (left_child tree))])
        (cond [(not (null?(right_child tree))) (sort (right_child tree))])
        (printf "~a " (cadr tree))))

;Question B
(define (search el tree)
  (cond
    [(null? tree) #f]
    [(equal? el (cadr tree)) #t]
    [(< el (cadr tree)) (search el (left_child tree))]
    [else (search el (right_child tree))]))

;Question C
(define (insert_tree el tree)
  (cond [(empty? tree) (list '() el '())]
        [(equal? el (cadr tree)) tree]
;        [(< el (cadr tree))
         [(list ( insert_tree el (left_child tree)) (cadr tree) (right_child tree))]
        [else (list (left_child tree) (caddr tree) (insert_tree el (right_child tree)))]))

;Question D
(define (add lst tree)
  (if (empty? lst) tree
      (add (cdr lst) (insert_tree (car lst) tree))))

(define (higher_order_add lst tree left)
  (if (null? lst) tree
      (higher_order_add (cdr lst) (higher_order_add_item (car lst) tree left) left)))

;Question E
(define (tree_sort lst)
  (sort (add lst '())))


;Question F
(define (higher_order_sort lst orderFunc)
  (sort (higher_order_add lst '() orderFunc)))

(define (higher_order_add_item item tree left)
  (cond [(null? tree) (list '() item '())]
        [(equal? item (cadr tree)) tree]
        [(left item (cadr tree))
         (list (higher_order_add_item item (left_child tree) left) (cadr tree) (right_child tree))]
        [else (list (left_child tree) (cadr tree) (higher_order_add_item item (right_child tree) left))]))


(define (ascending a b)
  (< (remainder a 10)(remainder b 10)))

(sort tree)
(display "\n")
(search 21 tree)
(display "\n")
(search 2 tree)
(display "\n")
(insert_tree 12 tree);inserts 12 to tree
(display "\n")
(add '(3 4 99 54 30) tree)
(display "\n")
(tree_sort to_Sort)
(display "\n")
(higher_order_sort to_Sort <)
(display "\n")
(higher_order_sort to_Sort >)
(display "\n")
(higher_order_sort to_Sort ascending)

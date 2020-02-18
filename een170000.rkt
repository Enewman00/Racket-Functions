#lang racket


;;Define your own Racket function that takes an integer as an argument and returns a function that
;;indicates whether its integer argument is evenly divisible by the first.
;;Input: An integer.
;;Output: A function. 
(define divisible-by-x?
  (λ (x)
    ;;return a function
    (λ (n) (integer? (/ n x)))))

;;Define a function that takes a function as an argument and passes the number 9 to that function. The
;;function argument must be able to accept a single integer as its argument.
;;Input: A named function which takes a single number as an argument.
;;Output: The value returned by applying the named function to the number 9.
(define function-9
  (λ (func) (func 9)))

;;Define your own Racket function that duplicates the the functionality of map from the standard library.
;;Input: The input to my-map is a function that takes a function and a homogeneous list of elements of
;;the same data type compatible with the function. Note: the function argument can be named or
;;anonymous (lambda).
;;Output: A new list of the original elements with the same procedure applied to each.
(define my-map
  (λ (func list)
    (my-map-helper func list null)))
(define my-map-helper
  (λ (func list acc)
    (if (null? list)
        acc
        (my-map-helper func (cdr list)
            (append acc (cons (func (car list)) '()))))))



;;Define a function takes two lists as arguments and returns a single list of pairs (i.e. two element sublists).
;;The the first pair should be the both first elements from the respective lists. The second pairs should be
;;the second elements from the respective lists, and so on. If one input list is longer than the other, extra
;;elements of the longer list are ignored. Your implementation must be recursive.
;;Input: Two lists of elements of any type, potentially heterogenous. The two lists do not have to be the
;;same length.
;;Output: A new list whose elements are each two-element sublists. The first sublist is composed of the
;;first elements from two input lists respectively, the second sublist is composed of the second elements
;;form the two input lists respectively, etc. If one list is longer than the other, extra elements of the
;;longer list are ignored. 
(define pair-up
  (λ (list1 list2)
    (pair-up-helper list1 list2 null)))
(define pair-up-helper
  (λ (list1 list2 acc)
    (if (null? list1)
        acc
        (if (null? list2)
            acc
            (pair-up-helper (cdr list1) (cdr list2) (append acc (cons (cons (car list1) (cons (car list2) '())) '())))))))
            



;;Define a function that takes a procedure that executes a Boolean test on an atomic value and a list of
;;elements as arguments. It should returns a list containing exactly two sublists. You may not use the builtin filter function as a helper function. You may define your own helper functions. Your
;;implementation must be recursive.
;;Input: Two arguments... (1) a function that takes a single element and returns a Boolean, and (2) a
;;list of elements whose element types are compatible with the single element from the first argument.
;;Output: A new list with two sublists. The first sublist contains the elements from the original list that
;;return true (#t) and the second sublist contains the elements from the original list that return false(#f). 
;;(define classify
;;  (λ (func list)
;;    (classify-helper func list null null)))
;;(define classify-helper
;;  (λ (func list sub1 sub2)
;;    (if (null? list)
;;        (cons









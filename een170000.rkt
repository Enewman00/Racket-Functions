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
  (λ (func lst)
    (my-map-helper func lst null)))
(define my-map-helper
  (λ (func lst acc)
    (if (null? lst)
        acc
        (my-map-helper func (cdr lst)
            (append acc (list (func (car lst))))))))



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
            (pair-up-helper (cdr list1) (cdr list2) (append acc (list (append (list (car list1)) (list (car list2))))))))))
            



;;Define a function that takes a procedure that executes a Boolean test on an atomic value and a list of
;;elements as arguments. It should returns a list containing exactly two sublists. You may not use the builtin filter function as a helper function. You may define your own helper functions. Your
;;implementation must be recursive.
;;Input: Two arguments... (1) a function that takes a single element and returns a Boolean, and (2) a
;;list of elements whose element types are compatible with the single element from the first argument.
;;Output: A new list with two sublists. The first sublist contains the elements from the original list that
;;return true (#t) and the second sublist contains the elements from the original list that return false(#f). 
(define classify
  (λ (func lst)
    (classify-helper func lst null null)))
(define classify-helper
  (λ (func lst sub1 sub2)
    (if (null? lst)
        (append (list sub1) (list sub2))
        (if (func (car lst))  ;;if func applied to item is true
            (classify-helper func (cdr lst) (append sub1 (list (car lst))) sub2) ;;then recurse with appended to sub1
            (classify-helper func (cdr lst) sub1 (append sub2 (list (car lst)))))))) ;;then recurse with appended to sub2




;;Define a function that takes two arguments, a list and an expression, which may be atomic or a list. Your
;;function should return true (#t) if the element is a member of the list and false (#f) if it does not. You
;;may not use the built-in member or element functions as helper functions. Your implementation must
;;be recursive.
;;Input: A single item of any data type and a (potentially heterogenous) list of elements of any data type.
;;Output: A boolean value that indicates whether the input item is a member of the input list.
(define is-member?
  (λ (item lst)
    (if (null? lst)
        #f
        (if (equal? item (car lst))  ;;if item is equal to head of the list given
            #t                     ;;return true
            (is-member? item (cdr lst))))))


;;Define a function that takes two arguments—a comparison function and a list. It should return a boolean
;;(i.e. #t or #f) indicating whether the list is sorted according to the comparison function. You may not use
;;the built-in sorted? function. Your implementation must be recursive.
;;Input: A comparison function and a list of elements whose values are compatible with the
;;comparison function.
;;Output: A boolean value that indicates whether the elements of the list are sorted according to the
;;comparison function.
(define my-sorted?
  (λ (func lst)
    (if (equal? 1 (length lst))
        #t
        (if (func (car lst) (cadr lst))
            (my-sorted? func (cdr lst))
            #f))))


;;Define your own Racket function that duplicates the the functionality of flatten from the standard
;;library. You may not use the built-in flatten function as a helper function. It should take a list
;;containing zero or more sublists as an argument. Each sublist may contain an arbitrary level of nesting. It
;;should return a single list containing all of the items from all nested levels with no sublists. You may not
;;use the built-in flatten function as a helper function. Your implementation must be recursive.
;;Input: A single list which may contain an arbitrary number of elements and sublists, each sublists
;;may also contain an arbitrary number of elements and sublists, nested to an any depth.
;;Output: A new single-level list which contains all of the atomic elements from the input list. 
(define my-flatten
  (λ (lst)
    (my-flatten-helper lst null)))
(define my-flatten-helper
  (λ (lst acc)
    (if (null? lst)
        acc
        (if (list? (car lst))             ;;if head is a list,_____, else append to acc
            (my-flatten-helper (append (car lst) (cdr lst)) acc)
            (my-flatten-helper (cdr lst) (append acc (list (car lst))))))))





;;Define a function that takes two arguments, a list of numbers and a single number (the threshold). It
;;should return a new list that has the same numbers as the input list, but with all elements greater than the
;;threshold number removed. You may not use the built-in filter function as a helper function. Your
;;implementation must be recursive.
;;Input: A list of numbers and a single atomic number.
;;Output: A new list of numbers that contains only the numbers from the original list that are strictly
;;“less than” (<), i.e. below the threshold number. 
(define upper-threshold
  (λ (lst num)
    (upper-threshold-helper lst num null)))
(define upper-threshold-helper
  (λ (lst num acc)
    (if (null? lst)
        acc
        (if (< (car lst) num)
            (upper-threshold-helper (cdr lst) num (append acc (list (car lst))))
            (upper-threshold-helper (cdr lst) num acc)))))



;;Define your own Racket function that duplicates the the functionality of list-ref from the standard
;;library. You may not use the built-in list-ref function as a helper function.
;;Define a function that takes a list and an integer. The function should return the list element at the integer
;;number (first list position is index “0”). If the integer is larger than the index of the last list member, it
;;should display an “index out of bounds” message. Your implementation must be recursive.
;;Input: A list of elements of any data type, potentially heterogenous, and a single integer.
;;Output: A single element from the original list that is at the “index” indicated by the integer. The first
;;list position is position “0”, the second list position is “1”, etc. If the integer is greater than the
;;number of list elements, the function should throw an error (using the error function) with the
;;message string “ERROR: Index out of bounds”. 
(define my-list-ref
  (λ (lst num)
    (cond
      [(null? lst) (error "ERROR: Index out of bounds")]
      [(equal? num 0) (car lst)]
      [else (my-list-ref (cdr lst) (- num 1))])))





;;Define a function similar to the built-in reverse function, except that it acts recursively, reversing the
;;order the members of any nested sublists. You may not use the built-in reverse function as a helper
;;function. However, you may use your own my-reverse function as a helper function.
;;Input: A single list which may contain an arbitrary number of elements and sublists, each sublists
;;may also contain an arbitrary number of elements and sublists, nested to an any depth.
;;Output: A new list which contains all elements in reverse order, as well as recursively reverse order
;;all members of sublists. 
(define deep-reverse
  (λ (lst)
    (deep-reverse-helper lst null)))
(define deep-reverse-helper
  (λ (lst acc)
    (if (null? lst)
        acc
        (if (list? (car lst))
            (deep-reverse-helper (cdr lst) (append (list (deep-reverse-helper (car lst) null)) acc))
            (deep-reverse-helper (cdr lst) (append (list (car lst)) acc))))))
         
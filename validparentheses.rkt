#lang racket
(require test-engine/racket-tests)

;; valid-parentheses? : String -> Boolean
;; Determines whether a given string has valid parentheses. All parentheses (, [, {, must be closed
;; with their respective counterparts ), ], }, in the order that they are opened. Examples:
;; () - valid (} - invalid ({[]}) - valid [{[}]] - invalid
(define (valid-parentheses? str)
  (valid-p-helper? str '()))

;; valid-p-helper? : String [List-of String] -> Boolean
;; Helps to determine whether a given string has valid parentheses, based on a given list of
;; already opened parentheses. 
(define (valid-p-helper? str open)
  (cond
    [(= (string-length str) 0) (empty? open)]
    [(is-open? (substring str 0 1)) (valid-p-helper? (substring str 1) (cons (substring str 0 1) open))]
    [(is-closed? (substring str 0 1)) (cond
                                        [(cons? open) (and (match? (first open) (substring str 0 1)) (valid-p-helper? (substring str 1) (rest open)))]
                                        [(empty? open) #f])]
    [else #f]))

;; is-open? : String -> Boolean
;; Determines whether a given string is an open parentheses, exclusively one of (, {, [.
(define (is-open? str)
  (or (string=? str "(") (string=? str "[") (string=? str "{")))

;; is-closed? : String -> Boolean
;; Determines whether a given string is a closed parentheses, exclusively one of ), }, ].
(define (is-closed? str)
  (or (string=? str ")") (string=? str "]") (string=? str "}")))

;; match? : String -> Boolean
;; Determines a given string _str2_ is the closing parentheses that matches with the open parentheses _str1_.
(define (match? str1 str2)
  (cond
    [(string=? str1 "(") (string=? str2 ")")]
    [(string=? str1 "[") (string=? str2 "]")]
    [(string=? str1 "{") (string=? str2 "}")]
    [else #f]))

(check-expect (valid-parentheses? "{[((()))]}") #true)
(check-expect (valid-parentheses? "[][][]{()}") #true)
(check-expect (valid-parentheses? "()[{()}]{}") #true)
(check-expect (valid-parentheses? "({})[]") #true)
(check-expect (valid-parentheses? "") #true)
(check-expect (valid-parentheses? "()") #true)
(check-expect (valid-parentheses? "({)}") #false)
(check-expect (valid-parentheses? "(()") #false)
(check-expect (valid-parentheses? "(}") #false)
(check-expect (valid-parentheses? "[]]") #false)
(check-expect (valid-parentheses? "[a]") #false)


(test)
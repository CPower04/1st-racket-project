#lang racket
;;https://youtu.be/hcj9of_5c4Q
;; List for mapping decimal to hexadecimal
(define hex '(0 1 2 3 4 5 6 7 8 9 A B C D E F))

;; Get the remainder of x / y, modulus function 
(define (mod x y)
  (if (< (- x y) y)
      (if (< x y)
          x
          (mod (- x y) y))
      (mod (- x y) y)))

;; This function retrieves the hexadecimal value corresponding to the decimal digit n (0-15) from the hex list.
(define (getHexValue x n)
  (if (zero? n)
      (car x)
      (getHexValue (cdr x) (- n 1))))

;; This function converts a decimal number n into a list of hexadecimal digits. It uses recursion to build the list.
(define (getListOfHexVals n)
  (if (zero? (quotient n 16))
      (list (getHexValue hex (mod n 16)))
      (append (list (getHexValue hex (mod n 16)))
              (getListOfHexVals (quotient n 16)))))

;; Convert decimal to hexadecimal checks if the input is an integer , then converts to hex- digits and joins them into a string
(define (decToHex n)
  (if (integer? n)
      (string-join (reverse (map ~s (getListOfHexVals n))) "")
      "invalid input"))

;; Test Data
(displayln (decToHex 16))
(displayln (decToHex 10))
(displayln (decToHex 8))
(displayln (decToHex 6))
(displayln (decToHex 28))

;; Test List Data
(displayln (map decToHex '(16 129 48 11 28)))
(displayln (map decToHex '(256 0 5 42 90)))
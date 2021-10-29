;;; numbers

123    ; integer
-123   ; integer
10/2   ; ratio => 5
5/2    ; ratio
#B1010 ; binary
#Xfff  ; hexadecimal
#O770  ; octal

;; s, f, d, l to specific the float type
123d2 ;; double precision, d2 = 10^2 with double precision
12e2  ;; scientific notation, default to s
23.45e10

(/ 4 3) ;; doesn't truncate, creates a rational

(floor (/ 4 3)) ; => 1
(truncate (/ 4 3)) ; => 1
(ceiling (/ 4 3)) ; => 2
(round (/ 4 3)) ; => 1

(/= 1 3) ; => T

(zerop 0.0)

;;; character are not number, so we can't use the operation for number

(char= #\a #\a) ; => T
(char= #\a #\A) ; => NIL
;; case insensitive
(char-equal #\a #\A) ; => T

(char> #\d #\a) ; => T
;; case insensitive
(char-greaterp #\D #\a) ; => T

;; special characters
#\space
#\newline
#\tab

;;; STRING are composed by a sequence of characters. The operations
;;; follow the same naming convention for chars.

;; comparisions accept 4 keyboard args to decide where to start and end
(string= "foobarbaz" "quuxbarfoo" :start1 3 :end1 6 :start2 4 :end2 7)

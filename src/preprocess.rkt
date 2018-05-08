;; MIT License
;;
;; Copyright (c) 2018 Francisca Cambra, Rui Ventura
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
#lang racket
; pre-defined actions
(require "actions.rkt")
; provisions
(provide
 ; macros
 def-active-token
 ;functions
 add-active-token process-string)

; Takes an active token, a parameter list, and a body and that expands into an
; appropriate use of the function `add-active-token`.
(define-syntax-rule
  (def-active-token token (str) body)
  (add-active-token token (λ (str) body)))

; Hash holding registered active tokens and respective actions.
(define active-tokens
  (make-hash (list (cons ";;" string-after-newline)
                   (cons "//eval " eval-string)
                   (cons "var" infer-java-type)
                   (cons "#\"" interpolate)
                   (cons "alias" type-alias))))

; Takes an active token and a function that should be triggered when that
; token is found. This last function takes a `string` and returns a `string`
; as a result. `add-active-token` stores the association between the token
; and the corresponding function.
(define/contract (add-active-token token action)
  [string? (string? . -> . string?) . -> . any]
  (hash-set! active-tokens token action)
  (printf "Action bound to ~s\n" token))

; Responsible for triggering the active token actions, transforming the
; string until no more active tokens are found. To this end, whenever an
; active token is found, th associated function is called with the substring
; that starts immediately after the token, and the result of that function
; replaces the substring that starts with the token.
(define/contract (process-string str)
  [string? . -> . string?]
  (define last 0)
  (define rx (regexp (string-join (hash-keys active-tokens) "|")))
  (do ([pos (regexp-match-positions rx str)
            (regexp-match-positions rx str last)])
    ([false? pos] str)
    (match pos
      [(list (cons start end))
       (define action (hash-ref active-tokens (substring str start end)))
       (define after (substring str end))
       (define result (action after))
       (set! last start)
       ; if the string is the same after transform, skip it
       (or (and (equal? result after)
                (set! last (add1 last)))
           ; else, replace it
           (set! str (string-replace str (substring str start) result)))])))

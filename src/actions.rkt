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
(provide (all-defined-out))

; Attempts to evaluate a given string, returning the evaluation's result as a
; string.
(define/contract (eval-string str)
  [string? . -> . string?]
  (call-with-input-string
   str
   (λ (in)
     (string-append (~a (eval (read in) (make-base-namespace)))
                    (port->string in)))))

; Takes a string that's a path to a file and inlines it in the file it's being
; included in.
(define/contract (include-macro str)
  [string? . -> . string?]
  (match (regexp-match #px"^\\s+\"(.*?[^\\\\])\"" str)
    [(list all file)
     (regexp-replace all str (file->string file))]
    [else (error "Could not find file to be included")]))

; Replaces interpolated constructs with concatenations of the expressions in
; said constructs.
; E.g, `#{a} + #{b} = #{a + b}"`
;   -> `" + (a) + " + " + b + " = " + (a + b) + ""`.
(define/contract (string-interpolation str)
  [string? . -> . string?]
  (match (regexp-match-positions #rx"[^\\]\"" str)
    [(list (cons _ end))
     (string-append "\"" (regexp-replace* #rx"#{(.*?)}" str
                      "\" + (\\1) + \"" 0 end))]
    [else (string-append "\"" str)]))

; Ignores every character in the string up until the first newline character,
; returning the rest, if it exists.
(define/contract (string-after-newline str)
  [string? . -> . string?]
  (match (regexp-match-positions "\n" str)
    [(list (cons start end)) (substring str end)]
    [else ""]))

; Takes an alias assignment and replaces every occurrence of the alias with the
; corresponding right value.
(define/contract (type-alias str)
  [string? . -> . string?]
  (match (regexp-match #px"^\\s+([\\w$]+)\\s*=\\s*(.*?);" str)
    [(list all alias value)
     (regexp-replace* (pregexp (string-append "\\b" alias "\\b"))
                      (substring str (string-length all)) value)]
    [else str]))

; Takes an assignment to an (expectedly) simple `new` Java expression and return
; it pre-prended with the type of the POJO being created.
(define/contract (type-inference str)
  [string? . -> . string?]
  (match (regexp-match (pregexp "^\\s+[\\w$]+\\s*=\\s*new\\s+(.*?)\\(") str)
    [(list _ type)
     (string-append type str)]
    [else str]))
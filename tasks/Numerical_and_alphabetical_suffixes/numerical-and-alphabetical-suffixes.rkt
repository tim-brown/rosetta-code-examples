#lang racket

(require parser-tools/lex (prefix-in : parser-tools/lex-sre))

(module+ test)

(define power-letters '(#\k #\m #\g #\t #\p #\e #\z #\y #\x #\w #\v #\u))
(define (char->metric-power c) (expt 10 (* 3 (add1 (index-of power-letters c)))))
(define (char->binary-power c) (expt 2 (* 10 (add1 (index-of power-letters c)))))

(define-lex-abbrevs
  [s? (:? #\s)]
  [PAIRs  (:: "pair" s?)]
  [DOZens (:: "doz" (:? (:: #\e (:? (:: #\n s?)))))]
  [SCOres (:: "sco" (:? (:: #\r (:? (:: #\e s?)))))]
  [GRoss  (:: "gr" (:? (:: #\o (:** 0 2 s?))))]
  [GREATGRoss (:: "great" GRoss)]
  [GOOGOLs (:: "googol" s?)]
  [metric-power  '(#\k #\m #\g #\t #\p #\e #\z #\y #\x #\w #\v #\u)]
  [binary-power ])

(define suffix->function-lexer
  (let ((multiplier (λ (m p) (compose (λ (x) (* x m)) (suffix->function-lexer p)))))
    (lexer
     [PAIRs (multiplier 2 input-port)]
     [DOZens (multiplier 12 input-port)]
     [SCOres (multiplier 20 input-port)]
     [GRoss (multiplier 144 input-port)]
     [GREATGRoss (multiplier 1728 input-port)]
     [GOOGOLs (multiplier (expt 10 100) input-port)]
     [(:&) (λ (x) (eprintf "need to apply: ~s to ~a" lexeme x) x)]
     [(eof) (λ (x) x)])))

(define (suffix->function s)
  (call-with-input-string s suffix->function-lexer))

(define (parse-number n)
  (match (string-downcase (regexp-replace* #px"," n ""))
    [(pregexp #px"([+-]?[0-9]*(?:\\.[0-9]*)?(?:[eE][+-]?[1-9][0-9]*)?)(.*)" (list _ (app string->number x) (and suffix (app suffix->function rest))))
     (displayln (list x suffix (rest x)) (current-error-port))
     (rest x)]))

{module+ test
  #|
Abbreviated alphabetic suffixes to be supported   (where the capital letters signify the minimum abbreation that can be used)
     GRoss         multiply the number by  144       (twelve dozen)
     GREATGRoss    multiply the number by  1,728     (a dozen gross)
     GOOGOLs       multiply the number by  10^100    (ten raised to the 100&sup>th</sup> power)

Note that the plurals are supported, even though they're usually used when expressing exact numbers   (She has 2 dozen eggs, and dozens of quavas)


Metric suffixes to be supported   (whether or not they're officially sanctioned)
     K     multiply the number by  10^3              kilo      (1,000)
     M     multiply the number by  10^6              mega      (1,000,000)
     G     multiply the number by  10^9              giga      (1,000,000,000)
     T     multiply the number by  10^12             tera      (1,000,000,000,000)
     P     multiply the number by  10^15             peta      (1,000,000,000,000,000)
     E     multiply the number by  10^18             exa       (1,000,000,000,000,000,000)
     Z     multiply the number by  10^21             zetta     (1,000,000,000,000,000,000,000)
     Y     multiply the number by  10^24             yotta     (1,000,000,000,000,000,000,000,000)
     X     multiply the number by  10^27             xenta     (1,000,000,000,000,000,000,000,000,000)
     W     multiply the number by  10^30             wekta     (1,000,000,000,000,000,000,000,000,000,000)
     V     multiply the number by  10^33             vendeka   (1,000,000,000,000,000,000,000,000,000,000,000)
     U     multiply the number by  10^36             udekta    (1,000,000,000,000,000,000,000,000,000,000,000,000)

Binary suffixes to be supported   (whether or not they're officially sanctioned)
     Ki    multiply the number by  2^10              kibi      (1,024)
     Mi    multiply the number by  2^20              mebi      (1,048,576)
     Gi    multiply the number by  2^30              gibi      (1,073,741,824)
     Ti    multiply the number by  2^40              tebi      (1,099,571,627,776)
     Pi    multiply the number by  2^50              pebi      (1,125,899,906,884,629)
     Ei    multiply the number by  2^60              exbi      (1,152,921,504,606,846,976)
     Zi    multiply the number by  2^70              zeb1      (1,180,591,620,717,411,303,424)
     Yi    multiply the number by  2^80              yobi      (1,208,925,819,614,629,174,706,176)
     Xi    multiply the number by  2^90              xebi      (1,237,940,039,285,380,274,899,124,224)
     Wi    multiply the number by  2^100             webi      (1,267,650,600,228,229,401,496,703,205,376)
     Vi    multiply the number by  2^110             vebi      (1,298,074,214,633,706,907,132,624,082,305,024)
     Ui    multiply the number by  2^120             uebi      (1,329,227,995,784,915,872,903,807,060,280,344,576)

All of the metric and binary suffixes can be expressed in   lowercase,   uppercase,   or   mixed case.

All of the metric and binary suffixes can be   stacked   (expressed multiple times),   and also be intermixed: 
I.E.:       123k   123K   123GKi   12.3GiGG   12.3e-7T   .78E100e


Factorial suffixes to be supported
     !      compute the (regular) factorial product:   5!   is  5 × 4 × 3 × 2 × 1  =  120
     !!     compute the  double   factorial product:   8!   is  8 × 6 × 4 × 2      =  384
     !!!    compute the  triple   factorial product:   8!   is  8 × 5 × 2          =   80
     !!!!   compute the quadruple factorial product:   8!   is  8 × 4              =   32
     !!!!!  compute the quintuple factorial product:   8!   is  8 × 3              =   24
     ··· the number of factorial symbols that can be specified is to be unlimited   (as per what can be entered/typed) ···

Note that these factorial products aren't   super─factorials   where (4!)! would be (24)!.

Factorial suffixes aren't, of course, the usual type of multipliers, but are used here in a similar vein.


Multifactorials aren't to be confused with   super─factorials     where   (4!)!   would be   (24)!.


Task
  Using the test cases (below),   show the "expanded" numbers here, on this page.
  For each list, show the input on one line,   and also show the output on one line.
  When showing the input line, keep the spaces (whitespace) and case (capitalizations) as is.
  For each result (list) displayed on one line, separate each number with two blanks.
  Add commas to the output numbers were appropriate.

Test cases
    
|#
 (require rackunit syntax/parse/define)
  (define-syntax check
    (syntax-rules ()
        [(_) (void)]
        [(_ (s n) more ...)
         (begin
             (check-equal? (parse-number s) n)
             (check more ...))]
      [(_ s more ...)
         (begin
             (printf "~a -> ~s~%" s (parse-number s))
             (check more ...))]))

  (check ("2greatGRo" (* 2 12 144))
         ("24Gros" (* 24 144))
         ("288Doz" (* 288 12))
         ("1,728pairs" (* 1728 2))
         ("172.8SCOre" (* 172.8 20))
         ("1,567" 1567)
         "+1.567k"
         "0.1567e-2m"
         "25.123kK"
         "25.123m"
         "2.5123e-00002G"
         "25.123kiKI"
         "25.123Mi"
         "2.5123e-00002Gi"
         "+.25123E-7Ei"
         "-.25123e-34Vikki"
         ("2e-77gooGols" 2e23)
         "9!"
         "9!!"
         "9!!!"
         "9!!!!"
         "9!!!!!"
         "9!!!!!!"
         "9!!!!!!!"
         "9!!!!!!!!"
         "9!!!!!!!!!")
  }

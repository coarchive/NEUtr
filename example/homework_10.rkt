#lang racket
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |homework 10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; to run: (main rows probability)

;----------------------------------------------------------------------------------------------------;
;
;                                ;;;;                       ;;;;;
;                               ;    ;                      ;
;    ;;;   ;   ;                     ;                      ;
;   ;;  ;   ; ;                      ;                      ;;;;;
;   ;   ;;  ;;;                     ;                           ;;
;   ;;;;;;   ;                     ;         ;;;;;;;             ;
;   ;       ;;;                   ;                              ;
;   ;       ; ;    ;;            ;                          ;   ;;
;    ;;;;  ;   ;   ;;           ;;;;;;                       ;;;;
;
;
;----------------------------------------------------------------------------------------------------;

; An Atom is one of
; - Number
; - String
; - Boolean
; - Symbol
(define ex-atom-1 72)
(define ex-atom-2 "hello there")
(define ex-atom-3 #true)
(define ex-atom-4 '+)
(define ex-atom-5 #false)
; Template:
; atom-temp : Atom -> ???
#;(define (atom-temp a)
    (cond
      [(number? a) ...]
      [(string? a) ...]
      [(boolean? a) ...]
      [(symbol? a) ...]))

; atom? : Any -> Boolean
(define (atom? x)
  (or (number? x)
      (string? x)
      (boolean? x)
      (symbol? x)))

; An SExpr is one of
; - Atom
; - ListOfSExpr
(define ex-sexpr-1 ex-atom-1)
(define ex-sexpr-2 ex-atom-4)
(define ex-sexpr-3 (list ex-sexpr-1 ex-sexpr-2))
(define ex-sexpr-4 (list ex-sexpr-3 ex-sexpr-1 (list ex-atom-2 ex-atom-3)))
(define ex-sexpr-5 '())
; Template:
; SExpr-temp : SExpr -> ???
#;(define (sexpr-temp s)
    (cond
      [(atom? s) (... (atom-temp s) ...)]
      [(list? s) (... (losexpr-temp s) ...)]))

; A ListOfSExpr is one of
; - '()
; - (cons SExpr ListOfSExpr)
(define ex-losexpr-1 '())
(define ex-losexpr-2 (cons ex-sexpr-1 '()))
(define ex-losexpr-3 (cons ex-sexpr-4 ex-losexpr-2))
; Template:
; losexpr-temp : ListOfSExpr -> ???
#;(define (losexpr-temp los)
    (cond
      [(empty? los) ...]
      [(cons? los) (... (sexpr-temp (first los)) ...
                        (losexpr-temp (rest los)) ...)]))


; EXERCISE 2

; print-sexpr : SExpr -> String
; produces a string representing how the SExpr would be written in ISL code
(check-expect (print-sexpr ex-sexpr-1) "72")
(check-expect (print-sexpr ex-sexpr-2) "'+")
(check-expect (print-sexpr ex-sexpr-3) "(list 72 '+)")
(check-expect (print-sexpr ex-sexpr-4) "(list (list 72 '+) 72 (list \"hello there\" #true))")
(check-expect (print-sexpr ex-sexpr-5) "(list)")
(define (print-sexpr s)
  (cond
    [(atom? s) (print-atom s)]
    [(list? s) (string-append "(list" (print-losexpr s) ")")]))

; print-atom : Atom -> String
; produces a string representing how the Atom would be written in ISL code
(check-expect (print-atom ex-atom-1) "72")
(check-expect (print-atom ex-atom-2) "\"hello there\"")
(check-expect (print-atom ex-atom-3) "#true")
(check-expect (print-atom ex-atom-4) "'+")
(check-expect (print-atom ex-atom-5) "#false")
(define (print-atom a)
  (cond
    [(number? a) (number->string a)]
    [(string? a) (string-append "\"" a "\"")]
    [(boolean? a) (if a "#true" "#false")]
    [(symbol? a) (string-append "'" (symbol->string a))]))

; print-losexpr : LOSExpr -> String
; produces a string representing how the LOSExpr would be written in ISL code
(check-expect (print-losexpr ex-losexpr-1) "")
(check-expect (print-losexpr ex-losexpr-2) " 72")
(check-expect (print-losexpr ex-losexpr-3)
              " (list (list 72 '+) 72 (list \"hello there\" #true)) 72")
(define (print-losexpr los)
  (foldr (λ (sexpr base) (string-append " " (print-sexpr sexpr) base)) "" los))


; EXERCISE 3

; A [Wide-tree X] is one of
; - X
; - [List-of [Wide-tree X]]

(define f1 (λ (x) (* 2 x)))
(define f2 (λ (x) (string-length x)))
(define f3 (λ (x) (if x 5 2)))
(define f4 (λ (x) -3))

; sexpr-map : {X} SExpr [Number -> X] [String -> X] [Boolean -> X] [Symbol -> X] -> [Wide-tree X]
; replaces each Atom in SExpr according to input functions
(check-expect (sexpr-map ex-sexpr-1 f1 f2 f3 f4) 144)
(check-expect (sexpr-map ex-sexpr-2 f1 f2 f3 f4) -3)
(check-expect (sexpr-map ex-sexpr-3 f1 f2 f3 f4) (list 144 -3))
(check-expect (sexpr-map ex-sexpr-4 f1 f2 f3 f4) (list (list 144 -3) 144 (list 11 5)))
(check-expect (sexpr-map ex-sexpr-5 f1 f2 f3 f4) '())
(define (sexpr-map s num->x str->x bool->x sym->x)
  (cond
    [(atom? s) (atom-map s num->x str->x bool->x sym->x)]
    [(list? s) (losexpr-map s num->x str->x bool->x sym->x)]))

; atom-map : {X} Atom [Number -> X] [String -> X] [Boolean -> X] [Symbol -> X] -> X
; manipulates Atom using 4 input functions, dependin on Atom's type
(check-expect (atom-map ex-atom-1 f1 f2 f3 f4) 144)
(check-expect (atom-map ex-atom-2 f1 f2 f3 f4) 11)
(check-expect (atom-map ex-atom-3 f1 f2 f3 f4) 5)
(check-expect (atom-map ex-atom-4 f1 f2 f3 f4) -3)
(check-expect (atom-map ex-atom-5 f1 f2 f3 f4) 2)
(define (atom-map a num->x str->x bool->x sym->x)
  (cond
    [(number? a) (num->x a)]
    [(string? a) (str->x a)]
    [(boolean? a) (bool->x a)]
    [(symbol? a) (sym->x a)]))

; losexpr-map : {X} LOSExpr [Number -> X] [String -> X] [Boolean -> X] [Symbol -> X] ->
;               [List-of [Wide-tree X]]
; replaces each atom in a LOSExpr according to input functions
(check-expect (losexpr-map ex-losexpr-1 f1 f2 f3 f4) '())
(check-expect (losexpr-map ex-losexpr-2 f1 f2 f3 f4) (list 144))
(check-expect (losexpr-map ex-losexpr-3 f1 f2 f3 f4) (list (list (list 144 -3) 144 (list 11 5)) 144))
(define (losexpr-map los num->x str->x bool->x sym->x)
  (map (λ (sexpr) (sexpr-map sexpr num->x str->x bool->x sym->x)) los))


; EXERCISE 4

; all-numbers? : SExpr -> Boolean
; are all Atoms in an SExpr numbers?
(check-expect (all-numbers? ex-sexpr-1) #true)
(check-expect (all-numbers? ex-sexpr-2) #false)
(check-expect (all-numbers? '(72 3 (-7 2.4) (2 (-100 123)) 5)) #true)
(check-expect (all-numbers? ex-sexpr-4) #false)
(check-expect (all-numbers? ex-sexpr-5) #true)
(define (all-numbers? s)
  (cond
    [(atom? s) (all-numbers?/atom s)]
    [(list? s) (all-numbers?/los s)]))

; all-numbers?/atom : Atom -> Boolean
; is the atom a number?
(check-expect (all-numbers?/atom ex-atom-1) #true)
(check-expect (all-numbers?/atom ex-atom-2) #false)
(check-expect (all-numbers?/atom ex-atom-3) #false)
(check-expect (all-numbers?/atom ex-atom-4) #false)
(define (all-numbers?/atom a)
  (cond
    [(number? a) #true]
    [(string? a) #false]
    [(boolean? a) #false]
    [(symbol? a) #false]))

; all-numbers?/los : LOSExpr -> Boolean
; are all Atoms in a LOSExpr numbers? this functions returns #true for an empty list,
; since it has no non-number atoms in it
(check-expect (all-numbers?/los ex-losexpr-1) #true)
(check-expect (all-numbers?/los ex-losexpr-2) #true)
(check-expect (all-numbers?/los ex-losexpr-3) #false)
(define (all-numbers?/los los)
  (andmap all-numbers? los))


; EXERCISE 5

; all-hi? : SExpr -> Boolean
; are all Atoms in an SExpr the string "hi"?
(check-expect (all-hi? ex-sexpr-2) #false)
(check-expect (all-hi? "hi") #true)
(check-expect (all-hi? '("hi" "hi" ("hi" "hi") ("hi" ("hi" "hi")) "hi")) #true)
(check-expect (all-hi? ex-sexpr-4) #false)
(check-expect (all-hi? ex-sexpr-5) #true)
(define (all-hi? s)
  (cond
    [(atom? s) (all-hi?/atom s)]
    [(list? s) (all-hi?/los s)]))

; all-hi?/atom : Atom -> Boolean
; is the Atom the string "hi"?
(check-expect (all-hi?/atom ex-atom-1) #false)
(check-expect (all-hi?/atom "hi") #true)
(check-expect (all-hi?/atom ex-atom-3) #false)
(check-expect (all-hi?/atom ex-atom-4) #false)
(define (all-hi?/atom a)
  (cond
    [(number? a) #false]
    [(string? a) (string=? a "hi")]
    [(boolean? a) #false]
    [(symbol? a) #false]))

; all-hi?/los : LOSExpr -> Boolean
; are all Atoms in a LOSExpr the string "hi"? this functions returns #true for an empty list,
; since it has no non-"hi" atoms in it
(check-expect (all-hi?/los ex-losexpr-1) #true)
(check-expect (all-hi?/los (list "hi")) #true)
(check-expect (all-hi?/los (list "hi" (list 17 #true))) #false)
(define (all-hi?/los los)
  (andmap all-hi? los))

; all-x? : SExpr [Atom -> Boolean] -> Boolean
; do all Atoms in SExpr return #true with the input predicate?
(check-expect (all-x? ex-sexpr-1 all-numbers?/atom) #true)
(check-expect (all-x? "hi" all-hi?/atom) #true)
(check-expect (all-x? ex-sexpr-4 all-numbers?/atom) #false)
(check-expect (all-x? '("hi" "hi" ("hi" "hi") ("hi" ("hi" "hi")) "hi") all-hi?/atom) #true)
(define (all-x? s match?)
  (cond
    [(atom? s) (match? s)]
    [(list? s) (all-x?/los s match?)]))

; all-x?/los : LOSExpr [Atom -> Boolean] -> Boolean
; do all Atoms in LOSExpr return #true with the input predicate?
(check-expect (all-x?/los ex-losexpr-1 all-numbers?/atom) #true)
(check-expect (all-x?/los (list "hi") all-hi?/atom) #true)
(check-expect (all-x?/los ex-losexpr-3 all-numbers?/atom) #false)
(define (all-x?/los los match?)
  (andmap (λ (sexpr) (all-x? sexpr match?)) los))

; all-numbers?/v2 : SExpr -> Boolean
; are all Atoms in an SExpr numbers?
(check-expect (all-numbers?/v2 ex-sexpr-1) #true)
(check-expect (all-numbers?/v2 ex-sexpr-2) #false)
(check-expect (all-numbers?/v2 '(72 3 (-7 2.4) (2 (-100 123)) 5)) #true)
(check-expect (all-numbers?/v2 ex-sexpr-4) #false)
(check-expect (all-numbers?/v2 ex-sexpr-5) #true)
(define (all-numbers?/v2 s)
  (all-x? s all-numbers?/atom))



;----------------------------------------------------------------------------------------------------;
;
;     ;             ;              ;
;     ;             ;
;   ;;;;;   ;;;   ;;;;;   ;;;;   ;;;    ;;;
;     ;    ;;  ;    ;     ;;  ;    ;   ;   ;
;     ;    ;   ;;   ;     ;        ;   ;
;     ;    ;;;;;;   ;     ;        ;    ;;;
;     ;    ;        ;     ;        ;       ;
;     ;    ;        ;     ;        ;   ;   ;
;     ;;;   ;;;;    ;;;   ;      ;;;;;  ;;;
;
;
;----------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------------------------------------------;
;-------------------------------------------- constants ---------------------------------------------;
;----------------------------------------------------------------------------------------------------;

;--------------------- drawing background of variable size --------------------;
; this section goes first so we can define a constant for the background

; draw-background : Nat Nat Nat -> Image
; draws the grid of tetris game with input width, height, and size of grid squares
(check-expect (draw-background 10 1 12) (make-row 10 12))
(check-expect (draw-background 5 4 30) (above (make-row 5 30)
                                              (make-row 5 30)
                                              (make-row 5 30)
                                              (make-row 5 30)))
(define (draw-background x y s)
  (cond
    [(= y 1) (make-row x s)] ; a single row of cells
    [(> y 1) (above (make-row x s) (draw-background x (- y 1) s))]))

; make-row : Nat Nat -> Image
; draws one row of a tetris game grid, with input number of cells and cell size
(check-expect (make-row 1 10) (rectangle 10 10 "outline" "black"))
(check-expect (make-row 4 15) (beside (rectangle 15 15 "outline" "black")
                                      (rectangle 15 15 "outline" "black")
                                      (rectangle 15 15 "outline" "black")
                                      (rectangle 15 15 "outline" "black")))
(define (make-row x s)
  (cond
    [(= x 1) (rectangle s s "outline" "black")] ; a single grid cell
    [(> x 1) (beside (rectangle s s "outline" "black") (make-row (- x 1) s))]))
;------------------------------------------------------------------------------;

(define GRID-SIZE 20) ; the width and height of the grid squares
(define BOARD-WIDTH 10) ; the width of the game board in grid squares
(define BOARD-HEIGHT 20) ; the height of the game board in grid squares
(define BOARD-HEIGHT/PIX (* BOARD-HEIGHT GRID-SIZE)) ; board height in pixels
(define BOARD-WIDTH/PIX (* BOARD-WIDTH GRID-SIZE)) ; board width in pixels

; grid for the game to be drawn on
(define BACKGROUND (draw-background BOARD-WIDTH BOARD-HEIGHT GRID-SIZE))

; speed to drop pieces at
(define TICK-RATE 0.25)


;----------------------------------------------------------------------------------------------------;
;----------------------------------------- data definitions -----------------------------------------;
;----------------------------------------------------------------------------------------------------;

;----------------------------------------- Brick definition -----------------------------------------;
(define-struct brick (x y color))
; A Brick is a (make-brick Integer Integer Color)
; Interpretation: A (make-brick x-g y-g c) represents a square brick
; at position (x-g, y-g) in the grid, to be rendered in color c.
; EXAMPLES
(define brick1 (make-brick 0 0 "red"))
(define brick2 (make-brick 9 19 "green"))
(define brick3 (make-brick 0 1 "magenta"))
(define brick4 (make-brick 2 3 "blue"))
(define brick5 (make-brick 4 4 "purple"))
; TEMPLATE
; brick-temp : Brick -> ???
#;(define (brick-temp b)
    (... (brick-x b) ... (brick-y b) ... (brick-color b) ...))

;------------------------------------------ LOB definition ------------------------------------------;
; A ListOfBricks (LOB) is one of:
; - '()
; - (cons Brick LOB)
; and represents a collection of Bricks in tetris
; EXAMPLES
; more examples can be found at the end of the "tetra creation functions" section
(define lob1 (cons brick1 '()))
(define lob2 (cons brick2 lob1))
; TEMPLATE
; lob-temp : LOB -> ???
#;(define (lob-temp lob)
    (cond
      [(empty? lob) ...]
      [(cons? lob) (... (brick-temp (first lob)) ... (lob-temp (rest lob)) ...)]))

;----------------------------------------- Piece definition -----------------------------------------;
(define-struct piece (lob center))
; A Piece is (make-piece ListOfBricks Posn)
; - where lob is a list of the bricks that make up the Piece
; - center is the position of the center of the piece
; and represents a piece in a game of tetris
; EXAMPLES:
; more examples can be found at the end of the "tetra creation functions" section
(define piece1 (make-piece lob1 (make-posn 0 0)))
(define piece2 (make-piece lob2 (make-posn 9 19)))
; TEMPLATE:
; Piece -> ???
#;(define (piece-temp p)
    (... (lob-temp (piece-lob p)) ...
         (posn-temp (piece-center p)) ...))

;----------------------------------------- World definition -----------------------------------------;
(define-struct world (piece pile score))
; A World is a (make-world Piece LOB Nat)
; - where piece is the current tetris piece in play
; - pile is the list of Bricks on the bottom of the game
; - score is the player's score
; and represents the current state of a game of tetris
; EXAMPLES:
; more examples can be found at the end of the "tetra creation functions" section
(define world1 (make-world piece1 lob2 0))
(define world2 (make-world piece2 '() 10))
; TEMPLATE:
; world-temp : World -> ???
#;(define (world-temp w)
    (... (piece-temp (world-piece w)) ...
         (lob-temp (world-pile w)) ...
         (world-score w) ...))


;----------------------------------------------------------------------------------------------------;
;---------------------------------------- generating pieces -----------------------------------------;
;----------------------------------------------------------------------------------------------------;

; create-tetris-piece : Nat Nat Color [List-of Posn] -> Piece
; makes a tetris piece with a center at (x,y), a color determined by the input, and a list of
; Posns determining how to shift the center coordinate to get each brick in the Piece
(check-expect (create-tetris-piece 7 8 "green" (list (make-posn 0 0) (make-posn 1 0)
                                                     (make-posn 0 -1) (make-posn 1 -1)))
              (create-o 7 8))
(check-expect (create-tetris-piece 2 14 "cyan" (list (make-posn 0 0) (make-posn -1 0)
                                                     (make-posn 1 0) (make-posn -1 1)))
              (create-j 2 14))
(define (create-tetris-piece x y color lop)
  (make-piece (create-tetris-lob x y color lop)
              (make-posn x y)))

; Nat Nat Color [List-of Posn] -> LOB
; makes a LOB with a color determined by the input, and a list of Posns determining how to shift
; (x,y) to get each brick in the LOB
(check-expect (create-tetris-lob 7 8 "green" (list (make-posn 0 0) (make-posn 1 0)
                                                   (make-posn 0 -1) (make-posn 1 -1)))
              (piece-lob (create-o 7 8)))
(check-expect (create-tetris-lob 2 14 "cyan" (list (make-posn 0 0) (make-posn -1 0)
                                                   (make-posn 1 0) (make-posn -1 1)))
              (piece-lob (create-j 2 14)))
(define (create-tetris-lob x y color lop)
  (local [; create-tetris-brick/posn : Posn -> Brick
          ; creates a Brick at an (x,y) coordinate, using a color passed in from create-tetris-lob
          (define (create-tetris-brick/posn p)
            (make-brick (+ x (posn-x p)) (+ y (posn-y p)) color))]
    ; for each Posn adjustment, make a Brick that distance from the center
    (map create-tetris-brick/posn lop)))

;------------------------------------- tetra creation functions -------------------------------------;
; create-o : Nat Nat -> Piece
; produces an O tetra  with the upper left block at (x,y)
(check-expect (create-o 0 19) (make-piece (list (make-brick 0 19 "green") (make-brick 1 19 "green")
                                                (make-brick 0 18 "green") (make-brick 1 18 "green"))
                                          (make-posn 0 19)))
(check-expect (create-o 8 7) (make-piece (list (make-brick 8 7 "green") (make-brick 9 7 "green")
                                               (make-brick 8 6 "green") (make-brick 9 6 "green"))
                                         (make-posn 8 7)))
(define (create-o x y)
  (create-tetris-piece x y "green" (list (make-posn 0 0) (make-posn 1 0)
                                         (make-posn 0 -1) (make-posn 1 -1))))

; create-i : Nat Nat -> Piece
; produces an I tetra with the second block at (x,y)
(check-expect (create-i 1 4) (make-piece (list (make-brick 1 4 "blue") (make-brick 0 4 "blue")
                                               (make-brick 2 4 "blue") (make-brick 3 4 "blue"))
                                         (make-posn 1 4)))
(check-expect (create-i 6 16) (make-piece (list (make-brick 6 16 "blue") (make-brick 5 16 "blue")
                                                (make-brick 7 16 "blue") (make-brick 8 16 "blue"))
                                          (make-posn 6 16)))
(define (create-i x y)
  (create-tetris-piece x y "blue" (list (make-posn 0 0) (make-posn -1 0)
                                        (make-posn 1 0) (make-posn 2 0))))

; create-l : Nat Nat -> Piece
; produces an L tetra with the center block at (x,y)
(check-expect (create-l 2 7) (make-piece (list (make-brick 2 7 "purple") (make-brick 1 7 "purple")
                                               (make-brick 3 7 "purple") (make-brick 3 8 "purple"))
                                         (make-posn 2 7)))
(check-expect (create-l 1 18) (make-piece (list (make-brick 1 18 "purple") (make-brick 0 18 "purple")
                                                (make-brick 2 18 "purple") (make-brick 2 19 "purple"))
                                          (make-posn 1 18)))
(define (create-l x y)
  (create-tetris-piece x y "purple" (list (make-posn 0 0) (make-posn -1 0)
                                          (make-posn 1 0) (make-posn 1 1))))

; create-j : Nat Nat -> Piece
; produces an J tetra with the center block at (x,y)
(check-expect (create-j 3 2) (make-piece (list (make-brick 3 2 "cyan") (make-brick 2 2 "cyan")
                                               (make-brick 4 2 "cyan") (make-brick 2 3 "cyan"))
                                         (make-posn 3 2)))
(check-expect (create-j 4 10) (make-piece (list (make-brick 4 10 "cyan") (make-brick 3 10 "cyan")
                                                (make-brick 5 10 "cyan") (make-brick 3 11 "cyan"))
                                          (make-posn 4 10)))
(define (create-j x y)
  (create-tetris-piece x y "cyan" (list (make-posn 0 0) (make-posn -1 0)
                                        (make-posn 1 0) (make-posn -1 1))))

; create-t : Nat Nat -> Piece
; produces a T tetra with the center block at (x,y)
(check-expect (create-t 3 2) (make-piece (list (make-brick 3 2 "orange") (make-brick 2 2 "orange")
                                               (make-brick 4 2 "orange") (make-brick 3 3 "orange"))
                                         (make-posn 3 2)))
(check-expect (create-t 4 10) (make-piece (list (make-brick 4 10 "orange") (make-brick 3 10 "orange")
                                                (make-brick 5 10 "orange") (make-brick 4 11 "orange"))
                                          (make-posn 4 10)))
(define (create-t x y)
  (create-tetris-piece x y "orange" (list (make-posn 0 0) (make-posn -1 0)
                                          (make-posn 1 0) (make-posn 0 1))))

; create-z : Nat Nat -> Piece
; produces a Z tetra with the bottom left block at (x,y)
(check-expect (create-z 8 8) (make-piece (list (make-brick 8 8 "pink") (make-brick 7 9 "pink")
                                               (make-brick 9 8 "pink") (make-brick 8 9 "pink"))
                                         (make-posn 8 8)))
(check-expect (create-z 1 12) (make-piece (list (make-brick 1 12 "pink") (make-brick 0 13 "pink")
                                                (make-brick 2 12 "pink") (make-brick 1 13 "pink"))
                                          (make-posn 1 12)))
(define (create-z x y)
  (create-tetris-piece x y "pink" (list (make-posn 0 0) (make-posn -1 1)
                                        (make-posn 1 0) (make-posn 0 1))))

; create-s : Nat Nat -> Piece
; produces an S tetra with the bottom right block at (x,y)
(check-expect (create-s 3 2) (make-piece (list (make-brick 3 2 "red") (make-brick 4 3 "red")
                                               (make-brick 2 2 "red") (make-brick 3 3 "red"))
                                         (make-posn 3 2)))
(check-expect (create-s 4 10) (make-piece (list (make-brick 4 10 "red") (make-brick 5 11 "red")
                                                (make-brick 3 10 "red") (make-brick 4 11 "red"))
                                          (make-posn 4 10)))
(define (create-s x y)
  (create-tetris-piece x y "red" (list (make-posn 0 0) (make-posn 1 1)
                                       (make-posn -1 0) (make-posn 0 1))))

; create-dot : Nat Nat -> Piece
; produces a one-brick tetris piece at (x,y)
(check-expect (create-dot 3 2) (make-piece (list (make-brick 3 2 "magenta")) (make-posn 3 2)))
(check-expect (create-dot 4 10) (make-piece (list (make-brick 4 10 "magenta")) (make-posn 4 10)))
(define (create-dot x y)
  (create-tetris-piece x y "magenta" (list (make-posn 0 0))))

; create-long-i : Nat Nat -> Piece
; produces a 5-brick I piece with the middle block at (x,y)
(check-expect (create-long-i 3 2) (make-piece (list (make-brick 3 2 "darkgreen")
                                                    (make-brick 1 2 "darkgreen")
                                                    (make-brick 2 2 "darkgreen")
                                                    (make-brick 4 2 "darkgreen")
                                                    (make-brick 5 2 "darkgreen"))
                                              (make-posn 3 2)))
(check-expect (create-long-i 4 10) (make-piece (list (make-brick 4 10 "darkgreen")
                                                     (make-brick 2 10 "darkgreen")
                                                     (make-brick 3 10 "darkgreen")
                                                     (make-brick 5 10 "darkgreen")
                                                     (make-brick 6 10 "darkgreen"))
                                               (make-posn 4 10)))
(define (create-long-i x y)
  (create-tetris-piece x y "darkgreen" (list (make-posn 0 0) (make-posn -2 0) (make-posn -1 0)
                                             (make-posn 1 0) (make-posn 2 0))))

; more Piece, World, and LOB examples for use in other functions:
(define piece3 (create-z 3 18))
(define piece4 (create-t 8 6))
(define lob3 (piece-lob piece3))
(define lob4 (piece-lob piece4))
(define lob5 (append (piece-lob piece1) (piece-lob piece3)))
(define world3 (make-world piece3 '() 70))
(define world4 (make-world piece4 lob5 0))

;----------------------------------- random-new-piece and helpers -----------------------------------;
; random-new-piece : _ -> Piece
; creates a new piece at a random x-coordinate in the top row of a grid (ignoring input)
(check-random (random-new-piece 0) (choose-tetra (random 9) BOARD-WIDTH BOARD-HEIGHT))
(check-random (random-new-piece 0) (choose-tetra (random 9) BOARD-WIDTH BOARD-HEIGHT))
(define (random-new-piece _)
  ; 9 options for piece shape, so pick integer in [0, 9)
  (choose-tetra (random 9) BOARD-WIDTH BOARD-HEIGHT))

; choose-tetra : [0,8] Nat Nat -> Piece
; generates a tetra using a random number to select the type, and a grid width for it to fit inside
; note: the piece will not "avoid" the current world-pile if it has reached the top row, so
; if the piece spawns on top of an existing piece in the pile, the game will end
(check-random (choose-tetra 0 10 20) (create-o (random-range 0 9) 19))
(check-random (choose-tetra 1 10 20) (create-i (random-range 1 8) 19))
(check-random (choose-tetra 2 10 20) (create-l (random-range 1 9) 18))
(check-random (choose-tetra 3 10 20) (create-j (random-range 1 9) 18))
(check-random (choose-tetra 4 10 20) (create-t (random-range 1 9) 18))
(check-random (choose-tetra 5 10 20) (create-z (random-range 1 9) 18))
(check-random (choose-tetra 6 10 20) (create-s (random-range 1 9) 18))
(check-random (choose-tetra 7 10 20) (create-dot (random-range 0 10) 19))
(check-random (choose-tetra 8 10 20) (create-long-i (random-range 2 8) 19))
(define (choose-tetra r width height)
  (cond
    [(= r 0) (create-o (random-range 0 (- width 1)) (- height 1))]
    [(= r 1) (create-i (random-range 1 (- width 2)) (- height 1))]
    [(= r 2) (create-l (random-range 1 (- width 1)) (- height 2))]
    [(= r 3) (create-j (random-range 1 (- width 1)) (- height 2))]
    [(= r 4) (create-t (random-range 1 (- width 1)) (- height 2))]
    [(= r 5) (create-z (random-range 1 (- width 1)) (- height 2))]
    [(= r 6) (create-s (random-range 1 (- width 1)) (- height 2))]
    [(= r 7) (create-dot (random-range 0 width) (- height 1))]
    [(= r 8) (create-long-i (random-range 2 (- width 2)) (- height 1))]))

; random-range : Integer Integer -> Integer
; generates a random integer in the range [min, max)
(check-member-of (random-range 1 8) 1 2 3 4 5 6 7)
(check-member-of (random-range 4 10) 4 5 6 7 8 9)
(define (random-range min max)
  (+ min (random (- max min))))


;----------------------------------------------------------------------------------------------------;
;--------------------------------------------- big-bang ---------------------------------------------;
;----------------------------------------------------------------------------------------------------;
; main : Nat [0,100] -> World
; executes an interactive tetris game
(define (main rows-with-debris probability)
  (local [; create-world : Nat [0,100] -> World
          ; generates an initial World for the tetris game with a random piece as the current
          ; piece in play, and randomly generated debris on the bottom of the board
          (define (create-world rows prob)
            (make-world (random-new-piece -1) ; random-new-piece doesn't use its argument
                        (generate-pile rows prob)
                        0))]
    ; returns just the score when big-bang ends
    (world-score (big-bang (create-world rows-with-debris probability)
                   [to-draw draw-world]
                   [on-key handle-key]
                   [on-tick world->world TICK-RATE]
                   [stop-when pile-to-top?]))))

; generate-pile : Nat Nat -> LOB
; randomly fills first r rows with gray bricks according to probability p, to use in initial gamestate
(check-expect (generate-pile 0 100) '())
(check-random (generate-pile 1 50)
              (map (λ (p) (make-brick (posn-x p) (posn-y p) "gray"))
                   (filter (λ (_) (<= (random 101) 50)) (build-list BOARD-WIDTH n->posn))))
(check-random (generate-pile 12 25)
              (map (λ (p) (make-brick (posn-x p) (posn-y p) "gray"))
                   (filter (λ (_) (<= (random 101) 25)) (build-list (* 12 BOARD-WIDTH) n->posn))))
(define (generate-pile rows prob)
  (local [; keep-posn? : _ -> Boolean
          ; uses probability passed from generate-pile to filter out Posns from list
          (define (keep-posn? _)
            (<= (random 101) prob))]
    ; makes a gray brick at each Posn in filtered list
    (map (λ (p) (make-brick (posn-x p) (posn-y p) "gray"))
         (filter keep-posn? (build-list (* BOARD-WIDTH rows) n->posn)))))

; n->posn : Nat -> Posn
; takes a single number and converts it to a coordinate on the tetris board (increasing
; left-to-right and bottom-to-top)
(check-expect (n->posn 5) (make-posn (remainder 5 BOARD-WIDTH) (floor (/ 5 BOARD-WIDTH))))
(check-expect (n->posn 40) (make-posn (remainder 40 BOARD-WIDTH) (floor (/ 40 BOARD-WIDTH))))
(define (n->posn n)
  (make-posn (remainder n BOARD-WIDTH)
             (floor (/ n BOARD-WIDTH))))

; one more LOB example to use in later check-expects
(define lob6 (generate-pile 3 100))

;----------------------------------------- to-draw functions ----------------------------------------;
; draw-world : World -> Image
; draws the current state of a tetris game
(check-expect (draw-world world1) (beside/align "top"
                                                (draw-piece piece1 (draw-lob lob2 BACKGROUND))
                                                (draw-score 000000)))
(check-expect (draw-world world2) (beside/align "top"
                                                (draw-piece piece2 BACKGROUND)
                                                (draw-score 000010)))
(define (draw-world w)
  (beside/align "top"
                (draw-piece (world-piece w)
                            (draw-lob (world-pile w) BACKGROUND))
                (draw-score (world-score w))))

; draw-piece : Piece Image -> Image
; draws a tetris piece on a background
(check-expect (draw-piece piece1 BACKGROUND) (draw-lob lob1 BACKGROUND))
(check-expect (draw-piece piece2 BACKGROUND) (draw-lob lob2 BACKGROUND))
(define (draw-piece p grid)
  (draw-lob (piece-lob p) grid))

; draw-lob : LOB Image -> Image
; draws every brick in a LOB on a background
(check-expect (draw-lob lob1 BACKGROUND) (draw-brick brick1 BACKGROUND))
(check-expect (draw-lob lob2 BACKGROUND) (draw-brick brick2 (draw-brick brick1 BACKGROUND)))
(define (draw-lob lob grid)
  (foldr draw-brick grid lob))

; draw-brick : Brick Image -> Image
; draws a Brick of a tetris Piece on the given game grid
(check-expect (draw-brick brick1 BACKGROUND)
              (place-image/align (overlay (rectangle GRID-SIZE GRID-SIZE "outline" "black")
                                          (rectangle GRID-SIZE GRID-SIZE "solid" "red"))
                                 0 (- BOARD-HEIGHT/PIX GRID-SIZE) "left" "top" BACKGROUND))
(check-expect (draw-brick brick2 BACKGROUND)
              (place-image/align (overlay (rectangle GRID-SIZE GRID-SIZE "outline" "black")
                                          (rectangle GRID-SIZE GRID-SIZE "solid" "green"))
                                 (* 9 GRID-SIZE) (- (- BOARD-HEIGHT/PIX GRID-SIZE) (* 19 GRID-SIZE))
                                 "left" "top" BACKGROUND))
(define (draw-brick b grid)
  (place-image/align
   ; this part is the brick itself
   (overlay (rectangle GRID-SIZE GRID-SIZE "outline" "black")
            (rectangle GRID-SIZE GRID-SIZE "solid" (brick-color b)))
   ; coordinates for the brick to align with on the grid
   (* GRID-SIZE (brick-x b)) (- (- (image-height grid) GRID-SIZE) (* GRID-SIZE (brick-y b)))
   "left" "top" ; the upper left corner of the brick is used for placement
   grid))

; draw-score : Nat -> Image
; draws the player's current score
(check-expect (draw-score 0) (text "Score: 000000" 24 "black"))
(check-expect (draw-score 350) (text "Score: 000350" 24 "black"))
(define (draw-score s)
  (local [; pad : String -> String
          ; makes all tetris scores 6 digits long
          (define (pad s)
            (string-append (substring "000000" (string-length s)) s))]
    (text (string-append "Score: " (pad (number->string s))) 24 "black")))

;----------------------------------------- on-key functions -----------------------------------------;
; handle-key : World Key -> World
; handles key presses for big-bang function
(check-expect (handle-key world3 "left") (make-world (check-move translate-left piece3 '()) '() 70))
(check-expect (handle-key world4 "right")
              (make-world (check-move translate-right piece4 lob5) lob5 0))
(check-expect (handle-key world3 "a") (make-world (check-move piece-rotate-ccw piece3 '()) '() 70))
(check-expect (handle-key world4 "s") (make-world (check-move piece-rotate-cw piece4 lob5) lob5 0))
(check-expect (handle-key world3 "p") world3)
(define (handle-key w k)
  (cond
    [(key=? k "left") (make-world (check-move translate-left (world-piece w) (world-pile w))
                                  (world-pile w) (world-score w))]
    [(key=? k "right") (make-world (check-move translate-right (world-piece w) (world-pile w))
                                   (world-pile w) (world-score w))]
    ; would normally stop rotating if player accidentally has caps-lock on
    [(or (key=? k "a") (key=? k "A"))
     (make-world (check-move piece-rotate-ccw (world-piece w) (world-pile w))
                 (world-pile w) (world-score w))]
    [(or (key=? k "s") (key=? k "S"))
     (make-world (check-move piece-rotate-cw (world-piece w) (world-pile w))
                 (world-pile w) (world-score w))]
    [else w]))

;-------------------------- ensuring legal movement ---------------------------;
; check-move : [Piece -> Piece] Piece LOB -> Piece
; moves a piece with given operation, but only if the operation won't put piece in an illegal space
; (conflicting with the pile of bricks or out of the board's bounds)
(check-expect (check-move piece-rotate-ccw (create-t 6 0) '()) (create-t 6 0))
(check-expect (check-move piece-rotate-cw (create-o 4 13) '()) (piece-rotate-cw (create-o 4 13)))
(define (check-move move p pile)
  (if (lob-legal? (piece-lob (move p)) pile)
      (move p)
      p))

; lob-legal? : LOB LOB -> Boolean
; is the LOB inside the board's bounds and not overlapping with the World's pile?
(check-expect (lob-legal? lob3 '()) #true)
(check-expect (lob-legal? lob3 lob3) #false)
(check-expect (lob-legal? lob4 lob5) #true)
(check-expect (lob-legal? (piece-lob (create-o (+ BOARD-WIDTH 2) 0)) lob3) #false)
(define (lob-legal? lob pile)
  (and (lob-in-bounds? lob BOARD-WIDTH BOARD-HEIGHT)
       (not (lob-pile-conflict? lob pile))))

; lob-in-bounds? : LOB Nat Nat -> Boolean
; is all of a list of tetris bricks within the left, right, lower, and upper bounds of the game?
(check-expect (lob-in-bounds? (piece-lob (create-t 7 8)) 10 20) #true)
(check-expect (lob-in-bounds? (piece-lob (create-o 9 0)) 10 20) #false)
(check-expect (lob-in-bounds? (piece-lob (create-z 10 3)) 10 20) #false)
(define (lob-in-bounds? lob width height)
  (local [; brick-in-bounds/width-height : Brick -> Boolean
          ; is the Brick within the game bounds passed in by lob-in-bounds?
          (define (brick-in-bounds?/width-height b)
            (brick-in-bounds? b width height))]
    (andmap brick-in-bounds?/width-height lob)))

; brick-in-bounds? : Brick Nat Nat -> Boolean
; is a Brick within the left, right, lower, and upper bounds of the game?
(check-expect (brick-in-bounds? (make-brick 0 10 "purple") 10 20) #true)
(check-expect (brick-in-bounds? (make-brick 10 0 "orange") 10 20) #false)
(define (brick-in-bounds? b width height)
  (and (>= (brick-x b) 0)
       (< (brick-x b) width)
       (>= (brick-y b) 0)
       (< (brick-y b) height)))

; lob-pile-conflict? : LOB LOB -> Boolean
; does the current LOB overlap with the board pile?
(check-expect (lob-pile-conflict? lob3 lob3) #true)
(check-expect (lob-pile-conflict? lob3 lob4) #false)
(check-expect (lob-pile-conflict? lob3 (list (make-brick 3 18 "magenta"))) #true)
(define (lob-pile-conflict? current-lob pile)
  (local [; Brick -> Boolean
          ; checks a Brick against every Brick in the pile to see if they conflict
          (define (bricks-conflict?/specific current-brick)
            (ormap (λ (pile-brick) (bricks-conflict? current-brick pile-brick)) pile))]
    (ormap bricks-conflict?/specific current-lob)))

; bricks-conflict? : Brick Brick -> Boolean
; are two bricks in the same spot?
(check-expect (bricks-conflict? brick1 brick1) #true)
(check-expect (bricks-conflict? brick1 brick2) #false)
(define (bricks-conflict? b1 b2)
  (and (= (brick-x b1) (brick-x b2))
       (= (brick-y b1) (brick-y b2))))

;------------------------------- moving pieces --------------------------------;
;------------------------ translation -------------------------;
; translate-left : Piece -> Piece
(check-expect (translate-left piece2) (make-piece (translate-lob lob2 -1 0) (make-posn 8 19)))
(check-expect (translate-left piece3) (make-piece (translate-lob lob3 -1 0) (make-posn 2 18)))
(check-expect (translate-left piece4) (make-piece (translate-lob lob4 -1 0) (make-posn 7 6)))
(define (translate-left p)
  (make-piece (translate-lob (piece-lob p) -1 0)
              (translate-center (piece-center p) -1 0)))

; translate-right : Piece -> Piece
(check-expect (translate-right piece1) (make-piece (translate-lob lob1 1 0) (make-posn 1 0)))
(check-expect (translate-right piece3) (make-piece (translate-lob lob3 1 0) (make-posn 4 18)))
(check-expect (translate-right piece4) (make-piece (translate-lob lob4 1 0) (make-posn 9 6)))
(define (translate-right p)
  (make-piece (translate-lob (piece-lob p) 1 0)
              (translate-center (piece-center p) 1 0)))

; translate-lob : LOB Integer Integer -> LOB
; moves all bricks in a LOB by input amounts in the x and y directions
(check-expect (translate-lob lob1 1 0) (cons (translate-brick brick1 1 0) '()))
(check-expect (translate-lob lob2 0 1) (list (translate-brick brick2 0 1)
                                             (translate-brick brick1 0 1)))
(define (translate-lob lob x y)
  (local [; translate-brick/xy : Brick -> Brick
          ; translates a Brick by (x,y) amounts passed in by translate-lob
          (define (translate-brick/xy b)
            (translate-brick b x y))]
    (map translate-brick/xy lob)))

; translate-center : Posn Integer Integer -> Posn
; moves a Posn's x and y coordinates by input amounts
(check-expect (translate-center (make-posn 3 4) -2 0) (make-posn 1 4))
(check-expect (translate-center (make-posn 8 2) 6 -2) (make-posn 14 0))
(define (translate-center c x y)
  (make-posn (+ x (posn-x c)) (+ y (posn-y c))))

; translate-brick : Brick Integer Integer -> Brick
; moves Brick in tetris piece by input amounts in the x and y directions
(check-expect (translate-brick brick1 3 4) (make-brick 3 4 "red"))
(check-expect (translate-brick brick2 -2 -3) (make-brick 7 16 "green"))
(define (translate-brick b x y)
  (make-brick (+ x (brick-x b)) (+ y (brick-y b)) (brick-color b)))

;-------------------------- rotation --------------------------;
; piece-rotate-ccw : Piece -> Piece
; rotates a tetris piece counterclockwise around its center
(check-expect (piece-rotate-ccw piece3)
              (make-piece (lob-rotate brick-rotate-ccw (piece-center piece3) lob3)
                          (piece-center piece3)))
(check-expect (piece-rotate-ccw piece4)
              (make-piece (lob-rotate brick-rotate-ccw (piece-center piece4) lob4)
                          (piece-center piece4)))
(define (piece-rotate-ccw p)
  (make-piece (lob-rotate brick-rotate-ccw (piece-center p) (piece-lob p))
              (piece-center p)))

; piece-rotate-cw : Piece -> Piece
; rotates a tetris piece clockwise around its center
(check-expect (piece-rotate-cw piece3)
              (make-piece (lob-rotate brick-rotate-cw (piece-center piece3) lob3)
                          (piece-center piece3)))
(check-expect (piece-rotate-cw piece4)
              (make-piece (lob-rotate brick-rotate-cw (piece-center piece4) lob4)
                          (piece-center piece4)))
(define (piece-rotate-cw p)
  (make-piece (lob-rotate brick-rotate-cw (piece-center p) (piece-lob p))
              (piece-center p)))

; lob-rotate : [Brick -> Brick] Posn LOB -> LOB
; uses a brick rotation function to rotate a LOB around its center Posn
(check-expect (lob-rotate brick-rotate-ccw (make-posn 3 18) (piece-lob piece3))
              (list (make-brick 3 18 "pink") (make-brick 2 17 "pink")
                    (make-brick 3 19 "pink") (make-brick 2 18 "pink")))
(check-expect (lob-rotate brick-rotate-ccw (make-posn 3 18) (cons (make-brick 3 19 "pink") '()))
              (cons (make-brick 2 18 "pink") '()))
(check-expect (lob-rotate brick-rotate-cw (make-posn 3 18) (piece-lob piece3))
              (list (make-brick 3 18 "pink") (make-brick 4 19 "pink")
                    (make-brick 3 17 "pink") (make-brick 4 18 "pink")))
(check-expect (lob-rotate brick-rotate-cw (make-posn 3 18) (cons (make-brick 3 19 "pink") '()))
              (cons (make-brick 4 18 "pink") '()))
(define (lob-rotate brick-rotation c lob)
  (local [; brick-rotation/center : Brick -> Brick
          ; rotates a Brick around a center Posn passed in by lob-rotate
          (define (brick-rotation/center b)
            (brick-rotation c b))]
    (map brick-rotation/center lob)))

; brick-rotate-ccw : Posn Brick -> Brick
; Rotate the brick _b_ 90 degrees counterclockwise around the center _c_.
(check-expect (brick-rotate-ccw (make-posn 1 1) brick1) (make-brick 2 0 "red"))
(check-expect (brick-rotate-ccw (make-posn 9 18) brick2) (make-brick 8 18 "green"))
(define (brick-rotate-ccw c b)
  (make-brick (+ (posn-x c)
                 (- (posn-y c)
                    (brick-y b)))
              (+ (posn-y c)
                 (- (brick-x b)
                    (posn-x c)))
              (brick-color b)))

; brick-rotate-cw : Posn Brick -> Brick
; Rotate the brick _b_ 90 degrees clockwise around the center _c_.
(check-expect (brick-rotate-cw (make-posn 1 1) brick1)
              (brick-rotate-ccw (make-posn 1 1)
                                (brick-rotate-ccw (make-posn 1 1)
                                                  (brick-rotate-ccw (make-posn 1 1) brick1))))
(check-expect (brick-rotate-cw (make-posn 9 18) brick2)
              (brick-rotate-ccw (make-posn 9 18)
                                (brick-rotate-ccw (make-posn 9 18)
                                                  (brick-rotate-ccw (make-posn 9 18) brick2))))
(define (brick-rotate-cw c b)
  (brick-rotate-ccw c (brick-rotate-ccw c (brick-rotate-ccw c b))))

;---------------------------------------- on-tick functions -----------------------------------------;
; world->world : World -> World
; generates next tetris World state, including dropping the current piece down and
; creating random new pieces when needed
(check-expect (world->world world3) (make-world (drop-piece piece3) '() 70))
(check-random (world->world (make-world (make-piece (translate-lob lob3 0 -18) (make-posn 3 0))
                                        '() 10))
              (next-piece (make-world (make-piece (translate-lob lob3 0 -18) (make-posn 3 0))
                                      '() 10)))
(check-random (world->world (make-world (make-piece (translate-lob lob3 0 -15) (make-posn 3 3))
                                        lob6 170))
              (next-piece (make-world (make-piece (translate-lob lob3 0 -15) (make-posn 3 3))
                                      lob6 170)))
(define (world->world w)
  (if (lob-legal? (piece-lob (drop-piece (world-piece w))) (world-pile w))
      (make-world (drop-piece (world-piece w))
                  (world-pile w)
                  (world-score w))
      (next-piece w)))

; drop-piece : Piece -> Piece
; moves a tetris piece down
(check-expect (drop-piece piece3) (make-piece (translate-lob (piece-lob piece3) 0 -1)
                                              (make-posn 3 17)))
(check-expect (drop-piece piece4) (make-piece (translate-lob (piece-lob piece4) 0 -1)
                                              (make-posn 8 5)))
(define (drop-piece p)
  (make-piece (translate-lob (piece-lob p) 0 -1)
              (translate-center (piece-center p) 0 -1)))

; next-piece : World -> World
; spawns new piece and handles row clearing when a piece joins the pile
(check-random (next-piece (make-world (make-piece (translate-lob lob3 0 -18) (make-posn 3 0)) '() 10))
              (make-world (random-new-piece -1) (clear-rows (translate-lob lob3 0 -18) '()) 10))
(check-random (next-piece (make-world (make-piece (translate-lob lob3 0 -15) (make-posn 3 3))
                                      lob6 170))
              (make-world (random-new-piece -1)
                          (clear-rows (append (translate-lob lob3 0 -15) lob6) (list 0 1 2))
                          260))
(define (next-piece w)
  (local [; pile-height : LOB -> Nat
          ; finds max y-value in pile
          (define (pile-height lob)
            (foldr (λ (b base) (max (brick-y b) base)) 0 lob))
          ; create definitions to avoid doing calculations several times
          (define pl (piece-lob (world-piece w)))
          (define full-rows (rows-to-clear (append pl (world-pile w))
                                           (pile-height (append pl (world-pile w)))))]
    (make-world (random-new-piece -1)
                ; add current piece to the pile of bricks
                (clear-rows (append pl (world-pile w)) full-rows)
                (+ (* 10 (length full-rows) (length full-rows)) (world-score w)))))

; rows-to-clear : LOB Nat -> [List-of Nat]
; returns a list of which rows are full of bricks, given a LOB and its max y-coord
(check-expect (rows-to-clear lob3 19) '())
(check-expect (rows-to-clear (generate-pile 5 100) 5) (list 0 1 2 3 4))
(check-expect (rows-to-clear (translate-lob (generate-pile 2 100) 0 3) 5) (list 3 4))
(define (rows-to-clear lob h)
  (local [; row-full?/lob : Nat -> Boolean
          ; passes current pile of bricks to row-full? function
          (define (row-full?/lob y)
            (row-full? lob y))]
    (filter row-full?/lob (build-list h (λ (x) x)))))

; row-full? : LOB Nat -> Boolean
; is the given y-coordinate row of a LOB completely full?
(check-expect (row-full? '() 0) #false)
(check-expect (row-full? lob3 18) #false)
(check-expect (row-full? (generate-pile 6 100) 4) #true)
(define (row-full? lob y)
  (local [; get-row : LOB Nat -> LOB
          ; filters full pile into only bricks in the row we care about
          (define (get-row lob y)
            (filter (λ (b) (= (brick-y b) y)) lob))]
    ; for every column, one of the bricks must be in that column
    ; checking purely based on how many bricks are in the row could cause glitches in the edge
    ; case of the last piece spawning overlapped with the pile
    (andmap (λ (col) (ormap (λ (b) (= (brick-x b) col)) (get-row lob y)))
            (build-list BOARD-WIDTH (λ (x) x)))))

; clear-rows : LOB [List-of Nat] -> LOB
; clears any brick in the world pile with a y-coord that appears in the list of full rows,
; and moves down other bricks by the appropriate amounts
(check-expect (clear-rows '() '()) '())
(check-expect (clear-rows (generate-pile 2 100) (list 0 1)) '())
(check-expect (clear-rows lob3 '()) lob3)
(check-expect (clear-rows (append (translate-lob lob3 0 -16) (generate-pile 2 100)) (list 0 1))
              (translate-lob lob3 0 -18))
(define (clear-rows current-pile row-list)
  (local [; gravity : LOB [List-of Nat] -> LOB
          ; moves bricks down depending on how many rows were cleared beneath them
          (define (gravity lob lon)
            (map (λ (b) (translate-brick b 0 (- 0 (length (filter (λ (n) (< n (brick-y b))) lon)))))
                 lob))]
    ; only keep bricks if they don't match any of the listed y-coords
    (gravity (filter (λ (b) (andmap (λ (row) (not (= (brick-y b) row))) row-list)) current-pile)
             row-list)))


;--------------------------------------- stop-when functions ----------------------------------------;
; pile-to-top? : World -> Boolean
; has the pile of bricks reached the top row of the board?
(check-expect (pile-to-top? world3) #false)
(check-expect (pile-to-top? world4) #true)
(define (pile-to-top? w)
  (not (lob-in-bounds? (world-pile w) BOARD-WIDTH (- BOARD-HEIGHT 1))))

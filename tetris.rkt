;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

#|
Project: Mini-Tetris
Author: Brad Burzon
Date: Spring 2016

Desrcription: The following problems are mini problems solved to help
create and design the game tetris. 
|#
(define collaboration-statement "I worked alone.")

; logical constants
(define WIDTH 7)    ; the number of blocks horizontally
(define HEIGHT 10)  ; the number of blocks vertically

; graphical constants
(define BLOCK-SIZE 50)  ; size of a rendered block (blocks are square)
(define SCENE-WIDTH (* WIDTH BLOCK-SIZE))      ; scaled width of scene
(define SCENE-HEIGHT (* HEIGHT BLOCK-SIZE))    ; scaled height of scene

; pool of possible block colors
(define POOL
  (list "red" "orange" "yellow" "green" "blue" "indigo" "violet"))

#|
A Hue is a Color in POOL
 
A Block is a (make-block [1..HEIGHT] [1..WIDTH] Hue)
 
A Landscape is one of:
– '()
– (cons Block Landscape)
 
A World is a (make-world Block Landscape)
(define-struct world (piece land)

A Speed is one of
-'fast
-'slow
-'medium

A Direction is one of
-"down"
-"left"
-"right"

A NonEmptyList is one of
-(cons Any '())
-(cons Any NonEmptyList)

A RowValue is an interval between [1, 10]

|#

(define-struct block [row col color])
(define-struct world [piece land])

;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;

(define w0
  (make-world (make-block 1 4 "indigo")
              (list (make-block (sub1 HEIGHT) 2 "red")
                    (make-block HEIGHT 1 "green")
                    (make-block HEIGHT 2 "blue")
                    (make-block HEIGHT WIDTH "yellow"))))

(define w1
  (make-world (make-block 3 6 "red")
              (list (make-block 6 4 "green")
                    (make-block 7 4 "blue")
                    (make-block 8 3 "orange")
                    (make-block 8 4 "green")
                    (make-block 9 3 "violet")
                    (make-block 9 4 "yellow")
                    (make-block 9 7 "indigo")
                    (make-block 10 1 "red")
                    (make-block 10 2 "red")
                    (make-block 10 3 "red")
                    (make-block 10 4 "red")
                    (make-block 10 5 "red")
                    (make-block 10 7 "red"))))
;;1

(define w2
  (make-world (make-block 1 4 "green")
              (list (make-block 7 1 "blue")
                    (make-block 8 1 "violet")
                    (make-block 9 1 "orange")
                    (make-block 10 1 "yellow")
                    (make-block 9 2 "red")
                    (make-block 10 2 "blue")
                    (make-block 6 3 "yellow")
                    (make-block 7 3 "green")
                    (make-block 8 3 "indigo")
                    (make-block 9 3 "indigo")
                    (make-block 10 3 "yellow")
                    (make-block 2 4"yellow")
                    (make-block 3 4 "indigo")
                    (make-block 4 4 "orange")
                    (make-block 5 4 "indigo")
                    (make-block 6 4 "indigo")
                    (make-block 7 4 "green")
                    (make-block 8 4 "blue")
                    (make-block 9 4 "green")
                    (make-block 10 4 "violet")
                    (make-block 9 5 "indigo")
                    (make-block 10 5 "red")
                    (make-block 10 6 "yellow")
                    (make-block 8 7 "yellow")
                    (make-block 9 7 "green")
                    (make-block 10 7 "red"))))

(check-expect (length (world-land w2)) 26)
(check-expect (block-color (world-piece w2)) "green")
(check-expect (member? (second (world-land w2)) (world-land w2)) #true)

;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;

;;B
#| A FrameRate is one of
-.2
-.6
-1
|#

;speed->rate : Speed -> FrameRate
;(speed->rate speed) Returns the corresponding frame rate as a number in
;the interval (0..1].Specifically, .2 is for 'fast, .6 is for 'medium,
;and 1 is for 'slow.

(define (speed->rate speed)
  (cond
    [(equal? speed 'fast) .2]
    [(equal? speed 'medium) .6]
    [(equal? speed 'slow) 1]
    [else "unreachable"]))

(check-within (speed->rate 'medium) 1 1)
(check-within (speed->rate 'fast) 1 1)
(check-expect (speed->rate 'fart) "unreachable")
(check-expect (speed->rate 'slow) 1)

;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;

;A

;block->image : Block -> Image
;(block->image block) returns a solid square of dimension BLOCK-SIZE
;filled in with the block’s color and surrounded by a black outline

(define (block->image block)
  (frame (square BLOCK-SIZE "solid" (block-color block))))

(check-satisfied (block->image (make-block 1 1 "green")) image?)
(check-expect
 (image-width (block->image (make-block HEIGHT WIDTH "red"))) 50)
(check-expect
 (image-height (block->image (make-block HEIGHT WIDTH "red"))) 50)

;B

;place-block : Block Image -> Image
;(place-block block image) Returns the result of placing the block on
;the scene at the block’s grid coordinate ([1..HEIGHT], [1..WIDTH])

(define (place-block block image)
  (place-image/align (block->image block)
                     (* (block-col block) BLOCK-SIZE)
                     (* (block-row block) BLOCK-SIZE)
                     "right"
                     "bottom"
                     image))

(check-satisfied
 (place-block (make-block 1 2 "blue")
              (empty-scene SCENE-WIDTH SCENE-HEIGHT))
 image?)
(check-satisfied
 (place-block (make-block HEIGHT WIDTH "red")
              (place-block (make-block 1 2 "blue")
                           (empty-scene SCENE-WIDTH SCENE-HEIGHT)))
 image?)
(check-expect (image-height (place-block (make-block 1 2 "blue")
                                         (empty-scene SCENE-WIDTH
                                                      SCENE-HEIGHT)))
              SCENE-HEIGHT)

;C

;world->image : World -> Image
;(world->image world) returns the corresponding scene of
;each blocks contained in the world corresponding to their
;row and column

(define (world->image world)
  (place-block (world-piece world)
               (cond
                 [(empty? (world-land world))
                  (empty-scene SCENE-WIDTH SCENE-HEIGHT)]
                 [else (place-block
                        (first (world-land world))
                        (world->image
                         (make-world
                          (world-piece world)
                          (rest (world-land world)))))])))

(check-satisfied (world->image w2) image?)
(check-satisfied (world->image w0) image?)
(check-satisfied (world->image w1) image?)

;;;;;;;;;;;;;;
; Problem 4
;;;;;;;;;;;;;;

;B
; next-block : Block Direction -> Block
; (next-block block dir) returns the block in the indicated Direction
;which changes the block's row and column value accordingly 

(define (next-block block dir)
  (cond
    [(equal? dir "down")
     (make-block (min (add1 (block-row block)) HEIGHT)
                 (block-col block)
                 (block-color block))]
    [(equal? dir "right")
     (make-block (block-row block)
                 (min (add1 (block-col block)) WIDTH)
                 (block-color block))]
    [(equal? dir "left")
     (make-block (block-row block)
                 (max (sub1 (block-col block)) 1)
                 (block-color block))]
    [else block]))

(check-expect (block-row (next-block (make-block 1 1 "red") "up"))
              1)
(check-expect (block-col (next-block (make-block 2 2 "red") "right"))
              3)
(check-expect (block-col (next-block (make-block 3 3 "red") "left"))
              2)
(check-expect (block-row (next-block (make-block 4 4 "red") "down"))
              5)
;C

;same-location? : Block Block -> Bool
;(same-location? block1 block2) Returns #true iff block1 and block2 are
;in the same location in the grid. else, returns #false.

(define (same-location? block1 block2)
  (and (= (block-row block1) (block-row block2))
       (= (block-col block1) (block-col block2))))

(check-expect (same-location? (make-block 2 7 "red")
                              (make-block 2 7 "red")) #true)
(check-expect (same-location? (make-block 2 4 "red")
                              (make-block 2 7 "red")) #false)
(check-expect (same-location? (make-block 5 3 "red")
                              (make-block 2 7 "red")) #false)
;D

; exists? : Block Landscape -> Bool
; (exists? block list) Returns #true iff block
;exists in the given Landscape. otherwise, #false

(define (exists? block list)
  (cond
    [(empty? list) #false]
    [(same-location?  block (first list)) #true]
    [else (exists? block (rest list))]))


(check-expect (exists? (make-block 2 7 "red")
                       (list (make-block 2 6 "red")
                             (make-block 2 7 "green"))) #true)
(check-expect (exists? (make-block 2 7 "red")
                       (list (make-block 2 3 "red")
                             (make-block 5 2 "red")
                             (make-block 6 1 "red")
                             (make-block 2 1 "green"))) #false)
(check-satisfied (exists? (make-block 2 7 "red")
                          (list (make-block 2 3 "red")
                                (make-block 5 2 "red")
                                (make-block 6 1 "red")
                                (make-block 2 1 "green"))) boolean?)

(check-expect (exists? (make-piece 3) (world-land w2)) #true)
(check-expect (exists? (make-piece 3) (world-land w0)) #false)
(check-expect (exists? (make-piece 10) (world-land w1)) #true)

;E

;;at-rest? : Block Landscape -> Bool
;;(at-rest? block landscape) returns #true
;iff the block cannot move further down upon reaching another block
;below or hitting the bottom

(define (at-rest? block landscape)
  (or (exists? (next-block block "down") landscape)
      (equal? (block-row block) HEIGHT)))


(check-expect (at-rest? (make-block 1 4 "indigo")
                        (list (make-block
                               (sub1 HEIGHT) 2 "red"))) #false)
(check-expect (at-rest? (make-block 1 3 "indigo")
                        (list (make-block 2 3 "red")
                              (make-block
                               (sub1 HEIGHT) 2 "red"))) #true)
(check-satisfied (at-rest? (make-block 1 3 "indigo")
                           (list (make-block 2 3 "red")
                                 (make-block
                                  (sub1 HEIGHT) 2 "red"))) boolean?)
(check-expect (at-rest? (make-block 9 3 "indigo")
                        (list (make-block 10 3 "red")
                              (make-block
                               (sub1 HEIGHT) 2 "red"))) #true)
(check-expect (at-rest? (make-block HEIGHT 3 "indigo")
                        (list (make-block 2 3 "red")
                              (make-block
                               (sub1 HEIGHT) 2 "red"))) #true)
(check-expect (at-rest? (make-piece 1) (world-land w0)) #false)

;;;;;;;;;;;;;;
; Problem 5
;;;;;;;;;;;;;;

;B
;helper
;an-element: Nat NonEmptyList -> Any
;(an-element nat list) returns the element of the indexed number
;from the list


(define an-element
  (lambda (nat list)
    (cond
      [(zero? nat) (first list)]
      [else (an-element (sub1 nat) (rest list))])))

(check-expect (an-element 1 '(1 3)) 3)
(check-expect (an-element 3 '(3 3 3 "haha" 3 3 3)) "haha")
(check-expect (an-element 0 '(1)) 1)

;pick-one : NonEmptyList -> Any
;(pick-one list) Returns an element of the list selected at random
;with equal probability

(define (pick-one list)
  (an-element (random (length list)) list))

(check-satisfied (pick-one '(1 2 3 4 5 6 7 8 9 10)) number?)
(check-satisfied (pick-one (list (make-posn 3 2)
                                 (make-posn 4 3))) posn?)
(check-expect (block? (pick-one (list (make-piece 1)
                                      (make-piece 2)))) #true)
(check-expect (pick-one '(me)) 'me)


;C

#|
A RowValue is an interval between [1..10]
|#

;make-piece: RowValue -> Block
;(make-piece row) Returns a Block that is positioned in the middle of
;WIDTH and in the the given RowValue, with randomly selected Hue

(define (make-piece row)
  (make-block row (ceiling (/ WIDTH 2)) (pick-one POOL)))

(check-expect (block-row (make-piece 1)) 1)
(check-expect (block-col (make-piece 10)) 4)
(check-satisfied (make-piece 3) block?)

;;;;;;;;;;;;;;
;Problem 6
;;;;;;;;;;;;;;

;A

;helper
;bottom-full? : List Nat -> Bool
;(bottom-full? list nat) returns #true if the ammount of blocks resting
;at the bottom is equal to WIDTH. else, returns #false

(define (bottom-row list counter)
  (cond
    [(= counter WIDTH) #true]
    [(empty? list) #false]
    [(= (block-row (first list)) HEIGHT) 
     (bottom-row (rest list) (add1 counter))]
    [else (bottom-row (rest list) counter)]))

(check-expect (bottom-row (world-land w1) 0) #false)
(check-expect (bottom-row (world-land w2) 0) #true)
(check-expect (bottom-row (world-land w0) 0) #false)
;A

;bottom-row-full? : Landscape -> Bool
;(bottom-row-full? landscape) Returns #true iff the bottom row is full
;of blocks and #false otherwise.

(define (bottom-row-full? landscape)
  (cond
    [(empty? landscape) #false]
    [else (bottom-row landscape 0)]))



(check-expect (bottom-row-full? (world-land w0)) #false)
(check-expect (bottom-row-full? (world-land w1)) #false)
(check-expect (bottom-row-full? (world-land w2)) #true)
(check-expect (bottom-row-full?
               (cons
                (make-block 10 6 "blue")
                (world-land w1))) #true)
;B

;remove-bottom-row : Landscape -> Landscape
;(remove-bottom-row scene) Returns Landscape with every block on the
;bottom row removed

(define (remove-bottom-row scene)
  (cond
    [(empty? scene) '()]
    [(= (block-row (first scene)) HEIGHT)
     (remove-bottom-row (rest scene))]
    [else (cons (first scene) (remove-bottom-row (rest scene)))]))

(check-expect (remove-bottom-row (world-land w0))
              (list (make-block 9 2 "red")))
(check-expect (world? (make-world
                       (make-piece 1)
                       (remove-bottom-row (world-land w1)))) #true)
(check-expect (length (remove-bottom-row (world-land w2))) 19)
(check-within (length (remove-bottom-row (world-land w1))) 3 4)

;C

;helper
;move-down : Landscape -> Landscape
;(move-down list) returns a list with all the blocks moved down by one
;row
(define (move-down list)
  (cond 
    [(null? list) '()]
    [else (cons (next-block (first list) "down")
                (move-down (rest list)))]))   

(check-expect (block? (first (move-down (list (make-piece 1))))) #true)
(check-expect (block-row (third (move-down (list(make-piece 1)
                                                (make-piece 5)
                                                (make-piece 2)
                                                (make-piece 3)
                                                (make-piece 4)
                                                (make-piece 1))))) 3)
(check-expect (block-col (first (move-down (list
                                            (make-piece 1))))) 4)


;compress: World -> World
;(compress world) returns World with the bottom row removed and all the
;blocks moved down by one row iff the bottom row is full. else,
;returns World unchanged

(define (compress world)
  (cond
    [(not(bottom-row-full? (world-land world))) world]
    [else (make-world (world-piece world)
                      (move-down (remove-bottom-row
                                  (world-land world))))]))

(check-expect (length (world-land (compress w2))) 19) 
(check-expect (length (world-land (compress w0))) 4)
(check-expect (length (world-land (compress w1))) 13)

;;;;;;;;;;;;;;
; Problem 7
;;;;;;;;;;;;;;

;A

;game-over? : World -> Bool
;(game-over? world) returns #true iff the block coming in the middle
;column is at rest and #false otherwise

(define (game-over? world)
  (and (same-location? (world-piece world)
                       (make-block 1 (round (/ WIDTH 2)) "red"))
       (at-rest? (world-piece world) (world-land world))))

(check-expect (game-over? w0) #false)
(check-expect (game-over? w1) #false)
(check-expect (game-over? w2) #true)

;B
;tick-handler : World -> World
;(tick-handler world) returns the next state of the world after one tick
;of the clock. If the block is at rest, a new block is generated and if
;the bottom row is full, the world is compressed.

(define (tick-handler world)
  (cond
    [(at-rest? (world-piece world) (world-land world))
     (tick-handler
      (make-world (make-piece 0)
                  (world-land (compress
                               (make-world
                                (make-piece 0)
                                (cons (world-piece world)
                                      (world-land world)))))))]
    [else (make-world
           (next-block (world-piece world) "down")
           (world-land world))]))

(check-satisfied (world? (tick-handler w2)) boolean?)
(check-expect (block-row (world-piece (tick-handler w2))) 1)
(check-expect (block-col (world-piece (tick-handler w2))) 4)

;C
;key-handler : World Direction -> World
;(key-handler world dir) returns the world from attempting to move the
;tetris piece from indicated direction

(define (key-handler world dir)
    (make-world
     (cond
       [(exists? (next-block (world-piece world) dir)
                 (world-land world)) (world-piece world)]
       [else (next-block (world-piece world) dir)])
     (world-land world)))

(check-expect (block-row (world-piece (key-handler w0 "down"))) 2)
(check-expect (block-col (world-piece (key-handler w0 "right"))) 5)
(check-expect (block-col (world-piece (key-handler w0 "left"))) 3)
(check-expect (block-row (world-piece (key-handler w2 "down"))) 1)

(define same-block-column
  (lambda (block list)
    (cond
      [(= (block-row block) HEIGHT) 0]
      [(exists? (make-block (block-row block) (block-col block)
                            "red") list)
       (+ 1 (same-block-column (make-block (add1 (block-row block))
                                           (block-col block)
                                           "red") list))]
      [else (same-block-column (make-block (add1 (block-row block))
                                           (block-col block)
                                           "red") list)])))

;D

;tetris: Speed -> Animation
;(tetris speed) Returns an animation of a game that users can interact
;with Direction in moving the blocks. The game can be in different
;speed in which the purpose is to organize each blocks and fill spaces
;of bottom row. Game ends when middle column is full of blocks.

(define (tetris speed)
  (big-bang (make-world (make-piece 1) '())
            [to-draw world->image]
            [on-tick tick-handler (speed->rate speed)]
            [on-key key-handler]
            [stop-when game-over?]
            [name "Simple Tetris"]))

;(tetris 'slow)

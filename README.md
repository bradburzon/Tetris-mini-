# Tetris(mini)
A playable tetris big-bang program in Racket using  with HTDP(How to Design Program) Intermediate Student Language. Don't stack the blocks! Put them in the same level to get rid of a layer of blocks.
![game](https://github.com/bradburzon/Tetris-mini-/blob/master/SimpleTetrisIMG.PNG)

Press "left" and "right" arrow to move the blocks and the "down" arrow button to make it faster.

Game Mode: 'slow, 'medium, 'fast


DATA DEFINITIONS
-------------------------------------------------------------------
Project: Mini-Tetris

Author: Brad Burzon

Date: Spring 2016

Desrcription: The following problems are mini problems solved to help
create and design the game tetris. 

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

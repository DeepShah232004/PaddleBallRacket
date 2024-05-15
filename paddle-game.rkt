;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname paddle-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(define-struct ball (x y dx dy))
(define-struct p1 (y dy score))
(define-struct p2 (y dy score))

(define-struct world (ball p1 p2))


;; constants

(define PADDLE (rectangle 10 40 "solid" "black"))
(define WIDTH 600)
(define HEIGHT 500)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define SCREEN  (place-image
                 (rectangle 2 HEIGHT "solid" "black")
                 CTR-X CTR-Y
                 (rectangle WIDTH HEIGHT "solid" "lightblue")))

(define PADDLE-LEFT 20)
(define PADDLE-RIGHT 580)

(define PADDLE-SPEED 30)

(define BALL-RADIUS 10)

(define TOP (+ 0        BALL-RADIUS)) ;these constants define the "inner box"
(define BOT (- HEIGHT 1 BALL-RADIUS)) ;that constrains the center of the ball
(define LEF (+ 0        BALL-RADIUS)) ;
(define RIG (- WIDTH  1 BALL-RADIUS)) ;

(define TP (+ 0 40))
(define BT (- HEIGHT 1 20))

(define LP (+ BALL-RADIUS 25))
(define RP (- (- WIDTH 25) BALL-RADIUS))

(define BALL (circle BALL-RADIUS "solid" "green"))



(define start (make-world (make-ball CTR-X CTR-Y (- 5 (random 11)) (- 5 (random 11)))
                          (make-p1 CTR-Y PADDLE-SPEED 0)
                          (make-p2 CTR-Y PADDLE-SPEED 0)))

(define start-1 (make-world (make-ball CTR-X CTR-Y 10 (- 5 (random 11)))
                          (make-p1 CTR-Y PADDLE-SPEED 0)
                          (make-p2 CTR-Y PADDLE-SPEED 0)))

(define trial (make-world (make-ball CTR-X CTR-Y 10 0)
                          (make-p1 CTR-Y PADDLE-SPEED 0)
                          (make-p2 CTR-Y PADDLE-SPEED 0)))


(@htdf main)

(define (main w)
  (big-bang w
    (on-tick next-ball)
    (to-draw render)
    (on-key handle-key)
    (stop-when reached5? win)
    ))


;(@htdf next-ball)

(define (next-ball w)
  (cond ;[(reached5? w) (win w)]
        [(touch-top?    w) (bounce-top w)]
        [(touch-bottom? w) (bounce-bottom w)]
        [(touch-right?  w) (restart-r w)]
        [(touch-left?   w) (restart-l w)]
        [(touch-left-paddle? w) (bounce-left w)]
        [(touch-right-paddle? w) (bounce-right w)]
        [else
         (glide w)]))

(define (touch-top? w)
  (<= (+ (ball-y (world-ball w)) (ball-dy (world-ball w))) TOP))
(define (bounce-top w)
  (make-world (make-ball (ball-x (world-ball w)) (+ TOP 1) (ball-dx (world-ball w)) (- (ball-dy (world-ball w)))) (if (<= (+ PADDLE-SPEED (p1-y (world-p1 w))) TP)
                                                                                                                      (make-p1 (- TP 19) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                                                                                                                      (if (>= (+ PADDLE-SPEED (p1-y (world-p1 w))) BT)
                                                                                                                          (make-p1 (- BT 1) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                                                                                                                          (world-p1 w)))
              (if (<= (+ PADDLE-SPEED (p2-y (world-p2 w))) TP)
                  (make-p2 (- TP 19) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                  (if (>= (+ PADDLE-SPEED (p2-y (world-p2 w))) BT)
                      (make-p2 (- BT 1) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                      (world-p2 w)))))

(define (touch-bottom? w)
  (>= (+ (ball-y (world-ball w)) (ball-dy (world-ball w))) BOT))
(define (bounce-bottom w)
  (make-world (make-ball (ball-x (world-ball w)) (- BOT 1) (ball-dx (world-ball w)) (- (ball-dy (world-ball w)))) (if (<= (+ PADDLE-SPEED (p1-y (world-p1 w))) TP)
                                                                                                                      (make-p1 (- TP 19) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                                                                                                                      (if (>= (+ PADDLE-SPEED (p1-y (world-p1 w))) BT)
                                                                                                                          (make-p1 (- BT 1) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                                                                                                                          (world-p1 w)))
              (if (<= (+ PADDLE-SPEED (p2-y (world-p2 w))) TP)
                  (make-p2 (- TP 19) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                  (if (>= (+ PADDLE-SPEED (p2-y (world-p2 w))) BT)
                      (make-p2 (- BT 1) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                      (world-p2 w)))))

(define (touch-left? w)
  (>= LEF (+ (ball-x (world-ball w)) (ball-dx (world-ball w)))))
(define (restart-l w)
  (make-world (make-ball CTR-X CTR-Y 10 (- 5 (random 11)))
              (make-p1 CTR-Y PADDLE-SPEED (+ 0 (p1-score (world-p1 w))))
              (make-p2 CTR-Y PADDLE-SPEED (+ 1 (p2-score (world-p2 w))))))


(define (touch-right? w)
  (>= (+ (ball-x (world-ball w)) (ball-dx (world-ball w))) RIG))
(define (restart-r w)
  (make-world (make-ball CTR-X CTR-Y -10 (- 5 (random 11)))
              (make-p1 CTR-Y PADDLE-SPEED (+ 1 (p1-score (world-p1 w))))
              (make-p2 CTR-Y PADDLE-SPEED (+ 0 (p2-score (world-p2 w))))))

(define (touch-left-paddle? w)
  (and (>= LP (+ (ball-x (world-ball w)) (ball-dx (world-ball w))))
       (>= (+ 20 (p1-y (world-p1 w))) (+ (ball-y (world-ball w)) (ball-dy (world-ball w))) (- (p1-y (world-p1 w)) 20))))

(define (bounce-left w)
  (make-world (make-ball (+ LP 1) (ball-y (world-ball w)) (- (ball-dx (world-ball w))) (- 5 (random 11)))
              (if (<= (+ PADDLE-SPEED (p1-y (world-p1 w))) TP)
                  (make-p1 (- TP 19) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                  (if (>= (+ PADDLE-SPEED (p1-y (world-p1 w))) BT)
                      (make-p1 (- BT 1) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                      (world-p1 w)))
              (if (<= (+ PADDLE-SPEED (p2-y (world-p2 w))) TP)
                  (make-p2 (- TP 19) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                  (if (>= (+ PADDLE-SPEED (p2-y (world-p2 w))) BT)
                      (make-p2 (- BT 1) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                      (world-p2 w)))))

(define (touch-right-paddle? w)
  (and (<= RP (+ (ball-x (world-ball w)) (ball-dx (world-ball w))))
       (>= (+ 20 (p2-y (world-p2 w))) (+ (ball-y (world-ball w)) (ball-dy (world-ball w))) (- (p2-y (world-p2 w)) 20))))

(define (bounce-right w)
  (make-world (make-ball (- RP 1) (ball-y (world-ball w)) (- (ball-dx (world-ball w))) (- 5 (random 11)))
              (if (<= (+ PADDLE-SPEED (p1-y (world-p1 w))) TP)
                  (make-p1 (- TP 19) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                  (if (>= (+ PADDLE-SPEED (p1-y (world-p1 w))) BT)
                      (make-p1 (- BT 1) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                      (world-p1 w)))
              (if (<= (+ PADDLE-SPEED (p2-y (world-p2 w))) TP)
                  (make-p2 (- TP 19) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                  (if (>= (+ PADDLE-SPEED (p2-y (world-p2 w))) BT)
                      (make-p2 (- BT 1) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                      (world-p2 w)))))

(define (glide w)
  (make-world (make-ball (+ (ball-x (world-ball w)) (ball-dx (world-ball w))) (+ (ball-y (world-ball w)) (ball-dy (world-ball w)))
                         (ball-dx (world-ball w)) (ball-dy (world-ball w)))
              (if (<= (+ PADDLE-SPEED (p1-y (world-p1 w))) TP)
                  (make-p1 (- TP 19) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                  (if (>= (+ PADDLE-SPEED (p1-y (world-p1 w))) BT)
                      (make-p1 (- BT 1) (p1-dy (world-p1 w)) (p1-score (world-p1 w)))
                      (world-p1 w)))
              (if (<= (+ PADDLE-SPEED (p2-y (world-p2 w))) TP)
                  (make-p2 (- TP 19) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                  (if (>= (+ PADDLE-SPEED (p2-y (world-p2 w))) BT)
                      (make-p2 (- BT 1) (p2-dy (world-p2 w)) (p2-score (world-p2 w)))
                      (world-p2 w)))))

(define (render w)
  
  (place-image (text (number->string (p2-score (world-p2 w))) 50 "darkgreen") (+ CTR-X 50) (- CTR-Y 200)
               (place-image (text (number->string (p1-score (world-p1 w))) 50 "darkgreen") (- CTR-X 50) (- CTR-Y 200)
                            (place-image BALL (ball-x (world-ball w)) (ball-y (world-ball w)) (place-image
                                                                                               PADDLE
                                                                                               PADDLE-LEFT
                                                                                               (p1-y (world-p1 w))
                                                                                               (place-image PADDLE
                                                                                                            PADDLE-RIGHT
                                                                                                            (p2-y (world-p2 w))
                                                                                                            SCREEN))))))


(define (handle-key w ke)
  (cond [(key=? "s" ke) (make-world (world-ball w) (make-p1 (+ PADDLE-SPEED (p1-y (world-p1 w))) (p1-dy (world-p1 w)) (p1-score (world-p1 w))) (world-p2 w))]
        [(key=? "w" ke) (make-world (world-ball w) (make-p1 (- (p1-y (world-p1 w)) PADDLE-SPEED) (p1-dy (world-p1 w)) (p1-score (world-p1 w))) (world-p2 w))]
        [(key=? "down" ke) (make-world (world-ball w) (world-p1 w) (make-p2 (+ PADDLE-SPEED (p2-y (world-p2 w))) (p2-dy (world-p2 w)) (p2-score (world-p2 w))))]
        [(key=? "up" ke) (make-world (world-ball w) (world-p1 w) (make-p2 (- (p2-y (world-p2 w)) PADDLE-SPEED) (p2-dy (world-p2 w)) (p2-score (world-p2 w))))]
        [(key=? "r" ke) (make-world (make-ball CTR-X CTR-Y 10 (- 5 (random 11)))
                                    (make-p1 CTR-Y PADDLE-SPEED 0)
                                    (make-p2 CTR-Y PADDLE-SPEED 0))]
        [else
         w]))

(define (reached5? w)
  (or (= (p1-score (world-p1 w)) 5)
      (= (p2-score (world-p2 w)) 5)))

(define (win w)
  (if (= (p1-score (world-p1 w)) 5)
  (place-image (text "P1 WON" 50 "black") CTR-X CTR-Y
               (rectangle WIDTH HEIGHT "solid" "lightblue"))
  (place-image (text "P2 WON" 50 "black") CTR-X CTR-Y
               (rectangle WIDTH HEIGHT "solid" "lightblue"))))


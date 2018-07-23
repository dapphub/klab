(set-option :auto-config false)
(set-option :smt.mbqi false)
;(set-option :smt.mbqi.max_iterations 15)

(declare-fun pow256 () Int)
;(assert (= pow256 115792089237316195423570985008687907853269984665640564039457584007913129639936))
(assert (>= pow256 115792089237316195423570985008687907853269984665640564039457584007913129639936))
(assert (<= pow256 115792089237316195423570985008687907853269984665640564039457584007913129639936))

;
; expFunc
;

(declare-fun expFunc (Int Int) Int)

(assert (forall ((x1 Int) (y1 Int) (x2 Int) (y2 Int))
  (!
    (=> (and (<= x1 x2) (<= y1 y2))
      (<= (expFunc x1 y1) (expFunc x2 y2))
    )
    :pattern ((expFunc x1 y1) (expFunc x2 y2))
  )
))

(assert (forall ((x Int) (y Int)) (! (=> (>= y 1) (>= (expFunc x y) x      )) :pattern ((expFunc x y)))))
(assert (forall ((x Int) (y Int)) (! (=> (>= y 2) (>= (expFunc x y) (* x x))) :pattern ((expFunc x y)))))

;
; smt_rpow
;

(declare-fun smt_rpow (Int Int Int Int) Int)
(assert (forall ((z Int) (x Int) (y Int) (b Int)) (! (=> (= y 1) (= (smt_rpow z x y b) (div (+ (* z x) (div b 2)) b))) :pattern ((smt_rpow z x y b)))))

(assert (forall ((z1 Int) (z2 Int) (x1 Int) (y1 Int) (x2 Int) (y2 Int) (b1 Int) (b2 Int))
  (!
    (=> (and (<= z1 z2) (<= x1 x2) (<= y1 y2) (<= b1 b2))
      (<= (smt_rpow z1 x1 y1 b1) (smt_rpow z2 x2 y2 b2))
    )
    :pattern ((smt_rpow z1 x1 y1 b1) (smt_rpow z2 x2 y2 b2))
  )
))

(assert (forall ((z Int) (x Int) (y Int) (b Int)) (! (=> (>= y 1) (>= (* (smt_rpow z x y b) b) (* z x))) :pattern ((smt_rpow z x y b)))))
(assert (forall ((z Int) (x Int) (y Int) (b Int)) (! (=> (>= y 2) (>= (* (smt_rpow z x y b) b) (* x x))) :pattern ((smt_rpow z x y b)))))

(assert (forall ((z Int) (x Int) (y Int) (b Int)) (! (=> (>= y 1) (>= (* (smt_rpow z x y b) b) (+ (* z x) (div b 2)))) :pattern ((smt_rpow z x y b)))))
(assert (forall ((z Int) (x Int) (y Int) (b Int)) (! (=> (>= y 2) (>= (* (smt_rpow z x y b) b) (+ (* x x) (div b 2)))) :pattern ((smt_rpow z x y b)))))

;;
;; smt_div_word
;;
;
;(declare-fun smt_div_word (Int Int) Int)
;
;(assert (forall ((x Int) (y Int))
;  (!
;    (=>
;      (not (= y 0))
;      (= (smt_div_word x y)
;         (div x y))
;    )
;    :pattern ((smt_div_word x y))
;  )
;))

;
; end of prelude
;

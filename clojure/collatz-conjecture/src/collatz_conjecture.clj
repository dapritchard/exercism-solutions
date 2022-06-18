(ns collatz-conjecture)

(defn collatz [num]
  (defn update [num]
    (if (= (rem num 2) 0)
      (quot num 2)
      (+ (* 3 num) 1)))
  (defn collatz-impl [num step]
    (if (= num 1)
      step
      (collatz-impl (update num) (+ step 1))))
  (if (and (integer? num) (>= num 1))
    (collatz-impl num 0)
    (throw (Exception. "The input must be an integer no less than 1"))))

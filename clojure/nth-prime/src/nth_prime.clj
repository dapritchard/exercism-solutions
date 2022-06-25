(ns nth-prime)

(defn check-prime [n]
  "Crude prime number checker"
  (let [sqrt-n (int (Math/sqrt n))]
    (defn check-odds [m]
      (if (> m sqrt-n)
        true
        (if (zero? (rem n m))
          false
          (check-odds (+ m 2)))))
    (cond
      (= n 2) true
      (zero? (rem n 2)) false
      :else (check-odds 3))))

(defn find-next-prime [m]
  "Finds the next prime number >= `m`. Assumes that `m` is odd."
  (if (check-prime m)
    m
    (find-next-prime (+ m 2))))

(defn nth-prime [n]
  "Returns the prime number in the nth position."
  (defn nth-prime-impl [m p]
    (if (>= p n)
      m
      (nth-prime-impl (find-next-prime (+ m 2)) (+ p 1))))
  (cond
    (or (not (integer? n)) (<= n 0))
    (throw (IllegalArgumentException.
            "The input must be an integer no less than 0 and less than 1000000000000"))
    (= n 1) 2
    (= n 2) 3
    :else (nth-prime-impl 3 2)))

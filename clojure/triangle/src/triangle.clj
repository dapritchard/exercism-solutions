(ns triangle)

(defn is-valid? [s0 s1 s2]
  (let [[v0, v1, v2] (sort [s0, s1, s2])]
    (and (> v0 0) (> v1 0) (> v2 0) (<= v2 (+ v0 v1)))))

(defn equilateral? [s0 s1 s2]
  (and (is-valid? s0 s1 s2) (= s0 s1) (= s1 s2)))

(defn isosceles? [s0 s1 s2]
  (and (is-valid? s0 s1 s2)
       (or (= s0 s1)
           (= s0 s2)
           (= s1 s2))))

(defn scalene? [s0 s1 s2]
  (and (is-valid? s0 s1 s2)
       (not= s0 s1)
       (not= s0 s2)
       (not= s1 s2)))

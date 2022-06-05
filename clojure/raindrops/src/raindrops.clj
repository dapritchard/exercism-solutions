(ns raindrops)

(def drop-sounds
  {3 "Pling"
   5 "Plang"
   7 "Plong"})

(defn convert [n]
  (let [is-fac-3 (= 0 (mod n 3))
        is-fac-5 (= 0 (mod n 5))
        is-fac-7 (= 0 (mod n 7))
        is-fac-any (or is-fac-3 is-fac-5 is-fac-7)]
    (if is-fac-any
      (str (if is-fac-3 (get drop-sounds 3) "")
           (if is-fac-5 (get drop-sounds 5) "")
           (if is-fac-7 (get drop-sounds 7) ""))
      (str n))))

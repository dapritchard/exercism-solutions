(ns armstrong-numbers)

(defn to-list [num acc]
  (if (= num 0)
    acc
    (to-list (quot num 10)
             (cons (rem num 10)
                   acc))))

(defn pow [num expt]
  (defn pow-impl [sum expt]
    (if (<= expt 0)
      sum
      (pow-impl (* sum num) (- expt 1))))
  (pow-impl 1N expt))

(defn calc-armstrong [num]
  (let [digits-list (to-list num '())
        len (count digits-list)]
    (long (reduce #(+' (pow %2 len) %1)
                  0N
                  digits-list))))

(defn armstrong? [num]
  (= (calc-armstrong num) num))

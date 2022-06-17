(ns all-your-base)

(defn pstl-to-val [base digits]
  (defn pstl-to-val-impl [m l]
    (if (empty? l)
      ()
      (cons (* m (first l))
            (pstl-to-val-impl (* base m) (rest l)))))
  (reduce + (pstl-to-val-impl 1 (reverse digits))))

(defn val-to-pstl [base num]
  (defn val-to-pstl-impl [num acc]
    (if (= num 0)
      acc
      (val-to-pstl-impl (quot num base)
                        (cons (rem num base)
                              acc))))
  (if (= num 0)
    '(0)
    (val-to-pstl-impl num ())))

(defn convert [base digits new-base]
  (if (or (<= base 1)
          (<= new-base 1)
          (empty? digits)
          (some #(or (< % 0) (<= base %)) digits))
    nil
    (let [val (pstl-to-val base digits)]
      (val-to-pstl new-base val))))

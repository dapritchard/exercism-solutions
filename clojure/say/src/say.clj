(ns say)

(def hunds-str
  {0 ""
   1 "one hundred"
   2 "two hundred"
   3 "three hundred"
   4 "four hundred"
   5 "five hundred"
   6 "six hundred"
   7 "seven hundred"
   8 "eight hundred"
   9 "nine hundred"})

(def tens-str
  {0 ""
   1 ""
   2 "twenty"
   3 "thirty"
   4 "forty"
   5 "fifty"
   6 "sixty"
   7 "seventy"
   8 "eighty"
   9 "ninety"})

(def ten-to-nineteen
  {10 "ten"
   11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"})

(def ones-str
  {0 ""
   1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"})

(defn split-digits [num]
  "Covert an integer like 1234 into '(1 2 3 4)"
  (defn split-digits-impl [num' acc]
    (if (= num' 0)
      acc
      (split-digits-impl (quot num' 10)
                         (cons (rem num' 10) acc))))
  (split-digits-impl num ()))

(defn split-thousands [num]
  "Convert an integer like 1234 into '((0 0 1) (2 3 4))"
  (defn split-thousands-impl [num' acc]
    (if (= num' 0)
      acc
      (split-thousands-impl (quot num' 1000)
                            (cons (split-digits (rem num' 1000))
                                  acc))))
  (let [digits (split-thousands-impl num ())
        high-digits (first digits)
        len-high-digits (count high-digits)]
    (cond
      (= len-high-digits 3) digits
      (= len-high-digits 2) (cons (cons 0 high-digits) (rest digits))
      (= len-high-digits 1) (cons (concat '(0 0) high-digits) (rest digits))
      (= len-high-digits 0) '(0 0 0))))

(defn create-hundreds-str [d3 d2 d1]
  (defn create-tens-str [d2 d1]
    (cond
      (= d2 0) (ones-str d1)
      (= d2 1) (ten-to-nineteen (+ 10 d1))
      true (str (tens-str d2) "-" (ones-str d1))))
  (let [tens (create-tens-str d2 d1)]
    (if (= d3 0)
      tens
      (str (hunds-str d3) " " tens))))

(defn number [num]
  (if (and (integer? (<= 0 num) (<= num 1000000000000)))
    (if (= num 0)
      "zero"
      ;; (number-impl num)
      1
      )
    (throw (Exception. "The input must be an integer no less than 0 and less than 1000000000000")))
)

(defn number-impl [num]
  (defn f [num-list' scale-words']
    (if (empty? scale-words')
      num-list'
      (list* (first num-list')
             (first scale-words')
             (f (rest num-list') (rest scale-words')))))
  (defn create-scale-words [n]
    (cond
      (= n 4) '("billion" "million" "thousand")
      (= n 3) '("million" "thousand")
      (= n 2) '("thousand")
      (= n 1) ()))
  (let [num-list (split-thousands num)
        scale-words (create-scale-words (count num-list))]
    (f num-list scale-words)))

;; (defn split-hund [num acc]
;;   (if (= num 0)
;;     acc
;;     (split-hund (quot num 1000)
;;                   (cons (rem num 1000) acc))))

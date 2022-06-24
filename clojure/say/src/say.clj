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

(defn split-digits [num val]
  "Covert an integer like 1234 into '(1 2 3 4) when splitting by 10s"
  (defn split-digits-impl [num' acc]
    (if (= num' 0)
      acc
      (split-digits-impl (quot num' val)
                         (cons (rem num' val) acc))))
  (split-digits-impl num ()))

(defn split-hundreds [num]
  "Convert an integer like 1234 into '((0 0 1) (2 3 4))"
  (defn ensure-3-digits [digits]
    (let [n-digits (count digits)]
      (cond
        (= n-digits 3) digits
        (= n-digits 2) (cons 0 digits)
        (= n-digits 1) (list* 0 0 digits)
        (= n-digits 0) '(0 0 0))))
  (let [hunds-vals (split-digits num 1000)
        hunds-lists (map #(split-digits % 10) hunds-vals)]
    (map ensure-3-digits hunds-lists)))

(defn create-hunds-str [d3 d2 d1]
  "Convert inputs like 1, 1, and 9 into \"one hundred nineteen\""
  (defn create-tens-str [d2 d1]
    (cond
      (= d2 0) (ones-str d1)
      (= d2 1) (ten-to-nineteen (+ 10 d1))
      (= d1 0) (tens-str d2)
      true (str (tens-str d2) "-" (ones-str d1))))
  (let [hunds (hunds-str d3)
        tens (create-tens-str d2 d1)]
    (cond
      (= d3 0) tens
      (and (= d2 0) (= d1 0)) hunds
      true (str hunds " " tens))))

(defn number-impl [num]
  "Workhorse function for `number` without any argument checking"
  ;; Takes inputs like '(1 2 3) and "thousand" and returns "one hundred and
  ;; twenty-three thousand"
  (defn create-segm [digits scale-word]
    (str (apply create-hunds-str digits)
         " "
         scale-word))
  ;; Return a list of scale words depending on `n`, the number of digits triples
  (defn create-scale-words [n]
    (cond
      (= n 4) '("billion" "million" "thousand")
      (= n 3) '("million" "thousand")
      (= n 2) '("thousand")
      (= n 1) ()))
  ;; Stringify each segment and then combine
  (let [num-list (split-hundreds num)
        scale-words (create-scale-words (count num-list))
        last-num-list (last num-list)
        last-str (when (not= last-num-list '(0 0 0))
                   (list (apply create-hunds-str last-num-list)))
        segm-pairs (map list (butlast num-list) scale-words)
        segm-pairs-nonzero (filter #(not= (first %) '(0 0 0)) segm-pairs)]
    (clojure.string/join
     " "
     (concat (map #(apply create-segm %) segm-pairs-nonzero)
             last-str))))

(defn number [num]
  "Convert an integer into a written word string"
  (if (and (integer? num) (<= 0 num) (< num 1000000000000))
    (if (= num 0)
      "zero"
      (number-impl num))
    (throw (IllegalArgumentException.
            "The input must be an integer no less than 0 and less than 1000000000000"))))

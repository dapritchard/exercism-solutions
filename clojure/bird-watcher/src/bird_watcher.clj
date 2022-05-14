(ns bird-watcher)

(def last-week [0 2 5 3 7 8 4])

(defn today [birds] (get birds (- (count birds) 1)))

(defn inc-bird [birds]
  (if (empty? birds)
    []
    (let [last-idx (- (count birds) 1)
          last-val (get birds last-idx)]
      (assoc birds last-idx (+ 1 last-val)))))

(defn day-without-birds? [birds]
  (cond
    (empty? birds) false
    (= (first birds) 0) true
    :else (day-without-birds? (rest birds))))

(defn n-days-count [birds n]
  (defn sum [x] (if (empty? x)
                  0
                  (+ (first x) (sum (rest x)))))
  (sum (take n birds)))

(defn busy-days [birds]
  (cond
    (empty? birds) 0
    (>= (first birds) 5) (+ 1 (busy-days (rest birds)))
    :else (busy-days (rest birds))))

(defn odd-week? [birds]
  (declare check-0)
  (defn check-1 [birds]
    (cond
      (empty? birds) true
      (= 1 (first birds)) (check-0 (rest birds))
      :else false))
  (defn check-0 [birds]
    (cond
      (empty? birds) true
      (= 0 (first birds)) (check-1 (rest birds))
      :else false))
  (check-1 birds))

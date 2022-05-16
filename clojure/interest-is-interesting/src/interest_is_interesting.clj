(ns interest-is-interesting)

(defn interest-rate
  [balance]
  (cond
    (< balance 0M) -3.213
    (< balance 1000M) 0.5
    (< balance 5000M) 1.621
    :else 2.475))

(defn annual-balance-update
  [balance]
  (let [interest-rate-pct (bigdec (interest-rate balance))
        interest-rate-val (/ interest-rate-pct 100M)
        interest-rate-abs (if (neg? interest-rate-val)
                            (- interest-rate-val)
                            interest-rate-val)
        interest (* balance interest-rate-abs)]
    (+ balance interest)))

(defn amount-to-donate
  [balance tax-free-percentage]
  ;; the value '0.02' comes from multiplying by 2 to double the amount of
  ;; `tax-free-percentage`, and dividing by 100 to convert `tax-free-percentage`
  ;; to a non-percent
  (if (> balance 0M)
    (int (* balance tax-free-percentage 0.02))
    0))

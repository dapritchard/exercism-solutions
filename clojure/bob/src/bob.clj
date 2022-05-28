(ns bob)

(defn strip-ws [s]
  (clojure.string/replace s #"\s" ""))

(defn question? [s]
  (re-find #"\?$" s))

(defn yelled? [s]
  (let [upper (clojure.string/replace s #"[^\p{Upper}]" "")
        lower (clojure.string/replace s #"[^\p{Lower}]" "")]
    (and (empty? lower) (not (empty? upper)))))

(defn response-for [s]
  (let [s-no-ws (strip-ws s)
        is-question (question? s-no-ws)
        is-caps (yelled? s-no-ws)]
    (cond (empty? s-no-ws) "Fine. Be that way!"
          (and (not is-question) (not is-caps)) "Whatever."
          (and (not is-question) is-caps) "Whoa, chill out!"
          (and is-question (not is-caps)) "Sure."
          (and is-question is-caps) "Calm down, I know what I'm doing!")))

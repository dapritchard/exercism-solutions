(ns anagram)

(defn normalize-string [s]
  (-> s
      (clojure.string/lower-case)
      (clojure.string/split #"")
      (sort)
      (clojure.string/join)))

(defn make-check-anagram [s1]
  (def nrm-s1 (normalize-string s1))
  (def lower-s1 (clojure.string/lower-case s1))
  (defn check-anagram [s2]
    (let [nrm-s2 (normalize-string s2)
          lower-s2 (clojure.string/lower-case s2)]
      (and (= nrm-s1 nrm-s2)
           (not (= lower-s1 lower-s2)))))
  check-anagram)

(defn anagrams-for [word prospect-list]
  (def check-anagram (make-check-anagram word))
  (filter check-anagram prospect-list))

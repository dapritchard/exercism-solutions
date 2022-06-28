(ns acronym)

(defn acronym
  "Converts phrase to its acronym."
  [phrase]
  (defn split-subwords [word]
    (if (empty? word)
      ()
      (let [first (subs word 0 1)
            next-upper (clojure.string/replace word #"^.[^\p{Upper}]*" "")]
        (cons first (split-subwords next-upper)))))
  (let [words (clojure.string/split phrase #"[- ]")
        subwords-depth-2 (map split-subwords words)
        subwords-depth-1 (apply concat subwords-depth-2)
        subwords-str (clojure.string/join subwords-depth-1)]
    (clojure.string/upper-case subwords-str)))

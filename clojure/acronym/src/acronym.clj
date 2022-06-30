(ns acronym
  (:require [clojure.string :as string]))

(defn acronym
  "Converts phrase to its acronym."
  [phrase]
  (defn split-subwords-rest [word]
    (let [next-upper (string/replace word #"^[\p{Upper}]*[^\p{Upper}]*" "")]
      (if (empty? next-upper)
        ()
        (cons (subs next-upper 0 1) (split-subwords-rest next-upper)))))
  (defn split-subwords [word]
    (if (empty? word)
      ()
      (cons (subs word 0 1) (split-subwords-rest (subs word 1)))))
  (let [words (string/split phrase #"[- ]")
        subwords-depth-2 (map split-subwords words)
        subwords-depth-1 (apply concat subwords-depth-2)
        subwords-str (string/join subwords-depth-1)]
    (string/upper-case subwords-str)))

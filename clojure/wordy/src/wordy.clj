(ns wordy
  (:require [clojure.string :as string]))

(defn trim-initial [s]
  ;; Remove the surrounding setence structure from the expression string
  ;; fragment
  (let [re #"What is (.*)\?$"]
    (if (re-matches re s)
      (string/replace-first s re "$1")
      (throw (IllegalArgumentException. "Invalid initial sentence")))))

(defn split-initial [s]
  ;; Split a string into a list of two strings with the elements representing a
  ;; value and whatever is remaining, in that order. Throws an error if the
  ;; input doesn't satisfy the expected regex
  (let [m #"^(-?\d+) (.*)$"]
    (if (re-matches m s)
      (list (string/replace-first s m "$1")
            (string/replace-first s m "$2"))
      (throw (IllegalArgumentException. (str "Illegal input:  '" s "'"))))))

(defn split-contents [s]
  ;; Split a string into a list of three strings with the elements representing
  ;; an operator, a value, and whatever is remaining (possibly an empty string),
  ;; in that order. Throws an error if the input doesn't satisfy the expected
  ;; regex
  (let [m #"(plus|minus|multiplied by|divided by) (-?\d+) ?(.*)"]
    (if (re-matches m s)
      (let [s1 (string/replace-first s m "$1")
            s2 (string/replace-first s m "$2")
            s3 (string/replace-first s m "$3")]
        (list s1 s2 s3))
      (throw (IllegalArgumentException. (str "Illegal input:  '" s "'"))))))

(defn split-exprs [s]
  ;; Peel of the leading operator and the value immediately following that and
  ;; then prepend to the result of a recursive call
  (defn split-expr-impl [s]
    (if (empty? s)
      ()
      (let [[v1 v2 v3] (split-contents s)]
        (cons v1 (cons v2 (split-expr-impl v3))))))
  ;; Remove the leading value and pass the remaining to `split-expr-impl`
  (let [[v1 remaining] (split-initial s)]
    (cons v1 (split-expr-impl remaining))))

(defn conv-op [s]
  ;; Convert a string to the corresponding operator
  (cond
    (= s "plus") +
    (= s "minus") -
    (= s "multiplied by") *
    (= s "divided by") /))

(defn eval-exprs [exprs]
  ;; Sequentially evaluate the expressions by taking the operator and next value
  ;; of off the top of the `exprs` stack and combining them with `v1`
  (defn eval-expr-impl [v1 exprs]
    (if (empty? exprs)
      v1
      (let [op (conv-op (first exprs))
            v2 (Integer/parseInt (second exprs))
            result (op v1 v2)
            remaining (rest (rest exprs))]
        (eval-expr-impl result remaining))))
  ;; Peel off the first value from `exprs` and invoke `eval-expr-impl`
  (eval-expr-impl (Integer/parseInt (first exprs)) (rest exprs)))

(defn evaluate [s]
  ;; Extract the expression string fragment from the full sentence, break the
  ;; string into a list of string expression representations, and then walk
  ;; through the list and evaluate the expressions
  (let [trimmed (trim-initial s)
        exprs (split-exprs trimmed)
        result (eval-exprs exprs)]
    result))

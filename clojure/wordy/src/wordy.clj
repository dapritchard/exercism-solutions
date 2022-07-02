(ns wordy
  (:require [clojure.string :as string]))

(defn remove-initial [s]
  (let [re #"What is (.*)\?$"]
    (if (re-matches re s)
      (string/replace-first s re "$1")
      (throw (IllegalArgumentException. "Invalid initial sentence")))))

(defn split-contents
  "Take a string `s` and regex `m` and return a list with elements given by the
  replacement strings `r1`, `r2`, and `r2`. Throws an error if `s` doesn't
  match."
  [s m r1 r2 r3 msg]
  (if (re-matches m s)
    (let [s1 (string/replace-first s m r1)
          s2 (string/replace-first s m r2)
          s3 (string/replace-first s m r3)]
      (list s1 s2 s3))
    (throw (IllegalArgumentException. (str msg "'" s "'")))))

(defn split-exprs
  ;; Split `s` on the last digit and call the 2-arg variant
  ([s]
   (let [m #"^(.*) (-?\d+)()$"
         [v1 v2 v3] (split-contents s m "$1" "$2" "" "No trailing number: ")]
     (split-exprs v1 (list v2))))
  ;; Peel off the leading two elements and prepend them to the result of the
  ;; recursive call
  ([s init]
   (let [m #"(-?\d+) (plus|minus|multiplied by|divided by) ?(.*)"
         r1 "$1"
         r2 "$2"
         r3 "$3"
         msg "Illegal input: "]
     (defn split-expr-impl [s]
       (if (empty? s)
         init
         (let [[v1 v2 v3] (split-contents s m r1 r2 r3 msg)]
           (cons v1 (cons v2 (split-expr-impl v3)))))))
   (split-expr-impl s)))

(defn conv-op [s]
  (cond
    (= s "plus") +
    (= s "minus") -
    (= s "multiplied by") *
    (= s "divided by") /))

(defn eval-exprs [exprs]
  (defn eval-expr-impl [v1 exprs]
    (if (empty? exprs)
      v1
      (let [op (conv-op (first exprs))
            v2 (Integer/parseInt (second exprs))
            result (op v1 v2)
            remaining (rest (rest exprs))]
        (eval-expr-impl result remaining))))
  (eval-expr-impl (Integer/parseInt (first exprs)) (rest exprs)))

(defn evaluate [s] ;; <- arglist goes here
  (let [trimmed (remove-initial s)
        exprs (split-exprs trimmed)]
    (eval-exprs exprs)))

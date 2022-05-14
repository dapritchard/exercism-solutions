(ns log-levels
  (:require [clojure.string :as str]))

(defn message
  "Takes a string representing a log line
   and returns its message with whitespace trimmed."
  [s]
  (let [re-search #"^\s*\[(ERROR|WARNING|INFO)\]:\s*"]
    (-> s
        (str/replace-first re-search "")
        (str/trim-newline)
        (str/trimr))))

(defn log-level
  "Takes a string representing a log line
   and returns its level in lower-case."
  [s]
  (let [re-search #"^\s*\[(ERROR|WARNING|INFO)\]:.*"]
    (-> s
        (str/trim-newline)
        (str/replace-first re-search "$1")
        (str/lower-case))))

(defn reformat
  "Takes a string representing a log line and formats it
   with the message first and the log level in parentheses."
  [s]
  (let [message-val (message s)
        loglevel-val (log-level s)]
    (str/join [message-val " (" loglevel-val ")"])))

(ns elyses-destructured-enchantments)

(defn first-card
  "Returns the first card from deck."
  [deck]
  (let [[fst-elem] deck]
    fst-elem))

(defn second-card
  "Returns the second card from deck."
  [deck]
  (let [[fst-elem snd-elem] deck]
    snd-elem))

(defn swap-top-two-cards
  "Returns the deck with first two items reversed."
  [deck]
  (let [[fst-elem snd-elem & rest-elems] deck]
    (conj rest-elems fst-elem snd-elem)))

(defn discard-top-card
  "Returns a vector containing the first card and
   a vector of the remaining cards in the deck."
  [deck]
  (let [[fst-elem & rest-elems] deck]
    [fst-elem rest-elems]))

(def face-cards
  ["jack" "queen" "king"])

(defn insert-face-cards
  "Returns the deck with face cards between its head and tail."
  [deck]
  (let [[fst-elem & rest-elems] deck]
    (if (empty? deck)
      face-cards
      (concat [fst-elem] face-cards rest-elems))))

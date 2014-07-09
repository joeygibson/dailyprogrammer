(ns dailyprogrammer.ch170-easy-blackjack-checker
  (:require [clojure.string :as string]))

(defn- get-data-from-file
  "Reads the data file, returning a vector of data for each line in the file."
  [file-name]
  (let [raw-contents (slurp file-name)
        lines (rest (string/split raw-contents #"\n"))]
    (for [line lines
          :let [[name cards] (string/split line #":")
                matches (re-seq #"\s*(\w+)\s*of\s*\w+\s*,?" cards)]]
      {:name  name
       :cards (map second matches)})))

(defn- get-value-for-card-name
  "Converts a textual name for a card into a numeric value. An ace returns a vector or 1 and 11"
  [name]
  (condp = name
    "Ace" 1
    "Two" 2
    "Three" 3
    "Four" 4
    "Five" 5
    "Six" 6
    "Seven" 7
    "Eight" 8
    "Nine" 9
    "Ten" 10
    "Jack" 10
    "Queen" 10
    "King" 10))

(defn- count-aces
  [cards]
  (let [aces (filter #(= % "Ace") cards)]
    (count aces)))

(defn- remove-aces
  [cards]
  (filter #(not (= % "Ace")) cards))

(defn- choose-ace-value
  "Decide if the aces should count as 1s or 11s.
  Shamelessly lifted from http://www.reddit.com/r/dailyprogrammer/comments/29zut0/772014_challenge_170_easy_blackjack_checker/ciq8mzr"
  [hand number-of-aces]
  (let [possible-hand (+ hand 10 number-of-aces)]
    (if (<= possible-hand 21)
      (+ 10 number-of-aces)
      number-of-aces)))

(defn- compute-hand
  "Returns a numeric value for a given player's textually-expressed hand"
  [player]
  (let [name (:name player)
        cards (:cards player)
        aceless-cards (remove-aces cards)
        number-of-aces (- (count cards) (count aceless-cards))
        numeric-cards (map get-value-for-card-name aceless-cards)
        aceless-total (reduce + numeric-cards)
        hand (+ aceless-total (choose-ace-value aceless-total number-of-aces))]
    {:name name
     :hand hand}))

(defn- get-names-from-winning-group
  [group]
  (let [names (map :name group)]
    (string/join ", " names)))

(defn- process-all-hands
  "Compute the value of each player's hand, sort by that value, and determine the winner"
  [data]
  (let [hands (map compute-hand data)
        still-in (remove #(> (:hand %) 21) hands)
        sorted-hands (reverse (sort-by :hand still-in))
        groups (vals (group-by :hand sorted-hands))
        winning-group (first groups)
        winning-names (get-names-from-winning-group winning-group)]
    (println "WG:" winning-group)
    (println "WN:" winning-names)
    (cond
      (empty? winning-group) "Nobody wins"
      (> (count winning-group) 1) (format "Tie: %s" winning-names)
      :else (format "Winner: %s" winning-names))))

(def data (get-data-from-file "resources/ch170-easy-input-1.txt"))
(process-all-hands data)
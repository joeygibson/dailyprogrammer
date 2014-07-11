(ns dailyprogrammer.ch170-easy-blackjack-checker
  (:require [clojure.string :as string]))

;; ch170_easy_blackjack_checker.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/29zut0/772014_challenge_170_easy_blackjack_checker/
;; which analyzes blackjack hands
;;
;; To run:
;; lein run -m dailyprogrammer.ch170-easy-blackjack-checker
;;

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
  "Converts a textual name for a card into a numeric value."
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
  "Simply counts the number of aces in the hand for later use"
  [cards]
  (let [aces (filter #(= % "Ace") cards)]
    (count aces)))

(defn- remove-aces
  "Return cards from the hand that are not aces"
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
        hand (+ aceless-total (choose-ace-value aceless-total number-of-aces))
        five-card-trick (and (<= hand 21)
                             (= (count cards) 5))]
    {:name            name
     :hand            hand
     :five-card-trick five-card-trick}))

(defn- get-names-from-winning-group
  "Return a comma-delimited list of the winners names."
  [group]
  (let [names (map :name group)]
    (string/join ", " names)))

(defn- process-all-hands
  "Compute the value of each player's hand, sort by that value, and determine the winner(s)"
  [data]
  (let [hands (map compute-hand data)
        still-in (remove #(> (:hand %) 21) hands)
        sorted-hands (reverse (sort-by :hand still-in))
        five-card-tricks (filter :five-card-trick still-in)
        groups (vals (group-by :hand sorted-hands))
        winning-group (if-not (empty? five-card-tricks)
                        five-card-tricks
                        (first groups))
        winning-names (get-names-from-winning-group winning-group)]
    (cond
      (empty? winning-group) "Nobody wins"
      (> (count winning-group) 1) (format "Tie: %s" winning-names)
      :else (format "Winner: %s" winning-names))))

(defn -main
  [& args]
  (let [files ["ch170-easy-input-1.txt"
               "ch170-easy-input-2.txt"
               "ch170-easy-input-chunes-1.txt"
               "ch170-easy-input-chunes-2.txt"
               "ch170-easy-input-chunes-3.txt"
               "ch170-easy-input-chunes-4.txt"]]
    (doseq [file files
            :let [file-name (format "resources/%s" file)]]
      (do (print file-name ": ")
          (println (process-all-hands (get-data-from-file file-name)))
          (println "---------------")))))


(ns dailyprogrammer.d6
  (:require [clojure.string :as string]))

;; d6.clj -- Joey Gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/25y2d0/5192014_challenge_163_easy_probability/
;;
;; To run, use Leiningen, like so:
;;
;; lein run -m d6
;;

(defn roll
  "Rolls 1d6, one time."
  []
  (rand-int 6))

(defn roll-n
  "Rolls the die n times, returning a vector of counts for each side."
  [n]
  (loop [cur 0
         counts [0 0 0 0 0 0]]
    (let [result (roll)
          result-count (nth counts result)
          updated-counts (assoc counts result (inc result-count))]
      (if (= cur n)
        updated-counts
        (recur (inc cur) updated-counts)))))

(defn collect-all-rolls
  "Rolls the dice an increasing number of times, collecting the results
with the number of times rolled."
  []
  (for [n [10 100 1000 10000 100000 1000000]]
    [n (roll-n n)]))

(defn calculate-percentages
  "For each value, calculate a percentage of the total."
  [results]
  (let [total (reduce + results)]
    (for [r results]
      (* (/ r total) 100.0))))

(defn pretty-print-rolls
  "Format each percentage rounded, right-aligned, with a percent sign."
  [results]
  (let [results-as-percentages (calculate-percentages results)
        formatted-results (for [r results-as-percentages]
                            (format "%5.2f%%" r))]
    (string/join " " formatted-results)))

(defn pretty-print-results
  "Format each row of results, including the number of rolls for the row."
  [results]
  (let [formatted-results (for [[k vals] results]
                            (format "%-10d %s" k (pretty-print-rolls vals)))]
    (string/join "\n" formatted-results)))

(defn roll-and-summarize
  "Starts the die rolls, summarizes the results, and returns a string,
suitable for printing."
  []
  (let [rolls (collect-all-rolls)]
    (str "# of Rolls 1s     2s     3s     4s     5s     6s\n"
         "====================================================\n"
         (pretty-print-results rolls))))

(defn -main
  []
  (println (roll-and-summarize)))


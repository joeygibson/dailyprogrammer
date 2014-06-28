(ns dailyprogrammer.ch168-string-index
  (:require [clojure.string :as string]))

;; ch168_string_index.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/299hvt/6272014_challenge_168_easy_string_index/
;; which implements a wacky string-chunker/indexer thingy.
;;
;; To run:
;; lein run -m dailyprogrammer.ch168-string-index
;;

(def input "...You...!!!@!3124131212 Hello have this is a --- string Solved !!...? to test @\n\n\n#!#@#@%$**#$@ Congratz this!!!!!!!!!!!!!!!!one ---Problem\n\n")

(defn- scrub-input
  "Remove invalid characters from the input string"
  [input]
  (map #(if (re-matches #"[a-zA-Z0-9]" (str %1))
         %1
         " ") input))

(defn- split-input
  "Divide the input into space-delimited chunks"
  [input]
  (let [chunks (string/split (str input) #"\s+")]
    (rest chunks)))

(defn- find-word
  "Returns the word found at the given index, where 0 < index <= (count input)"
  [input index]
  (let [adjusted-index (dec index)]
    (cond
      (or (< adjusted-index 0)
          (>= adjusted-index (count input))) ""
      :else (nth input adjusted-index))))

(defn string-index
  "Returns all the words found at the given indexes"
  [input indexes]
  (string/join " " (map (partial find-word input) indexes)))

(defn -main
  [& args]
  (let [clean-input (apply str (scrub-input input))
        chunks (split-input clean-input)]
    (println (string-index chunks [12 -1 1 -100 4 1000 9 -1000 16 13 17 15]))))




(ns dailyprogrammer.ch175-easy-bogo
  (:require [clojure.string :as st]))

;; ch175_easy_bogo.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/2d8yk5/8112014_challenge_175_easy_bogo/
;; which performs a bogo-sort.
;;
;; To run:
;; lein run -m dailyprogrammer.ch175-easy-bogo lolhe Hello
;;

(defn- remove-index
  "Accepts a sequence, and removes the element at the given index.
  If the index is outside the sequence, it returns the sequence, unchanged."
  [col i]
  (let [ex-i (inc i)]
    (if (> ex-i (count col))
      col
      (concat (take i col)
              (drop ex-i col)))))

(defn- randomize-string
  "Suffles the string into a 'random' order."
  [string]
  (loop [string string
         res []]
    (if (empty? string)
      (apply str res)
      (let [i (rand-int (count string))
            letter (get string i)]
        (recur (apply str (remove-index string i))
               (conj res letter))))))

(defn bogo
  "Compares the two strings. If they are not the same, it randomizes
  the first one and compares again. This continues until they are the same.
  It returns the number of tries it took to achieve sameness."
  [str1 str2]
  (loop [str1 str1
         iter 0]
    (if (= str1 (st/lower-case str2))
      iter
      (recur (randomize-string str1)
             (inc iter)))))

(defn -main
  [& args]
  (let [res (bogo (first args)
                  (second args))]
    (println (format "%d iterations" res))))


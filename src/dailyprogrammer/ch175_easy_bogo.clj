(ns dailyprogrammer.ch175-easy-bogo
  (:require [clojure.string :as st]))

(defn- remove-index
  [col i]
  (let [ex-i (inc i)]
    (if (> ex-i (count col))
      col
      (concat (take i col)
              (drop ex-i col)))))

(defn- randomize-string
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
  [str1 str2]
  (loop [str1 str1
         iter 0]
    (println str1 " | " str2)
    (if (= str1 (st/lower-case str2))
      iter
      (recur (randomize-string str1)
             (inc iter)))))

(defn -main
  [& args]
  (println (bogo (first args) (second args))))


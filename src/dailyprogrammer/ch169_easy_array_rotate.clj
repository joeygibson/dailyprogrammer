(ns dailyprogrammer.ch169-easy-array-rotate
  (:require [clojure.string :as string]))

;; ch169_easy_array_rotate.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/29i9jw/6302014_challenge_169_easy_90_degree_2d_array/
;; which rotates a matrix of numbers
;;
;; To run:
;; lein run -m dailyprogrammer.ch169-easy-array-rotate resources/ch169-easy-10-by-10.txt
;;

(defn- get-array-from-file
  "Load the data from the specified file into a nested vector,
  converting the strings to integers."
  [file-name]
  (let [contents (.trim (slurp file-name))
        lines (string/split contents #"\n")]
    (vec
      (for [line lines]
        (map #(Integer/parseInt %) (string/split line #"\s+"))))))

(defn- rotate-once
  "Rotate the matrix 90 degrees clockwise"
  [matrix]
  (apply map (fn [& args]
               (reverse args)) matrix))

(defn- rotate
  "Rotate the given matrix the specified number of times. The one-arg
  version rotates it once."
  ([matrix] (rotate matrix 1))
  ([matrix times]
   (loop [matrix matrix
          times times]
     (if (= times 0)
       matrix
       (recur (rotate-once matrix) (dec times))))))

(defn- print-matrix
  "Pretty-print the matrix without the Clojure type indicators"
  [matrix]
  (doseq [line matrix]
    (println (string/join " " line))))

(defn -main
  [& args]
  (let [matrix (get-array-from-file (first args))]
    (dotimes [i 4]
      (print-matrix (rotate matrix i))
      (println))))

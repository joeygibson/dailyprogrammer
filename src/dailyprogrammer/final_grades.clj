(ns dailyprogrammer.final-grades
  (:require [clojure.string :as string]))

;; final_grades.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/28gq9b/6182014_challenge_167_intermediate_final_grades/
;; 
(defn get-data-from-file
  "Reads the data file, returning a vector of data for each line in the file."
  [file-name]
  (let [raw-contents (slurp file-name)
        lines (string/split raw-contents #"\n")]
    (map #(rest (re-find #"\s*(\w+)\s*,\s*([^0-9]+?)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)" %)) lines)))

(defn- string->int
  "Use Java's Integer/parseInt to create ints from strings"
  [str]
  (Integer/parseInt str))

(defn- convert-scores-to-proper-type
  "Convert all the scores from strings to ints."
  [records]
  (for [record records
        :let [prefix (take 2 record)
              scores (drop 2 record)]]
    (concat prefix (map string->int scores))))

(defn- compute-grade-average
  "Compute the mean of a student's grades"
  [grades]
  (let [sum (reduce + grades)
        num (count grades)]
    (double (/ sum num))))

(defn- compute-letter-grade
  "Compute the correct letter grade for the student's average score"
  [score]
  "A")

(defn- format-grades
  "Format the student naem and grade for display"
  [student-info]
  (let [[first-name last-name & grades] student-info
        average (compute-grade-average grades)
        letter-grade (compute-letter-grade average)
        grades-str (apply (partial format "%d %d %d %d %d") grades)]
    (format "%s %s (%f%%) (%s) %s" first-name last-name average letter-grade grades-str)))

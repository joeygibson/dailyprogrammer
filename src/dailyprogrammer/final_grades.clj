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
    (Math/round (double (/ sum num)))))

(defn- between?
  "Determines if the number is between two other numbers"
  [num min max]
  (and (>= num min)
       (< num max)))

(defn- compute-plus-or-minus
  "Decides if there should be a + or - with the letter grade.
  A + for the top 30 percentile, and a - for the bottom 30."
  [score letter]
  (let [perc (rem score 10)
        plus-or-minus (cond
                        (<= perc 3) "-"
                        (and (>= perc 7)
                             (not (= letter "A"))) "+"
                        :else "")]
    (format "%s%s" letter plus-or-minus)))

(defn- compute-letter-grade
  "Compute the correct letter grade for the student's average score"
  [score]
  (cond
    (between? score 90 101) (compute-plus-or-minus score "A")
    (between? score 80 90) (compute-plus-or-minus score "B")
    (between? score 70 80) (compute-plus-or-minus score "C")
    (between? score 60 70) (compute-plus-or-minus score "D")
    :else "F"))

(defn- process-student
  "Format the student naem and grade for display"
  [student-info]
  (let [[first-name last-name & grades] student-info
        average (compute-grade-average grades)
        letter-grade (compute-letter-grade average)
        sorted-grades (sort grades)]
    [first-name last-name average letter-grade sorted-grades]))

(defn- format-student
  "Pretty-prints the data for each student"
  [student-info]
  (let [[first-name last-name average letter-grade grades] student-info
        grades-str (apply (partial format "%3d %3d %3d %3d %3d") grades)]
    (format "%-10s %-10s %3d%% %-2s %s" first-name last-name average letter-grade grades-str)))

(defn process-grades
  "Process all the grade records from the specified file"
  [file-name]
  (let [data (get-data-from-file file-name)
        records (convert-scores-to-proper-type data)
        processed-students (map process-student records)
        sorted-students (sort-by #((juxt second first) (drop 1 %)) processed-students)]
    (map format-student (reverse sorted-students))))

(defn- print-grades
  "Prints the grades after all the works is done"
  [records]
  (doseq [record records]
    (println record)))

(print-grades (process-grades "resources/final_grades.txt"))
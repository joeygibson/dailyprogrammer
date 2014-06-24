(ns dailyprogrammer.final-grades-generator
  (:require [clojure.string :as string]))

;; final_grades_generator.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/28vgej/6232014_challenge_168_easy_final_grades_test_data/
;; which provides data for final_grade.clj to operate on.
;;
;; To run:
;; lein run -m dailyprogrammer.final-grades-generator <number of records> [output file]
;;

(defn- generate-random-names
  "Returns an infinite lazy sequence of names from the specified file,
  in the format provided by the Social Security Administration."
  [& file-names]
  (let [raw-contents (mapcat slurp file-names)
        contents (apply str raw-contents)
        lines (string/split contents #"\n")
        names (map #(first (string/split % #"\s+")) lines)
        cnt (count names)
        random-name (fn []
                      (string/capitalize (nth names (rand-int cnt))))]
    (repeatedly random-name)))

(defn- generate-student-and-grade
  [first-names last-names grades]
  (repeatedly #(vec [(take 1 first-names)
                     (take 1 last-names)
                     (take 5 grades)])))

;; Sequences
(def first-names (generate-random-names "resources/dist.male.first.txt" "resources/dist.female.first.txt"))
(def last-names (generate-random-names "resources/dist.all.last.txt"))
(def grades (repeatedly #(nth (range 50 101) (rand-int 50)))) ;; This needs some work
(def students-and-grades (generate-student-and-grade first-names last-names grades))

;;(take 1 first-names)
;;(take 1 students-and-grades)

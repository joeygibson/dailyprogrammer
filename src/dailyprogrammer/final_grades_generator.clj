(ns dailyprogrammer.final-grades-generator
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

;; final_grades_generator.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/28vgej/6232014_challenge_168_easy_final_grades_test_data/
;; which provides data for final_grade.clj to operate on.
;;
;; To run:
;; lein run -m dailyprogrammer.final-grades-generator <number of records> [output file]
;;

(defn- generate-random-name
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

(defn- generate-random-full-name
  "Generates a person's full name (first-name last-name)"
  [first-names last-names]
  [(first (take 1 first-names))
   (first (take 1 last-names))])

(defn- generate-non-repeating-names
  "Generates num names, with no duplicates"
  [num first-names last-names]
  (loop [i 0
         first-names first-names
         last-names last-names
         res #{}]
    (if (= i num)
      res
      (let [name [(first first-names)
                  (first last-names)]]
        (recur (inc i)
               (rest first-names)
               (rest last-names)
               (conj res name))))))

(defn- format-student-and-grades
  "Pretty-print a single student's names and grades in the correct format"
  [record]

  (let [[name-chunk grades] record
        [first-name last-name] name-chunk
        grades-string (apply (partial format "%s %s %s %s %s") grades)]
    (format "%s, %s %s" first-name last-name grades-string)))

(defn- show-usage
  "Print a helpful usage message"
  []
  (println "Usage: final-grades-generator <number> [output-file]"))

(defn- generate-and-format-students-and-grades
  "The main workflow of the program"
  [first-names last-names grades-seq num]
  (let [names (generate-non-repeating-names num first-names last-names)
        grades (partition 5 (take (* 5 num) grades-seq))
        names-and-grades (map vector names grades)]
    (map format-student-and-grades names-and-grades)))

(defn- output-students-and-grades
  "Send the records to the given file name or *out*"
  [records file-name]
  (binding [*out* (if (nil? file-name)
                    *out*
                    (io/writer file-name))]
    (doseq [record records]
      (println record))))

;; Sequences
(def first-names (generate-random-name "resources/dist.male.first.txt" "resources/dist.female.first.txt"))
(def last-names (generate-random-name "resources/dist.all.last.txt"))
(def grades (repeatedly #(nth (range 50 101) (rand-int 50)))) ;; This needs some work

(defn -main
  [& args]
  (if (< (count args) 1)
    (show-usage)
    (let [num (Integer/parseInt (first args))
          file-name (second args)
          students-and-grades (generate-and-format-students-and-grades first-names
                                                                       last-names grades num)]
      (output-students-and-grades students-and-grades file-name))))





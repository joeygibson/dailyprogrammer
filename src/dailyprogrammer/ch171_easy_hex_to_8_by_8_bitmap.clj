(ns dailyprogrammer.ch171-easy-hex-to-8-by-8-bitmap
  (:require [clojure.string :as string]))

;; ch171_easy_hex_to_8_by_8_bitmap.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/2ao99p/7142014_challenge_171_easy_hex_to_8x8_bitmap/
;; which prints "pictures" based on an array of hex values
;;
;; To run:
;; lein run -m dailyprogrammer.ch171-easy-hex-to-8-by-8-bitmap resources/ch171-easy-input-1.txt
;;

(defn- get-numbers-from-file
  "Gets each bitmap's numbers from the given file"
  [file-name]
  (let [contents (string/trim (slurp file-name))
        lines (string/split contents #"\n")]
    (for [line lines]
      (string/split line #"\s+"))))

(defn- get-bits
  "Get the individual bits of the passed-in hex value"
  [x]
  (loop [x x
         i 7
         res '()]
    (if (< i 0)
      res
      (let [cur (bit-and x 0x1)]
        (recur (bit-shift-right x 1)
               (dec i)
               (conj res cur))))))

(defn- format-bit
  "Return an x for a 1, and a space for a 0"
  [bit]
  (if (= bit 1)
    "x"
    " "))

(defn- format-bits
  "Returns a string of x's and spaces based on the bit values"
  [bits]
  (let [string-bits (map format-bit bits)]
    (string/join "" string-bits)))

(defn- generate-bitmap
  "Generates the 'picture' in an 8x8 'bitmap'"
  [numbers]
  (for [num numbers
        :let [num (Integer/parseInt num 16)
              bits (get-bits num)]]
    (format-bits bits)))

(defn- print-bitmap
  "Actually displays the 'picture' on the console"
  [numbers]
  (let [bitmap (generate-bitmap numbers)]
    (doseq [row bitmap]
      (println row))))

(defn -main
  [& args]
  (let [pictures (get-numbers-from-file (first args))]
    (doseq [pic pictures]
      (print-bitmap pic)
      (println))))

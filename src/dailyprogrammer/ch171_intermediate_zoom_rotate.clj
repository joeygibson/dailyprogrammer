(ns dailyprogrammer.ch171-intermediate-zoom-rotate
  (:require [clojure.string :as string]))

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

(defn- zoom-in
  "Zooms in the given bitmap by 2x"
  [bitmap]
  (let [doubled-rows (mapcat #(vector % %) bitmap)]
    (for [row doubled-rows]
      (apply str (map #(format "%s%s" % %) row)))))

(defn- zoom-out
  "Zooms out by 2x"
  [bitmap]
  (let [partitioned-rows (partition 2 bitmap)
        halved-rows (map first partitioned-rows)]
    (for [row halved-rows
          :let [partitioned-row (partition 2 row)
                halved-row (map first partitioned-row)]]
      (apply str halved-row))))

(defn- zoom-bitmap
  "Zoom in the given bitmap up to 4 times"
  [bitmap factor]
  (let [zoom-out? (< factor 0)]
    (loop [bitmap bitmap
           factor (if (> factor 2)
                    2
                    factor)]
      (if (= factor 0)
        bitmap
        (recur (if zoom-out? (zoom-out bitmap)
                             (zoom-in bitmap))
               (dec factor))))))

(defn -main
  [& args]
  (let [pictures (get-numbers-from-file (first args))]
    (doseq [pic pictures]
      (print-bitmap pic)
      (println))))

(let [all-nums (get-numbers-from-file "resources/ch171-easy-input-1.txt")
      one-row (first all-nums)
      bitmap (generate-bitmap one-row)
      twox-bitmap (zoom-bitmap bitmap 1)
      zo-bitmap (zoom-bitmap twox-bitmap -1)]
  (doseq [row twox-bitmap]
    (println row)))

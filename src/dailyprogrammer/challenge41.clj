(ns dailyprogrammer.challenge41
  (:use [clojure.string :only [trim]]))

;; challenge41.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/shp28/4192012_challenge_41_easy/
;;

;; Simple version, no wrapping
(defn asciify [line]
  (let [len (count line)
        stars (apply str (repeat (+ 4 len) "*"))
        empty-line (apply str (repeat len " "))]
    (println stars)
    (println "*" empty-line "*")
    (println "*" line "*")
    (println "*" empty-line "*")
    (println stars)))

;; Bonus version, wraps long lines
(defn split-long-line [line max-chars]
  (if (> (count line) max-chars)
    (let [sub-line (subs line 0 max-chars)
          split-pos (.lastIndexOf sub-line " ")]
      (if (> split-pos 0)
        (let [[fst rem] (split-at split-pos line)]
          (list (trim (apply str fst))
                (if (> (count rem) max-chars)
                  (split-long-line (apply str rem) max-chars)
                  (trim (apply str rem)))))
        (list (trim sub-line))))
    (list (trim line))))

(defn asciify-with-wrap [line max-chars]
  (let [lines (flatten (split-long-line line max-chars))
        len (apply max (map count lines))
        stars (apply str (repeat (+ 4 len) "*"))
        empty-line (apply str (repeat len " "))]
    (println stars)
    (println "*" empty-line "*")
    (doseq [l lines]
      (let [trimmed-l (trim l)
            len (count trimmed-l)
            padding (if (< len max-chars)
                      (apply str (repeat (- max-chars len 1) " ")))
            padded-l (str l padding)]
        (println "*" padded-l "*")))
    (println "*" empty-line "*")
    (println stars)))

;; Main
(asciify "So long and thanks for all the fish")
(asciify-with-wrap "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                   80)

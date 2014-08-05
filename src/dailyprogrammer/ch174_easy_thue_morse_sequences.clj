(ns dailyprogrammer.ch174-easy-thue-morse-sequences)

;; ch174_easy_thue-morse-sequencxes.clj -- Joey gibson <joey@joeygibson.com>
;; Solution for http://www.reddit.com/r/dailyprogrammer/comments/2cld8m/8042014_challenge_174_easy_thuemorse_sequences/
;; which computes the nth Thue-Morse sequence.
;;
;; To run:
;; lein run -m dailyprogrammer.ch174-easy-thue-morse-sequences [n]
;;

(defn- complement-seq
  "Reverses each element of the passed-in seq"
  [s]
  (mapv #(if (zero? %)
          1
          0) s))

(defn thue-morse
  "Computes the nth Thue-Morse sequence."
  [n]
  (loop [n n
         bits [0]]
    (if (zero? n)
      (apply str bits)
      (recur (dec n)
             (concat bits (complement-seq bits))))))

(defn -main
  [& args]
  (dotimes [i (if (> (count args) 0)
                (Integer/parseInt (first args))
                1)]
    (let [num (thue-morse i)]
      (println num))))
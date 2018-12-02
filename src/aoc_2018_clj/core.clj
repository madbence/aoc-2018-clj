(ns aoc-2018-clj.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn ->int [c]
  (cond
    (string? c) (Integer/parseInt c)
    (nil? c) nil
    :else (- (int c) 48)))

(defn p01a [lines]
  (reduce + 0 (map ->int lines)))

(defn p01b [lines]
  (loop [history #{}
         freqs (reductions + (cycle (map ->int lines)))]
    (let [freq (first freqs)]
      (if (contains? history freq)
        freq
        (recur (conj history freq) (next freqs))))))

(defn -main
  "Advent of Code 2018"
  [& args]
  (println ((resolve (symbol "aoc-2018-clj.core" (first args))) (str/split (slurp *in*) #"\n"))))

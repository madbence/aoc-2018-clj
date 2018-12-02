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

(defn p02a [lines]
  (let [letter-freqs (map (fn [line]
                            (reduce (fn [num-found c]
                                      (conj num-found (count (filter #(= %1 c) line))))
                                    #{} line)) lines)
        [twos threes] (reduce (fn [[twos threes] freqs]
                                [(+ twos (if (contains? freqs 2) 1 0))
                                 (+ threes (if (contains? freqs 3) 1 0))])
                              [0 0] letter-freqs)]
    (* twos threes)))

(defn p02b [lines]
  (let [matching-chars (fn [[a b]] (filter (fn [[a b]] (= a b)) (map vector a b)))
        differs-by-one? (fn [[a b]] (= (count (filter (fn [[a b]] (not= a b)) (map vector a b))) 1))
        pairs (mapcat (fn [line] (filter differs-by-one? (map (partial vector line) lines))) lines)]
    (str/join (map #(first %1) (matching-chars (first pairs))))))

(defn -main
  "Advent of Code 2018"
  [& args]
  (println ((resolve (symbol "aoc-2018-clj.core" (first args))) (str/split (slurp *in*) #"\n"))))

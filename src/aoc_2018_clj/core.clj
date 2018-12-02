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
  (let [has-freq? (fn [n word] (some #(= % n) (vals (frequencies word))))]
    (* (count (filter (partial has-freq? 2) lines))
       (count (filter (partial has-freq? 3) lines)))))

(defn p02b [lines]
  (->> (for [a lines b lines] [a b])                ; every possible pair from `lines`
       (map #(apply map vector %))                  ; pairs of chars in each line pair
       (filter #(-> (filter (partial apply not=) %) ; filter for pairs when they don't match
                    (count)                         ; count them
                    (= 1)))                         ; only line-pairs with 1 mismatching chars should be part of the result
       (first)                                      ; take the first such line-pair
       (filter #(apply = %))                        ; filter for matching char-pairs
       (map first)                                  ; take the first char from those pairs (they are the same)
       (str/join)))                                 ; join the the common chars into a string

(defn -main
  "Advent of Code 2018"
  [& args]
  (println ((resolve (symbol "aoc-2018-clj.core" (first args))) (str/split (slurp *in*) #"\n"))))

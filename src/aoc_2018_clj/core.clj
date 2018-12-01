(ns aoc-2018-clj.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn ->int [c]
  (cond
    (string? c) (Integer/parseInt c)
    (nil? c) nil
    :else (- (int c) 48)))

(defn p01a []
  (loop [freq 0]
    (if-let [n (->int (read-line))]
      (recur (+ freq n))
      freq)))

(defn p01b []
  (let [adjustments (loop [xs []]
                      (if-let [x (->int (read-line))]
                        (recur (conj xs x))
                        xs))]
    (loop [freq 0
           history #{}
           adj (cycle adjustments)]
      (let [next-freq (+ freq (first adj))]
        (if (contains? history next-freq)
          next-freq
          (recur next-freq
                 (conj history freq)
                 (next adj)))))))

(defn -main
  "Advent of Code 2018"
  [& args]
  (println ((resolve (symbol "aoc-2018-clj.core" (first args))))))

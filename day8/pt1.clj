(ns aoc2020-day8-pt1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [union]]))

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(def signs {"-" -
            "+" +})

(defn parse [l]
  (let [[_ op sign n] (re-matches #"([^\s]+) ([+|-])(\d+)" l)]
    [op (get signs sign) (Integer. n)]))

(defn calc-pointer [current-pointer [op sign n]]
  (condp = op
    "nop" (inc current-pointer)
    "acc" (inc current-pointer)
    "jmp" (sign current-pointer n)
    (throw (Exception. "Bad operation"))))

(defn accumulate [acc [op sign n]]
  (cond-> acc
    (= "acc" op) (sign n)))

(defn execute [executed-instructions pointer acc all-instructions]
  (if (some #{pointer} executed-instructions)
    acc
    (let [current-instruction (nth all-instructions pointer)]
      (execute 
        (conj executed-instructions pointer)
        (calc-pointer pointer current-instruction)
        (accumulate acc current-instruction)
        all-instructions))))

(defn solve [input] 
  (->> input
       (map parse)
       (execute [] 0 0)))

(println (solve (input)))

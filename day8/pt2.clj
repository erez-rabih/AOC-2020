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

(defn replace-instruction [[op sign n]]
  (let [replacement-op (if (= op "nop") "jmp" "nop")]
    [replacement-op sign n]))

(defn execute [executed-instructions pointer acc replaced? all-instructions]
  (when-not (some #{pointer} executed-instructions)
    (if (>= pointer (count all-instructions))
      acc
      (if-let [current-instruction (nth all-instructions pointer)]
        (let [no-replacement (execute 
                               (conj executed-instructions pointer)
                               (calc-pointer pointer current-instruction)
                               (accumulate acc current-instruction)
                               replaced?
                               all-instructions)]
          (if (and (not replaced?) (not= "acc" (first current-instruction)))
            (let [replacement-instruction (replace-instruction current-instruction)
                  with-replacement (execute
                                     (conj executed-instructions pointer)
                                     (calc-pointer pointer replacement-instruction)
                                     (accumulate acc replacement-instruction)
                                     true
                                     all-instructions)]
              (or no-replacement with-replacement))
            no-replacement))
        acc))))

(defn solve [input] 
  (->> input
       (map parse)
       (execute [] 0 0 false)))

(println (solve (input)))

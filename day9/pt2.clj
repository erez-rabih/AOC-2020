(ns aoc2020-day9-pt2
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [union]]))

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn find-sum [coll target-value]
  (first (filter true? (for [[idx1 v1] coll
                             [idx2 v2] coll]
                         (when (not= idx1 idx2)
                           (= (+ v1 v2) target-value))))))
                           
(defn has-sum? [preamble input]
  (fn [[idx v]]
    (find-sum
      (subvec input (- idx preamble) idx)
      v)))

(defn find-contiguous 
  ([coll target-value] (find-contiguous coll target-value 0 0 []))
  ([coll target-value starting-idx current-idx acc]
   (loop [sidx starting-idx
          cidx current-idx
          acc []]
     (cond
       (< (count acc) 2) (recur sidx (inc cidx) (conj acc (last (nth coll cidx)))) 
       (> (apply + acc) target-value) (recur (inc sidx) (inc sidx) [])
       (< (apply + acc) target-value) (recur sidx (inc cidx) (conj acc (last (nth coll cidx))))
       (= (apply + acc) target-value) acc))))

(defn sum [coll] (+ (apply min coll) (apply max coll)))
   
(defn solve [input preamble] 
  (let [parsed-input (map-indexed (fn [idx v] [idx (Long. v)]) input)]
    (->> parsed-input
         (drop-while (fn [[idx _]] (< idx preamble)))
         (remove (has-sum? preamble (vec parsed-input)))
         first
         last
         (find-contiguous parsed-input)
         sum)))

(println (solve (input) 25))


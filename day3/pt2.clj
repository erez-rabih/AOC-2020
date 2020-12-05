(ns aoc2020-day3-pt2
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]))

(def tree "#")

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn- tree? [right-factor down-factor]
  (fn [row-number row]
    (when (and (not (zero? row-number)) (zero? (mod row-number down-factor)))
      (let [row-length (count row)
            x-position (mod (* (/ right-factor down-factor) row-number) row-length)
            chr (get row x-position)]
        (= chr tree)))))

(defn solve [input right-factor down-factor]
  (->> input
       (map #(split % #""))
       (map-indexed (tree? right-factor down-factor))
       (filter true?)
       count))

(println (apply * (map #(apply (partial solve (input)) %) [[1 1] [3 1] [5 1] [7 1] [1 2]])))

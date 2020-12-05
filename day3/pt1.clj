(ns aoc2020-day3-pt1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]))


(def tree "#")
(def right-factor 3)

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn- tree? 
  [row-number row]
  (let [row-length (count row)
        x-position (mod (* right-factor row-number) row-length)
        chr (get row x-position)]
    (= chr tree)))

(defn solve [input]
  (->> input
       (map #(split % #""))
       (map-indexed tree?)
       (filter true?)
       count))

(println (solve (input)))

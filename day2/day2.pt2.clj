(ns aoc2020-day2-pt2
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]))

(defn- input []
   (with-open [rdr (io/reader "day2.input")]
     (vec (line-seq rdr))))

(defn- parse [line]
  (re-matches #"(\d+)-(\d+) (.): (.+)" line))

(defn valid? [line]
  (let [[_ pos1 pos2 chr pwd] (parse line)
        pos1 (dec (Integer. pos1))
        pos2 (dec (Integer. pos2))
        chrs (split pwd #"")]
    (->> [pos1 pos2]
         (filter #(= (nth chrs %) chr))
         count
         dec
         zero?)))

(defn solve [input]
  (->> input
       (filter valid?)
       count))

(println (solve (input)))

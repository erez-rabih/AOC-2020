(ns aoc2020-day2-pt1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]))

(defn- input []
   (with-open [rdr (io/reader "day2.pt1.input")]
     (vec (line-seq rdr))))

(defn- count-occurrences 
  [c s] 
  (-> s
      (split #"")
      (->> (filter #(= c %)))
      count))

(defn- parse [line]
  (re-matches #"(\d+)-(\d+) (.): (.+)" line))

(defn valid? [line]
  (let [[_ min max chr pwd] (parse line)
        min (Integer. min)
        max (Integer. max)]
    (<= min (count-occurrences chr pwd) max)))

(defn solve [input]
  (->> input
       (filter valid?)
       count))

(println (solve (input)))

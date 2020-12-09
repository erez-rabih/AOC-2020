(ns aoc2020-day6-pt2
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]))

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn parse [input]
  (->> input
       (partition-by empty?)
       (remove (comp empty? first))))

(defn solve [input]
  (->> input
       parse

       (map (fn [x] (map set x)))
       (map #(apply clojure.set/intersection %))
       (map count)
       (apply +)))

(println (solve (input)))

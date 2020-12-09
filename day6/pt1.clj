(ns aoc2020-day6-pt1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]))

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn parse [input]
  (->> input
       (partition-by empty?)
       (remove (comp empty? first))
       (map #(clojure.string/join "" %))))

(defn solve [input]
  (->> input
       parse
       (map distinct)
       (map count)
       (apply +)))

(println (solve (input)))

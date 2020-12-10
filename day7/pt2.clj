(ns aoc2020-day7-pt2
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]))

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn parse-tail [s]
  (let [[_ n c] (re-matches #"\s*(\d+) (.+) bag.*" s)]
    (if (and n c) [c (Integer. n)] [])))

(defn parse [acc l]
  (let [[_ color tail] (re-matches #"^(.+) bags contain (.*)$" l)
        tail-parts (clojure.string/split tail #",")
        parsed-tail (map parse-tail tail-parts) ]
  (assoc acc color parsed-tail)))

(defn collect-inner-bags [bag conversion-map]
  (let [direct-bags (get conversion-map bag)]
    (reduce (fn [acc [bag-color bag-count]]
              (if (and bag-color bag-count) 
                (+ acc bag-count (* bag-count (collect-inner-bags bag-color conversion-map)))
                0)) 
              0 direct-bags)))

(defn solve [input]
  (->> input
       (reduce parse {})
       (collect-inner-bags "shiny gold")))

(println (solve (input)))

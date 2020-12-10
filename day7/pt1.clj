(ns aoc2020-day7-pt1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]))

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn parse-tail [s]
  (let [[_ n c] (re-matches #"\s*(\d+) (.+) bag.*" s)]
    (if (and n c) [c n] [])))

(defn parse [acc l]
  (let [[_ color tail] (re-matches #"^(.+) bags contain (.*)$" l)
        tail-parts (clojure.string/split tail #",")
        parsed-tail (map parse-tail tail-parts) ]
  (assoc acc color parsed-tail)))

(defn can-contain? [conversion-map bags target-bag]
  (when-not (empty? bags)
    (or (some #{target-bag} (set (map first bags)))
        (can-contain? conversion-map 
                      (reduce (fn [acc bag] (concat acc (get conversion-map (first bag)))) [] bags)
                      target-bag))))

(defn solve [input]
  (let [conversion-map (reduce parse {} input)]
    (count (filter (fn [[_ v]] (can-contain? conversion-map v "shiny gold")) conversion-map))))

(println (solve (input)))

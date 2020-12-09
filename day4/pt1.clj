(ns aoc2020-day4-pt1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn- parse-single-passport 
  [s]
  (into {} (map #(clojure.string/split % #":") (clojure.string/split s #" "))))

(defn to-passports [input]
  (->> input
       (partition-by empty?)
       (remove (comp empty? first))
       (map #(clojure.string/join " " %))
       (map parse-single-passport)))

(defn valid? 
  [passport]
  (empty? (difference required-fields (set (keys passport)))))

(defn solve [input]
  (->> input
       to-passports
       (filter valid?)
       count))

(println (solve (input)))

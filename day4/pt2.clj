(ns aoc2020-day4-pt2
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn- between [x y] 
  (fn [v]
    (when (re-matches #"^\d{4}$" v)
      (let [int-value (Integer. v)]
        (<= x int-value y)))))

(defn cm? [s] (let [[_ i] (re-matches #"^(\d{3})cm$" s)] (when i (<= 150 (Integer. i) 193))))
(defn in? [s] (let [[_ i] (re-matches #"^(\d{2})in$" s)] (when i (<= 59 (Integer. i) 76))))
(defn hair-color? [s] (re-matches #"^#[0-9a-f]{6}$" s))
(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(defn valid-id? [s] (re-matches #"^\d{9}$" s))

(def validations {"byr" (between 1920 2002)
                  "iyr" (between 2010 2020)
                  "eyr" (between 2020 2030)
                  "hgt" (fn [s] (or (cm? s) (in? s)))
                  "hcl" hair-color?
                  "ecl" (fn [s] (some #{s} eye-colors))
                  "pid" valid-id?})

(defn- parse-single-passport 
  [s]
  (into {} (map #(clojure.string/split % #":") (clojure.string/split s #" "))))

(defn to-passports [input]
  (->> input
       (partition-by empty?)
       (remove (comp empty? first))
       (map #(clojure.string/join " " %))
       (map parse-single-passport)))

(defn validate-values
  [p]
  (let [errors (reduce (fn [errors [k v]]
                         (if (v (get p k))
                           errors
                           (conj errors k))) [] validations)]
    (empty? errors)))

(defn valid? 
  [passport]
  (and (empty? (difference required-fields (set (keys passport))))
       (validate-values passport)))

(defn solve [input]
  (->> input
       to-passports
       (filter valid?)
       count))

(println (solve (input)))

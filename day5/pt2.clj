(ns aoc2020-day5-pt1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]))

(defn- input []
   (with-open [rdr (io/reader "input")]
     (vec (line-seq rdr))))

(defn find-in-range 
  [v lower-bound upper-bound l u]
  (if (empty? v)
    lower-bound
    (let [x (first v)
          mid (/ (+ lower-bound upper-bound) 2)]
      (if (= x l)
        (find-in-range (rest v) lower-bound (-> mid Math/floor int) l u)
        (find-in-range (rest v) (-> mid Math/ceil int) upper-bound l u)))))

(defn row-number [characters]
  (let [v (subvec characters 0 7)]
    (find-in-range v 0 127 "F" "B")))

(defn column-number [characters]
  (let [v (subvec characters 7)]
    (find-in-range v 0 7 "L" "R")))

(row-number (clojure.string/split "FBFBBFFRLR" #""))
(column-number (clojure.string/split "FBFBBFFRLR" #""))


(defn seat-id [code]
  (let [characters (split code #"")]
    (+ (column-number characters) 
       (* 8 (row-number characters)))))

(defn solve [input]
  (->> input
       (map seat-id)
       sort
       (partition 2 1)
       (filter (fn [x] (> -1 (apply - x))))
       first
       first
       (+ 1)))

(println (solve (input)))

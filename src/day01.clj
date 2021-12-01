(ns day01
  (:require [clojure.java.io :as io]))

(def data-sample
  (-> (io/resource "day01sample.txt")
      io/reader
      line-seq))


(def data-a
  (-> (io/resource "day01a.txt")
      io/reader
      line-seq))


(->> data-a
     (map #(Long/parseLong %))
     (partition 2 1)
     (filter #(< (nth % 0) (nth % 1)))
     count
     )
;; => 1233

(->> data-a
     (map #(Long/parseLong %))
     (partition 3 1)
     (map #(reduce + %))
     (partition 2 1)
     (filter #(< (nth % 0) (nth % 1)))
     count
     )
;; => 1275

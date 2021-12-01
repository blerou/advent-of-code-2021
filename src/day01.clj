(ns day01
  (:require [clojure.java.io :as io]))

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

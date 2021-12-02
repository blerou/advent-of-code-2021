(ns day02
  (:require [clojure.java.io :as io]))

(def data-sample
  (->> (io/resource "day02.sample")
       slurp
       (re-seq #"[^\n ]+")))

(def data
  (->> (io/resource "day02.txt")
       slurp
       (re-seq #"[^\n ]+")))

(->> data
     (partition 2)
     (map (fn [[dir val]] [dir (Long/parseLong val)]))
     (reduce (fn [res [dir val]]
               (case dir
                 "forward" (update res :hor #(+ % val))
                 "down" (update res :dep #(+ % val))
                 "up" (update res :dep #(- % val))))
             {:hor 0, :dep 0})
     vals
     (reduce *)
     )
;; => 1947824

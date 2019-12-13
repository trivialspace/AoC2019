(ns day2.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0")
;(def myvec (map #(Integer/parseInt %) (str/split input #",")))

(defn getvec
  [input]
  (as-> input v
    (str/split v #",")
    (map #(Integer/parseInt %) v)
    (vec v)))

(defn computeValue
  [position myvec]
  (cond
    (= (get myvec position) 1) (+ (get myvec (get myvec (+ position 1))) (get myvec (get myvec (+ position 2))))
    (= (get myvec position) 2) (* (get myvec (get myvec (+ position 1))) (get myvec (get myvec (+ position 2))))
    :else "error"))

(defn process
  [position myvec]
  (assoc myvec (get myvec (+ position 3)) (computeValue position myvec)))

(defn start
  [position myvec]
  (if (= (get myvec position) 99)
    myvec
    (do ;(println "stepping in")
        (start (+ position 4) (process position myvec)))))

(defn try2
  [noun verb myvec]
  (let [myvec1 (assoc myvec 1 noun)
        myvec2 (assoc myvec1 2 verb)]
    (get (start 0 myvec2) 0)))

(defn tryall
  [noun verb myvec]
  (if (= (try2 noun verb myvec) 19690720)
    (+ (* 100 noun) verb)
    (recur (rand-int 100) (rand-int 100) myvec))
  )
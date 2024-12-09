(ns day-8 (:require [clojure.string :as str ][ clojure.set :as set]))
(def text (slurp "data8.txt"))

(defn get-nodes "returns a list of the position of the nodes related to the given letter" [text letter] (
  let [lines (str/split-lines text)
       height (count lines)
       width (count (peek lines))] (
    loop [lines lines x-index 0 y-index 0 acc []] (
      if (empty? lines) acc
      (
       let [new-x (if (= x-index (dec width)) 0 (inc x-index))
            new-y (if (= x-index (dec width)) (inc y-index) y-index)
            new-lines (if (= x-index (dec width)) (pop lines) lines)
            new-acc (if (= (subs (peek lines) x-index (inc x-index)) letter) (conj acc [x-index y-index]) acc)]
         (recur new-lines new-x new-y new-acc)
       )
    )                                  
  )                                                                                                         
))

(defn get-antinodes-of "Returns a set of the position of the antinodes caused by the two given antennas" [pos1 pos2 text] (
  let [lines (str/split-lines text)
       height (count lines)
       width (count (peek lines))
       x1 (pos1 0)
       y1 (pos1 1)
       x2 (pos2 0)
       y2 (pos2 1)
       start (loop [x x1 y y1] (
                if (or (< x 0) (> x (dec width)) (< y 0) (> y (dec height))) [(- x (- x1 x2)) (- y (- y1 y2))]          
                    (recur (+ x (- x1 x2)) (+ y (- y1 y2)))
                )              
              )
       ] (loop [x (start 0) y (start 1) acc #{}] (
            if (or (< x 0) (> x (dec width)) (< y 0) (> y (dec height))) acc                            
                (recur (+ x (- x2 x1)) (+ y (- y2 y1)) (set/union acc (set [[x y]])))
            )
          )                                                                                                                                                                                                                                                                                                 
))

(defn get-antinodes "Returns a set of the position of the antinodes related to the letter given" [text letter] (
  let [lines (str/split-lines text)
       height (count lines)
       width (count (peek lines))
       nodes (get-nodes text letter)] (
    loop [nodes nodes acc #{}] (
        if (empty? nodes) acc (
          let [node1 (peek nodes)
               x1 (node1 0)
               y1 (node1 1)
               antinodes (loop [other (pop nodes) acc #{}](
                             if (empty? other) acc
                              (
                                recur (pop other) (set/union acc (get-antinodes-of node1 (peek other) text))
                               )
                           ))] (
            recur (pop nodes) (set/union acc antinodes)                 
          )
        )                          
    )                               
  )                                                                                                              
))

(defn get-all-antinodes "Is given a string and gives back a set of all the position of the antinodes" [text] (
  let [alphabet ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" 
                 "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                 "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"]] (
      loop [alphabet alphabet acc #{}] (
          if (empty? alphabet) acc (
            let [letter (peek alphabet)] (
              recur (pop alphabet) (set/union acc (get-antinodes text letter))                              
            )
          )
      )                                                                                                                    
    )                                                                                                         
))

(defn count-antinodes "count the number of antinodes created with the given antennas. the parameter given is a string" [text] (
  let [antinodes (get-all-antinodes text)] (count antinodes)                                                                                             
))


(println (str "Il y a " (count-antinodes text) " antinodes"))

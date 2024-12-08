(ns day-8 (:require [clojure.string :as str ][ clojure.set :as set]))
(def text (slurp "data8.txt"))

(defn get-antinodes "Returns a set of the position of the antinodes related to the letter given" [text letter] (
  let [height (count lines)
       lines (str/split-lines text)
       width (count (peek lines))] (
    loop [x-index 0 y-index 0] (
        if (= y-index)                          
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

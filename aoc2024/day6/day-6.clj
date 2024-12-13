(ns day6 (:require [clojure.string :as str] [clojure.set :as set]))
(def text (slurp "data6.txt"))

(defn find-start "Return the starting position of the guard of the given text and the associated direction as: [[x y] direction]" [text] (
  let [lines (str/split-lines text)
       height (count lines)
       length (count (peek lines))] (
    loop [x 0 y 0 lineslist lines] (
      if (or (empty? lineslist) (= y height)) (do (println "On a pas trouvé la position initiale du garde")) (
        let [character (subs (peek lineslist) x (inc x))
             newx (if (= x (dec length)) 0 (inc x))
             newy (if (= x (dec length)) (inc y) y)
             newlines (if (= x (dec length)) (pop lineslist) lineslist)] (
          cond (= character "^") [[x y] "UP"]
               (= character ">") [[x y] "RIGHT"]
               (= character "<") [[x y] "LEFT"]
               (= character "v") [[x y] "DOWN"]
               :else (recur newx newy newlines)
        )
      )                                             
  )                                                                                                                                         
)))

(defn get-obstacles "returns a list of all the obstacles" [text](
  let [lines (str/split-lines text)
       height (count lines)
       length (count (peek lines))] (
    loop [x 0 y 0 acc (list) lineslist (into '() lines)](
      if (or (= y height) (empty? lineslist)) acc
        (let [newx (if (= x (dec length)) 0 (inc x))
              newy (if (= x (dec length)) (inc y) y)
              newacc (if (= (subs (first lineslist) x (inc x)) "#") (conj acc [x y]) acc)
              newlines (if (= x (dec length)) (rest lineslist) lineslist)]
          (recur newx newy newacc newlines)
         )      
    )                                 
  )                                                             
))

(defn get-path "returns a list containing each position of the path of the guard and the direction he was facing at the time. ex: ([[2,1] 'UP'])" [text] (
  let [obstacles (get-obstacles text)
       start (find-start text)
       lines (str/split-lines text)
       height (count lines) 
       length (count (peek lines))] (
    loop [x ((start 0) 0) y ((start 0) 1) acc (list) direction (start 1)] (
      let [newx (cond (= direction "RIGHT") (inc x) (= direction "LEFT") (dec x) :else x)
           newy (cond (= direction "UP") (inc y) (= direction "DOWN") (dec y) :else y)
           is_obstacle (some #(= % [newx newy]) obstacles)
           newdirection (if is_obstacle (cond (= direction "RIGHT") "DOWN" (= direction "DOWN") "LEFT" (= direction "LEFT") "UP" (= direction "UP") "RIGHT") direction)
           real_newx (if is_obstacle x newx)
           real_newy (if is_obstacle y newy)
           newacc (if (some #(= [[x y] direction] %) acc) acc (conj acc [[x y] direction]))] (
          if (some #(= [[x y] direction] %) acc) [acc true]; looop
          (
            if (or (< real_newx 0) (> real_newx (dec length)) (< real_newy 0) (> real_newy (dec height))) [newacc false]; escaped
            (recur real_newx real_newy newacc newdirection)
          )
      )
    )
  )                                                                                                     
))

(defn is-loop "Checks if the guard is going to loop in the given map" [text] (
      (get-path text) 1                         
))

(defn place-obstacle "Returns a new text with an obstacle at the position given as argument" [text position] (
  let [lines (str/split-lines text)
       height (count lines)
       length (count (peek lines))
       x (position 0)
       y (dec (- length (position 1)))
       line (lines y)
       new_line (str (subs line 0 x) "#" (subs line (inc x) length))
       new_lines (assoc lines y new_line)] (
       str/join "\n" new_lines                                
  )                                    
))

(defn possible-loops "returns a set of the position where by putting an obstacle you create a loop in the guard path. It is given a string as an argument" 
  [text] (
  let [path ((get-path text) 0)
       start ((find-start text) 0)] (
    loop [positions_to_check path acc #{}] (
      if (empty? positions_to_check) acc (
        let [position ((first positions_to_check) 0)
             at_start (= position start)
             new_text (if at_start text (place-obstacle text position))
             is_loop (if at_start false (is-loop new_text))
             ](
          recur (rest positions_to_check) (if is_loop (set/union acc #{position}) acc)
          )
      )                                      
    )
  )
))
(println (str "Nombre de positions du chemin :" (count ((get-path text) 0))))

(def loops (possible-loops text))
(println loops)
(println (str "Nombre de positions pour faire des loops possibles :" (count loops)))


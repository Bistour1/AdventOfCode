(ns day-7 (:require [clojure.string :as str]))

(def text (slurp "data7.txt"))

(defn get-possibilities 
  "prends un vecteur de nombres et renvoie tous les résultats possibles en utilisant les opérations + et *    de gauche à droite sous la forme d'un vecteur." 
[numbers](
  loop [numbers (vec (rseq numbers)) current_list []] (
    let [current_number (if (empty? numbers) 0 (bigint (peek numbers)))
         new_list (if (empty? current_list) [current_number] (vec (flatten (conj 
                                                                             (mapv #(* current_number %) current_list) 
                                                                             (mapv #(+ current_number %) current_list)
                                                                             (mapv #(bigint(str % current_number)) current_list)))))
         ] (
      if (empty? numbers) current_list
        (recur (pop numbers) new_list)
    )                                     
  )                                                                                      
))

(defn is-calibrated "prends comme param!tre une calibration sous la forme d'un string exemple '190: 10 19 et vérifie s'il est calibrée'" [calibration] (
  let [splitted (str/split calibration #": ")
       goal_number (bigint (peek (pop splitted)))
       numbers (str/split (peek splitted) #" ")] (
      some #(= goal_number %) (get-possibilities numbers)      
    )                                                                  
))

(defn total-calibration "prends comme paramètres les calibrations possibles sous la forme d'un vecteur de string '190: 10 19' et compte le nombre qui sont calibrées" 
            [calibrations] (
          loop [calibrations calibrations acc 0] (
            if (empty? calibrations) acc
              (let [this_calibration (peek calibrations)
                    splitted (str/split this_calibration #": ")
                    goal_number (bigint (peek (pop splitted)))]
                  (recur (pop calibrations) (if (is-calibrated this_calibration) (+ acc goal_number) acc))
               )            
          )                  
))

(def to_calibrate (str/split-lines text))
(def total (total-calibration to_calibrate))
(println (str "Résultat de calibration: " total))


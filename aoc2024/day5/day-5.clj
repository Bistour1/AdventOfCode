(ns day-5 (:require [clojure.string :as str]))
(def text (slurp "data5.txt"))

(defn list-index-of [l elem] "returns the index of the elem" (loop [l l acc 0] (if (empty? l) (+ acc 3) (if (= (first l) elem) acc (recur (rest l) (+ acc 1))))))

(defn create_cond? "parses a text like 5|9 into a function" [txt](
    let [splitted (str/split txt #"\|") num1 (first splitted) num2 (first (rest splitted))] (
     fn [l] (or (<= (list-index-of l num1) (list-index-of l num2)) (or (>= (list-index-of l num1) (count l)) (>= (list-index-of l num2) (count l)) ) )     
     )                     
))

(defn get_condition? "combine all the conditions" [& conditions] (
  fn [l] (
    loop [condlist (first conditions)] (
      if (empty? condlist) true 
        (if-not ((create_cond? (first condlist)) l) false
          (recur (rest condlist))
         )      
    )
  ))  
)

(defn update_valid? [condition updt] ((get_condition? (str/split-lines condition)) updt))

(defn get_middle [l] (let [len (count l)](get l (/ (- len 1) 2))))

(defn repair [condition upd] (
  let [compx (comparator (fn [x y] ((get_condition? (str/split-lines condition)) (list x y))))]
  (sort compx upd)
))

(defn sum "gives the function sum for bool = true and incorect-sum for bool = false" [bool] ( fn [condition text] (
   loop [updates (str/split-lines text) acc 0](      
    if (empty? updates) acc
      (let [
        updte (str/split (first updates) #",") 
        add (if (not ((fn [%1 %2] (or (and %1 (not %2)) (and (not %1) %2))) bool (update_valid? condition updte))) (Integer/parseInt (get_middle (vec (repair condition updte)))) 0)]
        (recur (rest updates) (+ acc add))
       )
    )                            
)))

(def splitted (str/split text #"\n\n"))
(def condition_part (first splitted))
(def update_part (first (rest splitted)))
(println (str "Première étoile :" ((sum true) condition_part update_part)))
(println (str "Deuxième étoile :" ((sum false) condition_part update_part)))

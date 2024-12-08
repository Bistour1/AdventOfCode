(require '[clojure.string :as str])
(def text (slurp "input-day2.txt"))
(def reports (vec (str/split-lines text)))


(defn is_safe "checks if the report is safe" [report_values]
  (loop [report_values report_values last_value -1 state 0 acc 0] ; the state is 1 if ascending and -1 if descending.
    (                                                                                                                      ;acc is the position of the problem
     if (empty? report_values)  -1; -1 means true
      (let [
            current_value (Integer/parseInt (first report_values)) 
            gap (abs (- current_value last_value)); 
            gap_signe (- current_value last_value); positif si ascendant
            direction (= gap gap_signe); true si ça monte faux si ça descent
            new_state (if (= last_value -1) 0 (if direction 1 -1))
            ]
       ( if (and (not (= last_value -1)) (or (or (= gap 0) (> gap 3)) (not (or (= state new_state) (= state 0)) ))); Si (last_value != 1 and (abs(current_value - last_value) = 0))
           acc ; is false, return the index of error
           (recur (rest report_values) current_value new_state (+ acc 1))
           )
        )
     )
   )
)

(defn reverse_list " reverse a list" [l] (loop [l l reversed ()] (if (empty? l) reversed (recur (rest l) (conj reversed (first l))))))

(defn remove_index "remove an index of a list" [l index]
  (
   loop [l l counter () acc 0] (
    if (empty? l) counter(
       if (< index (+ acc 1)) (concat (reverse_list counter) (rest l))
        (recur (rest l) (conj counter (first l)) (+ acc 1))
       )                             
    )
)
)

(defn is_almost_safe "checks if the report is either safe or safe if we remove 1 data" [report_values]
  (
  let [result (is_safe report_values)]
  (if (= result -1 ) 
    true
    (or (= (is_safe (remove_index report_values (- result 1))) -1)
        (
         or (= (is_safe (remove_index report_values (- result 0))) -1)
          (= (is_safe (remove_index report_values (- result 2))) -1)
           )
     ) 
    )
   
   )
)
(defn count_safe "count the number of safe reports" [reps]
  (
   loop [reps reps acc 0] (
    if (empty? reps) acc
    (
     let [
          report (vec (str/split (first reps) #" "))
          add (if (= (is_safe report) -1) 1 0)
          ]    
     (recur (rest reps) (+ acc add))
    )
   )
  )
)
(defn count_almost_safe "count the number of almost safe reports" [reps]
  (
   loop [reps reps acc 0]
   (
    if (empty? reps) acc
    (
     let [
          report (vec (str/split (first reps) #" "))
          add (if (is_almost_safe report) 1 0)
          ]    
     (recur (rest reps) (+ acc add))
    )
   )
  )
)


(println "nombre de rapports qui sont safes :")
(println (count_safe reports))
(println "nombre de rapports qui sont quasi safes :")
(println (count_almost_safe reports))

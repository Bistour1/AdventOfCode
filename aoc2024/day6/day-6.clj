(ns day-6 (:require [clojure.string :as str]))

(def text (slurp "data6.txt"))

(defn list-contains? [l elem] (loop [l l] (if (empty? l) false (if (= (first l) elem ) true (recur (rest l))))))

(defn get-direction [guardmap] (
  let [text (flatten guardmap)] (
    cond (list-contains? text "<") "LEFT"
        (list-contains? text "^") "UP"
        (list-contains? text ">") "RIGHT"
        (list-contains? text "v") "DOWN"
        :else nil
  )
))

(defn move "index1 -> height index2 -> length" [guardmap direction index1 index2]  (
    let [face1 (cond (= direction "LEFT") "<" (= direction "RIGHT") ">"  (= direction "UP") "^" (= direction "DOWN") "v")
         face2  (cond (= direction "LEFT") "^" (= direction "RIGHT") "v"  (= direction "UP") ">" (= direction "DOWN") "<")
         limit1 (cond (= direction "LEFT") -1 (= direction "RIGHT") -1  (= direction "UP") 0 (= direction "DOWN") (dec (count guardmap)))
         limit2 (cond (= direction "LEFT") 0 (= direction "RIGHT") (dec (count (guardmap 0)))  (= direction "UP") -1 (= direction "DOWN") -1)
         ] (
      loop [guardmap guardmap index1 index1 index2 index2]  
        (
        let [newindex1 (cond (= direction "LEFT") index1 (= direction "RIGHT") index1  (= direction "UP") (dec index1) (= direction "DOWN") (inc index1))
             newindex2 (cond (= direction "LEFT") (dec index2) (= direction "RIGHT") (inc index2)  (= direction "UP") index2 (= direction "DOWN") index2)
             ] (
          if (or (= index1 limit1) (= index2 limit2)) guardmap (
            if (or (= (get (get guardmap newindex1)  newindex2 ) "#")) (assoc guardmap index1 (assoc (guardmap index1) index2 face2))
              (let [newmap (if (= index1 newindex1)  
                              (assoc guardmap index1 (assoc (assoc (guardmap index1) index2 "X")  newindex2 face1))
                              (assoc (assoc guardmap index1 (assoc (get guardmap index1) index2 "X")) newindex1 (assoc (get guardmap newindex1) index2 face1)))]             
                (recur newmap newindex1 newindex2)
              )
        ))
      )              
    )
))

(defn get-index [v] (
  loop [acc 0] (
    cond (= acc (count v)) nil
        (or (= (v acc) ">") (= (v acc) "<") (= (v acc) "v") (= (v acc) "^")) acc
        :else (recur (inc acc))
  )                     
))
   
(defn get-double-index "gives the height and the length pos of the guard" [guardmap] (
  loop [acc 0] (
      cond (= acc (count guardmap)) [-1 -1]
           (= (get-index (guardmap acc)) nil) (recur (inc acc)) 
           :else [acc (get-index (guardmap acc))] 
  )                                                                               
))

(defn get-path "gives a double vector like [[]  []  []] " [guardmap] (
  loop [guardmap guardmap mem '[]] 
  (
    let [
         direction (get-direction guardmap)
         pos (get-double-index guardmap)
         new (if (= (pos 0) -1) guardmap (move guardmap direction (pos 0) (pos 1) )  )
         is_out ( if (= (pos 0) -1) false (
                 cond (= direction "LEFT") (= (pos 1) 0)
                      (= direction "UP") (= (pos 0) 0)
                      (= direction "RIGHT") (= (pos 1) (dec (count (guardmap (pos 0)))))
                      (= direction "DOWN") (= (pos 0) (dec (count guardmap)))
                 ))
         is_loop (list-contains? mem [direction pos])
        ] (
        if (= (pos 0) -1 ) guardmap (
          if is_out guardmap (
            if is_loop "loop"
            (recur new (conj mem [direction pos]))
          )
        )
      )
    )
  )
)

(defn how-many [matcher] (
    loop [elem (re-find matcher) acc 0] (
        if-not elem acc (recur (re-find matcher) (inc acc))
    ))
)
(defn where [guardmap] (
  loop [guardmap guardmap index1 0 index2 0 pos []] (println (str pos index1 index2 " " (first guardmap))  ) (
    if (empty? guardmap) pos
      (let [new_index1 (if (empty? (first guardmap)) (inc index1) index1)
            new_index2 (if (empty? (first guardmap)) 0 (inc index2))
            new_guardmap (if (empty? (first guardmap)) (pop guardmap) (conj (rest (first guardmap)) (rest guardmap)))
            new_pos (if (list-contains? (first guardmap) "X") ["haaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"] pos)]
         (recur new_guardmap new_index1 new_index2 new_pos)
       ) 
  )                        
))
(defn string-to-vec [s] (loop [v '[] s (seq s)] (if (empty? s) v (recur (assoc v (count v) (str (first s))) (rest s)))))

(defn double-vector [guardmap_lines] (
  loop [guardmap guardmap_lines acc 0] (
    if (= acc (count guardmap)) guardmap (recur (assoc guardmap acc (string-to-vec (guardmap acc))) (inc acc))            
  )                                      
))

(defn possible-loops [guardmap] (
  let [
      initial (get-double-index guardmap) height (count guardmap) length (count (guardmap 0))
      pos (where (get-path guardmap))
      ]
  ( loop [pos pos acc 0] (println (str "pos:" pos " accumulateur en est à" acc)) (
      
                                                                                  
        let [
          index1 
           ((peek pos) 0)
           index2 ((peek pos) 1)
           newacc (if (and (not= initial [index1 index2]) (= (get-path (assoc guardmap index1 (assoc (get guardmap index1) index2 "#"))) "loop")) (inc acc) acc)] (
        if  (empty? (rest pos)) acc
            (recur (pop pos) newacc)      
      )
  ))
))



(def guardmap_lines  (str/split-lines text))
(def guardmap (double-vector guardmap_lines))
(println (count guardmap))
(println (count (guardmap 0)))
(def final (get-path guardmap))
(println final)
;(def result (how-many (re-matcher #"X|>|<|v|\^" (str/join (flatten final)))))
;(println result)
(println (possible-loops guardmap))

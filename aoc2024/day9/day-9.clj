(ns day9 (:require [clojure.string :as str]
                   [clojure.test :as test]))
(def text (str/triml (slurp "data9.txt") ))
(def numbers (conj nil \1 \2 3 \4 \5 \6 \7 \8 \9 \0))

(defn diskmap-to-files "Transform a diskmap string like '2543' into files block like '00.....1111...' " [diskmap] (
 loop [diskmap diskmap acc [] state true file-id 0] (
   if (empty? (str/trim diskmap)) (vec (flatten acc)) (
     let [length (count diskmap)
          value (Integer/parseInt (subs diskmap 0 1))] (
       recur (subs diskmap 1 length) (conj acc (repeat value (if state file-id "."))) (not state) (if state (inc file-id) file-id)     
     )            
   )                             
 )
))

(defn is-number? "takes a string and says if it's a number" [s] (
  loop [acc s] (
    if (empty? acc) true (
      if (not (some #(= (first s) %) numbers)) false  (recur (rest acc)))
    )
  )
)

(defn process-files "Takes files block and rearrange them on the left by taking the rightmost items first" [files] (
  loop [acc files free_space_index 0 file_index (dec (count files)) state false]( ;state= false means searching files.. true means searching space.
    if (= file_index free_space_index) acc
      (
      let [on_file? (is-number? (str (acc file_index)))
           new_file_index  (if state file_index (if on_file? file_index (dec file_index))) 
           on_free_space?  (= (acc free_space_index) ".")
           new_free_space_index (if state (if on_free_space? free_space_index (inc free_space_index)) free_space_index)
           newacc  (if on_free_space? (assoc (assoc acc free_space_index (acc file_index)) file_index ".") acc)
           new_state (if state (if on_free_space? false true) (if on_file? true false))
           ] (
              recur newacc new_free_space_index new_file_index new_state
         )
       )
  )                                                                                               
))

(defn final-sum "sum the multiplication of value*index of a vector" [files] (
  loop [files (replace {"." 0} files) acc 0 index 0] (
    if (empty? files) acc (recur (rest files) (+ acc (* index (first files))) (inc index))
  )                                                                             
))

(defn process-and-sum-files "Basically process + final sum functions but together and faster (we will need to test that)" [files] (
  loop [free_file (seq files) file_file files acc 0 free_space_index 0 file_index (dec (count files)) state false] ( ;state= false means searching files.. true means searching space.
    if (= file_index free_space_index) (if (is-number? (str (peek file_file))) (+ acc (* file_index (peek file_file))) acc)
      (
      let [on_file? (is-number? (str (peek file_file)))
           on_free_space? (= (first free_file) ".") 
           new_file_file (if state (if on_free_space? (pop file_file) file_file) (if on_file? file_file (pop file_file)))
           new_file_index (if state (if on_free_space? (dec file_index) file_index) (if on_file? file_index (dec file_index)))

           new_free_file (if state (rest free_file) free_file)
           new_free_space_index (if state (inc free_space_index) free_space_index)
           newacc (if state (+ acc (* free_space_index (if on_free_space? (peek file_file) (first free_file)))) acc) 
           new_state (if state (if on_free_space? false true) (if on_file? true false))
           ] (
              recur new_free_file new_file_file newacc new_free_space_index new_file_index new_state
         )
       )
  )                                                                                               
))

;Deuxième étoile:
(defn get-rightmost "returns the rightmost item of the vector that is a number" [files] (
  loop [files files] (if (empty? files) 0 (if (is-number? (str (peek files))) (peek files) (recur (pop files))))                       
))

(defn get-size "returns the size of the file with the given id" [id files] (
  loop [files files acc 0] (if (empty? files) acc (recur (pop files) (if (= id (peek files)) (inc acc) acc)))
))

(defn place [item size index files] (
  loop [n 0 acc files] (if (= n size) acc (recur (inc n) (assoc acc (+ index n) item)))   
))

(defn replace-file-block "replace the file-block on the leftmost place possible-> can't fragment it" [id files] (
  let [size (get-size id files)
       file-size (count files)] (
    loop [acc files space-size 0 index 0 found false found-id false] (
      if (or ( = file-size index) (and found-id (not= (files index) id))) acc (
        let [item (files index)
             new-found-id (or (= item id) found-id)
             new-space-size (if (= "." item) (inc space-size) 0);000....222
             new-found (and (= size new-space-size) (not found-id) )
             need-to-replace (and new-found (not found))
             newacc1 (if need-to-replace (place id size (- index space-size) acc) acc)
             newacc (if (and found (= item id)) (assoc newacc1 index ".") newacc1)
             ] (
          recur newacc new-space-size (inc index) (or found new-found) new-found-id
        )
      )
    )                                
                                  
  )                                                                                                                 
))

(defn process-file-blocks "likes the process function but can't fragment files into parts." [files] (
  let [rightmost-id (get-rightmost files)]  (
    loop [n rightmost-id acc files] (if (= (mod n 100) 0) (println "Processing file id: " n "...") nil) (if (= n 0) acc (
      recur (dec n) (replace-file-block n acc))                                     
    )
  )
))


(def files (diskmap-to-files text))
;(def processed (process-files files))
;(println (final-sum processed))
(println (str "Première étoile: " (process-and-sum-files files)))
(time (final-sum (process-files files))); 692 msecs
(time (process-and-sum-files files));460 msecs
(println (str "Deuxième étoile: " (final-sum (process-file-blocks files))))
; on voit que la fonction process-and-sum-files est plus rapide que les 2 autres fonctions emboitées.

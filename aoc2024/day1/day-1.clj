(require '[clojure.string :as str])
(def text ( slurp "input-day1.txt"))
(def filelines (vec (str/split-lines text)))

(defn getlists "takes the lines of the data file and put the numbers in two vectors l1 and l2" 
  ;[lines] (println type lines) (getlists (pop lines) (vector (get (str/split (peek (vec 'lines)) "   " ) 0 )) (vector (get (str/split (peek (vec 'lines)) "   ") 1))  )
   [lines, v1, v2] (
    loop [lines lines v1 [] v2 [] ] 
    (
      if (empty? lines) (vector v1 v2)
      (let [line (first lines)
          splitted (str/split line #"   ");TODO test avec #"\s+"
          num1 (Integer/parseInt (first splitted))
          num2 (Integer/parseInt (first (rest splitted)))]
      (recur (rest lines) (conj v1 num1) (conj v2 num2))
      ) 
      
    )
) 
)

(defn absolute "return absolute value" [x] ( if (> x 0) x (- 0 x) ))


(defn get_distance "gives the distance between two vectors, you need to sort them beforhand" [v1 v2]
  ( loop [v1 v1 v2 v2 acc 0] (
    if (empty? v1) acc
    (recur (rest v1) (rest v2) (+ acc (absolute (- (first v1) (first v2)))))
    )
  )
)

(defn count_elem "gives the number of elements of the vector that are like the element given" [v a]
  ( loop [v v acc 0] (
     if (empty? v) acc (recur (rest v) (+ acc (if (= a (first v))1 0)) )
    )
   )  
)

(defn get_similarity "gives the similarity between two vectors," [v1 v2] (
  loop [v1 v1 v2 v2 acc 0] (
    if (empty? v1) acc
      (
       let [
           num1 (first v1)
           ]
       (recur (rest v1) v2 (+ acc (* num1 (count_elem v2 num1))))
                                                                                 
      )
    )
)) 



(def l1 (sort > (get (getlists filelines [] []) 0)))
(def l2 (sort > (get (getlists filelines [] []) 1)))
(println "Distance entre les 2 listes :")
(println (get_distance l1 l2))
(println "Similarité entre les 2 listes :")
(println (get_similarity l1 l2))

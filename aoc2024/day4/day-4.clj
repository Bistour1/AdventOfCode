(ns day-4 (:use [clojure.string :as str]))
(def text (slurp "data4.txt"))
(def regex1 #"XMAS")
(def len (count (first (str/split-lines text))))
;(def regex2 #"M[A-Za-z]M[A-Za-z]{- len 1}A[A-Za-z]{- len 1}S[A-Za-z]S|M[A-Za-z]S[A-Za-z]{- len 1}A[A-Za-z]{- len 1}M[A-Za-z]S|S[A-Za-z]M[A-Za-z]{- len 1}A[A-Za-z]{- len 1}S[A-Za-z]M|S[A-Za-z]S[A-Za-z]{- len 1}A[A-Za-z]{- len 1}M[A-Za-z]M")
(def regex2 (re-pattern (str/join ["M[A-Za-z .]M[A-Za-z \n .]{" (str (- len 1)) "}A[A-Za-z \n .]{" (str (- len 1)) "}S[A-Za-z .]S|M[A-Za-z .]S[A-Za-z \n .]{" (str (- len 1)) "}A[A-Za-z \n .]{" (str (- len 1)) "}M[A-Za-z .]S|S[A-Za-z .]M[A-Za-z \n .]{" (str (- len 1)) "}A[A-Za-z \n .]{" (str (- len 1)) "}S[A-Za-z .]M|S[A-Za-z .]S[A-Za-z \n .]{" (str (- len 1)) "}A[A-Za-z \n .]{" (str (- len 1)) "}M[A-Za-z .]M"])))

(defn contains "checks if the list contains the character" [l ch] (
   loop [l l] (
     if (empty? l) 
       false 
       (
        if 
          (= (first l) ch)
          true  
          (recur (rest l))
       )
   )
))

(defn tronc [string i] (
  loop [strl (seq string) acc 0] (
    if (= acc i) (str/join strl)
      (recur (rest strl) (+ acc 1))
  )  
))

(defn find-horizontal [regex text] (
 loop [text text acc 0 mem (rest (conj nil)) elem (re-find (re-matcher regex text))]
  ( 
    
     if (= elem nil)
      (count mem)
      (let [ new (if (contains mem elem) [] elem)
            ] 
         (recur (tronc text 1) (+ acc 1) (flatten (conj mem new)) (re-find (re-matcher regex (tronc text 1))))
       )
    
  )
))

(defn find-all-horizontal [text regex] (
    loop [lines (str/split-lines text) acc 0] ;(println lines) (println acc) 
    (
      if (empty? lines)
        acc
        (recur (rest lines) (+ (+ acc (find-horizontal regex (str/reverse (first lines)))) (find-horizontal regex (first lines))))                                )                                            
  )
)

(defn get_index [line i] (
  loop [chs (seq line) acc 0] (
    if (= acc i) (str (first chs))
      (recur (rest chs) (+ acc 1))    
  )                    
)    
)

(defn get-column [text i] (
  loop [lines (str/split-lines text) column ""] (
    if (empty? lines)
      column
      (recur (rest lines) (str/join [column (get_index (first lines) i)]))     
  )                           
))

(defn get-all-column [text] (
  let [len (count (first (str/split-lines text)))]
  (
   loop [i 0 columns ""] (
    if (= i len) columns
    (recur (+ i 1) (str/join "\n" (conj nil columns (get-column text i))))
   )
  )                                  
)
)

(defn get-diagonal1 "gets the diagonal from top left to bottom right" [text i] (;on veut que le 0 soit en dernière ligne->acc = i-len lines On a que les diagonales sont à 0 <= i <= 20
    loop [lines (str/split-lines text) diag "" acc (- i(count lines ))] (
      if (empty? lines) diag
        (let [line (first lines)
              len (count line)] (
            if (= acc len) diag
              (
                let [nc (if (< acc 0) " " (get_index line acc))]
                (recur (rest lines) (str/join [diag nc]) (+ acc 1))
              )  
          )
         )
    )                                                                                                                                                         
  )
)

(defn get-diagonal2 "gets the diagonal from top right to bottom left" [text i] (
    loop [lines (str/split-lines text) diag "" acc i] (
      if (empty? lines) diag
        (let [line (first lines)
              len (count line)]
          (
            if (< acc 0) diag
            (
              recur (rest lines) (str/join [diag (get_index line acc)]) (- acc 1) 
            )
          )
        )                                                                            
    )
)) 

(defn max_length [text] (
  loop [lines (str/split-lines text) m 0] (
    if (empty? lines) m
      (recur (rest lines) (max m (count (first lines))))    
  )                         
))

(defn get-all-diagonal "gets all the diagonals possible" [text] (
  loop [i 0 diagonals ""] (
    if (= i (* (max_length text) 2)) diagonals
      (recur (+ i 1) (str/join "\n" (conj nil diagonals (get-diagonal1 text i) (get-diagonal2 text i))))
  )                                                                
)  
)

(defn find-all-diagonal [text regex] (find-all-horizontal (get-all-diagonal text) regex))

(defn find-all-column [text regex] (find-all-horizontal (get-all-column text) regex))

(defn find-all [text regex] (
   + (find-all-column text regex) (+ (find-all-horizontal text regex) (find-all-diagonal text regex))
  )
)

(println "Première étoile")
(println (find-all text regex1))
(println "Deuxième étoile")
(println (find-horizontal regex2 text))

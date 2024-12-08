(require '[clojure.string :as str])
(def text (slurp "input-day3.txt"))
(def *numbers* (conj nil \1 \2 \3 \4 \5 \6 \7 \8 \9 \0))
(def *prison* (conj nil \( \)))


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



(defn is_number [string] (
  loop [strl (seq string)] (
   if 
     (empty? strl)
     true
     (
       if 
         (not (contains *numbers* (first strl)))
         false
         (recur (rest strl))
     )
  )
))


(defn get_numbers [part] (
           let [
              nums (first (str/split part #"\)"))
              num1 (first (str/split nums #","))
              num2 (first (rest (str/split nums #",")))
           ]
           (
            if (empty? (rest (str/split nums #",")))
            (conj nil 0 0)
            (
             let [
               num1 (first (str/split nums #","))
               num2 (first (rest (str/split nums #",")))
             ] 
             (
              if 
               (or (not (is_number num1)) (not (is_number num2)))
               (conj nil 0 0)
               (conj nil (Integer/parseInt num1) (Integer/parseInt num2))
             )
            )    
           )
))

(defn get_sum "gets the sum of the multiplications" [text] 
  (
   loop [texts (str/split text #"mul\(") acc 0] ;(println texts) (println "on est là:") 
      (
       if (empty? texts) acc
       (
           let [ numbers (get_numbers (first texts))
                num1 (first numbers)
                num2 (first (rest numbers))
               ]
          (recur (rest texts) (+ acc (* num1 num2)))
        )                     
      )
   )  
)

(defn escape "from a string, escape all problematic characters like brackets,..." [string] (
  loop [strl (seq string) construct ""] (
    if (empty? strl) 
       (re-pattern construct )
       (
        let [add (if (contains *prison* (first strl))
                    (str/join "" (conj nil (str (first strl)) "\\"))
                    (str (first strl))
                  )
             ]
        (recur (rest strl) (str/join "" (conj nil add construct)))
        )
    )                                                                                            
                                                                                            
                                                                                            
))


(defn split_once "only split in half where we first meet the character" [st ch]
(
 conj 
  nil
  (str/join ch (rest (str/split st (escape ch))))
  (first (str/split st (escape ch)))
 )  
)

(defn get_condition_sum "only sum when do is enable and don't when don't is enable" [text] (
     loop [text text acc 0] (
        if (empty? (seq text)) acc (
        if (empty? (rest (split_once text "don't()"))); il y a plus encore don't dans le texte et qqch derrière                
            (+ acc (get_sum (first (split_once text "don't()"))))
            (let [new_text (first (rest (split_once text "don't()")))]
                (if (empty? (rest (split_once new_text "do()")))
                    (+ acc (get_sum (first (split_once text "don't()"))))
                    (recur (first (rest (split_once new_text "do()"))) (+ acc (get_sum (first (split_once text "don't()")))) )
                 )
             )     
          )
      )                                                                           
))

(println "Première étoile:")
(println (get_sum text))
(println "Deuxième étoile:")
(println (get_condition_sum text))

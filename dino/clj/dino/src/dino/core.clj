(ns dino.core)

(defn -cartesian-helper [list-a list-b]
  (for [a list-a]
    (for [b list-b]
      (list a b))))

(defn cartesian [list-a list-b]
  (reduce concat (-cartesian-helper list-a list-b)))

(defn payoff [[a b]]
  (cond (> a b) 1
        (< a b) -1
        :else 0))

(defn dominance [list-a list-b]
  (apply + (map payoff (cartesian list-a list-b))))

(defn remove-index [s i]
  (concat (take i s) (drop (inc i) s)))

; This will be a list of rows
; with list-a corresponding to the vertical (row) player
; and list-b the horizontal (column) player
(defn payoff-matrix [list-a list-b]
 (map-indexed
   (fn [row-idx row] 
     (map-indexed
       (fn [col-idx entry]
           (+
             (payoff (list (nth list-a row-idx) (nth list-b col-idx)))
             (/ 
              (dominance 
                (remove-index list-a row-idx) 
                (remove-index list-b col-idx))
              (dec (count list-a)))))

       list-b))
   list-a))

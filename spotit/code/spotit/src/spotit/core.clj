(ns spotit.core
  (:gen-class))

(require '[clojure.math.combinatorics :as combinatorics])

; Sectikon: Various utilities

(defn remove-ix [s ix]
  (concat (take ix s)
  								(drop (inc ix) s)))

; (defn all-chooses [s ct]
;   (cond (= 0 ct) '()
;         :else 
;         (map
;           (fn [ix]
;           	 (map #(concat (list (nth s ix)) %)
;                 	 (all-chooses (remove-ix s ix) (dec ct))))
;           (range (count s)))))


;; stolen from https://codereview.stackexchange.com/questions/8930/enumerate-k-combinations-in-clojure-p26-from-99-problems
(defn subsets [n items]
(cond
    (= n 0) '(())
    (empty? items) '()
    :else (concat (map
                    #(cons (first items) %)
                    (subsets (dec n) (rest items)))
                  (subsets n (rest items)))))

(defn cartesian [l1 l2]
  (for [e1 l1
      e2 l2]
  (list e1 e2))
)



(defn ordered-pairs [l1]
  (filter #(> (first %) (second %))
  (cartesian l1 l1)))


;   (filter #(not= 0 %)

(defn all-diffs [l1 modulus]
					(filter #(not= 0 %))
      (map #(mod
      	(- (first %) (second %))
      	modulus)
     (ordered-pairs l1)))

(defn all-distinct? [l1]
  (= (count l1)
  	  (count (distinct l1))))


(defn crunch [n modulus]
  (if (> n (/ modulus 2))
  				(- modulus n)
  				n))

(defn crunch-all [l modulus]
  (map #(crunch % modulus) l))

(defn candidate? [l1 modulus badmod]
  (let [diffs (crunch-all (all-diffs l1 modulus) modulus)]
    (and 
      (all-distinct? diffs)
      (every? #(not= 0 (mod % badmod)) diffs)
    )
    ))


;(filter #(candidate? % 16 4) (subsets 4 (range 16))) ;; none for g=4, s=5
 ; (filter #(candidate? % 36 6) (subsets 6 (range 36))) ;; none for g=6, s=7
;(filter #(candidate? % 15 5) (subsets 3 (range 15)))
;(filter #(candidate? % 21 999) (subsets 5 (range 21)))

(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn all-zero?  [s]
 (empty? (filter (complement zero?) s)))



; Spot it: Old section - is there a PDS on K_n?

(defn self-3-cartesian [l]
  (for [e1 '((0 1 2))
  e2 l e3 l]
  (list e1 e2 e3)))


(defn three-plus [a b]
  (mod (+ a b ) 3))

; assume all distinct coming in
(defn good-threebythree [[vec-a vec-b vec-c]]
  (let [ab (map three-plus vec-a vec-b)
  						abc (map three-plus ab vec-c)]
  	(and 
  			(all-distinct? ab)
  			(all-zero? abc))))





(defn self-4-cartesian [l]
  (for [e1 '((0 1 2 3))
  e2 l e3 l e4 l]
  (list e1 e2 e3 e4)))

(defn four-plus [a b]
  (mod (+ a b ) 4))

; assume all distinct coming in
(defn good-fourbyfour [[vec-a vec-b vec-c vec-d]]
  (let [ab (map four-plus vec-a vec-b)
  						abc (map four-plus ab vec-c)
  						abcd (map four-plus abc vec-d)]
  	(and 
  			(all-distinct? ab)
					 (all-distinct? abc)
  			(all-zero? abcd))))




; Old section - is there a PDS on K_5?


(defn self-5-cartesian [l]
  (for [e1   '((0 1 2 3 4))
  e2 l e3 l e4 l e5 l]
  (list e1 e2 e3 e4 e5)))

(defn five-plus [a b]
  (mod (+ a b ) 5))

; assume all distinct coming in
(defn good-fivebyfive [[vec-a vec-b vec-c vec-d vec-e]]
  (let [ab (map five-plus vec-a vec-b)
  						abc (map five-plus ab vec-c)
  						abcd (map five-plus abc vec-d)
  						abcde (map five-plus abcd vec-e)
 					 ]
  	(and 
  			(all-distinct? ab)
					 (all-distinct? abc)
					 (all-distinct? abcd)
  			(all-zero? abcde))))


; cheat: can assign the first vector to be anything legal
(defn self-6-cartesian [l]
  (for [e1 '((0 1 2 3 4 5)) e2 l e3 l e4 l e5 l e6 l]
  (list e1 e2 e3 e4 e5 e6) ))

(defn six-plus [a b]
  (mod (+ a b ) 6))

; assume all distinct coming in
(defn good-sixbysix [[vec-a vec-b vec-c vec-d vec-e vec-f]]
  (let [ab (map six-plus vec-a vec-b)
  						abc (map six-plus ab vec-c)
  						abcd (map six-plus abc vec-d)
  						abcde (map six-plus abcd vec-e)
  						abcdef (map six-plus abcde vec-f)]
  						
  	(and 
  			(all-distinct? ab)
					 (all-distinct? abc)
					 (all-distinct? abcd)
					 (all-distinct? abcde)
  			(all-zero? abcdef))))


(defn self-7-cartesian [l]
  (for [e1 '((0 1 4 6 3 2 5)) e2 l e3 l e4 l e5 l e6 l e7 l]
  (list e1 e2 e3 e4 e5 e6 e7) ))

(defn seven-plus [a b]
  (mod (+ a b ) 7))

; assume all distinct coming in
(defn good-sevenbyseven [[vec-a vec-b vec-c vec-d vec-e vec-f vec-g]]
  (let [ab (map seven-plus vec-a vec-b)
  						abc (map seven-plus ab vec-c)
  						abcd (map seven-plus abc vec-d)
  						abcde (map seven-plus abcd vec-e)
  						abcdef (map seven-plus abcde vec-f)
  						abcdefg (map seven-plus abcdef vec-g)
  						]
  						
  	(and 
  			(all-distinct? ab)
					 (all-distinct? abc)
					 (all-distinct? abcd)
					 (all-distinct? abcde)
					 (all-distinct? abcdef)
  			(all-zero? abcdefg))))


; (take 1 (filter good-threebythree (self-3-cartesian (permutations (range 3)))))
; (take 1 (filter good-fourbyfour (self-4-cartesian (permutations (range 4)))))
; (take 1 (filter good-fivebyfive (self-5-cartesian (permutations (range 5)))))
(take 1 (filter good-sixbysix (self-6-cartesian (permutations (range 6)))))
; (take 1 (filter good-sevenbyseven (self-7-cartesian (permutations (range 7)))))




;; ====

(defn seek
  "Returns first item from coll for which (pred item) returns true.
   Returns nil if no such item is present, or the not-found value if supplied."
  {:added  "1.9" ; note, this was never accepted into clojure core
   :static true}
  ([pred coll] (seek pred coll nil))
  ([pred coll not-found]
   (reduce (fn [_ x]
             (if (pred x)
               (reduced x)
               not-found))
           not-found coll)))


; Spot it: Redone - can we backtrack search our way to a PDS?


(defn backtracker [accum elts fn-legit fn-done]
  ;(when (= 4 (count accum)) (println accum))
  ;(println "BT called with accum " accum " elts " elts)
  (cond (fn-done accum) accum
  						(empty? elts) :failed
      	 :else (if (fn-legit (concat accum (list (first elts))))

        	   ;; if a legit possibility
        	   (let [r (backtracker (concat accum (list (first elts))) (rest elts) fn-legit fn-done)]
        	   		(if (not= :failed r) r
        	   		                     (backtracker accum (rest elts) fn-legit fn-done)
        	   		))
        	   
        	   ;; if not legit, try next
        	   (backtracker accum (rest elts) fn-legit fn-done)
        	 			)))

; Spot it: uncomment these to try various combos.  Note that 999907 etc.
; means "do not exclude anything based on modulus"
; Note also that we havne't found any PDS's with an excluded modulus

#_(


; Expected - g-1 is prime
; s=g=4, n=m=13: succeed - (0 1 3 9)
(backtracker 
		'(0 1)
		(range 3 13)
		(fn [x] (candidate? x 13 99907) )
		(fn [x] (and (candidate? x 13 99907) (= 4 (count x))))
		)


; WEIRD - g-1 is composite
;; (0,1,4,14,16) on s=g=5, n=21
(backtracker 
		'(0 1)
		(range 3 20)
		(fn [x] (and (= 0 (first x)) (candidate? x 21 99907) ))
		(fn [x] (and (candidate? x 21 99907) (= 5 (count x))))
		)

; Expected - g-1 is prime
; s=g=6, n=m=31: (0 1 3 8 12 18)
(backtracker 
		'(0 1)
		(range 3 31)
		(fn [x] (candidate? x 31 99907) )
		(fn [x] (and (candidate? x 31 99907) (= 6 (count x))))
		)

; Expected - g-1 is composite
;; :failed on s=g=7, n=m=43
(backtracker 
		'(0 1)
		(range 3 43)
		(fn [x] (candidate? x 43 99907) )
		(fn [x] (and (candidate? x 43 99907) (= 7 (count x))))
		)

; Expected - g-1 is prime
; s=g=8, n=m=57 - (0 1 3 13 32 36 43 52)
(backtracker 
		'(0 1)
		(range 3 57)
		(fn [x] (candidate? x 57 99907) )
		(fn [x] (and (candidate? x 57 99907) (= 8 (count x))))
		)

; WEIRD - g-1 is not prime
; s=g=9, n=m=73 - (0 1 3 7 15 31 36 54 63)
; NOTE - these are all 2^n-1 for a bit	
(backtracker 
		'(0 1)
		(range 3 73)
		(fn [x] (candidate? x 73 99907) )
		(fn [x] (and (candidate? x 73 99907) (= 9 (count x))))
		)

; WEIRD, g-1 is not prime
; s=g=10, n=m=91 - (0 1 3 9 27 49 56 61 77 81)
(backtracker 
		'(0 1)
		(range 3 91)
		(fn [x] (candidate? x 91 99907) )
		(fn [x] (and (candidate? x 91 99907) (= 10 (count x))))
		)

;; WAITING on g=s=11,m=n=111- :failed
	(backtracker 
			'(0 1)
			(range 3 110)
			(fn [x] (and (= 0 (first x)) (candidate? x 111 99907) ))
			(fn [x] (and (candidate? x 111 99907) (= 11 (count x))))
			)

; WAITING g=s=12, m=n=133 - (0 1 3 12 20 34 38 81 88 94 104 109)
(backtracker 
		'(0 1)
		(range 3 131)
		(fn [x] (candidate? x 133 999))
		(fn [x] (and (candidate? x 133 999) (= 12 (count x))))
		)

; g=3, s=4, n=9, m=12: NOTE - all three-fers but this one fails. WEIRDISH
(backtracker 
		'(0 1)
		(range 3 8)
		(fn [x] (candidate? x 9 3) )
		(fn [x] (and (candidate? x 9 4) (= 3 (count x))))
		)

; g=4, s=5, n=16, m=20: failed
(backtracker 
		'(0 1)
		(range 3 15)
		(fn [x] (candidate? x 16 4) )
		(fn [x] (and (candidate? x 16 4) (= 4 (count x))))
		)

; g=5, s=6, n=25, m=30: failed - WEIRD SHOULD SUCCEED
(backtracker 
		'(0 1)
		(range 3 24)
		(fn [x] (candidate? x 25 5) )
		(fn [x] (and (candidate? x 25 5) (= 5 (count x))))
		)

; g=6, s=7, n=36, m=42: failed 
(backtracker 
		'(0 1)
		(range 3 35)
		(fn [x] (candidate? x 36 6) )
		(fn [x] (and (candidate? x 36 6) (= 6 (count x))))
		)

; g=7, s=8, n=49, m=56: failed this style (WEIRDISH)
(backtracker 
		'(0 1)
		(range 3 48)
		(fn [x] (candidate? x 49 7) )
		(fn [x] (and (candidate? x 49 7) (= 7 (count x))))
		)

; g=8, s=9, n=64, m=72: failed
(backtracker 
		'(0 1)
		(range 3 63)
		(fn [x] (candidate? x 64 8) )
		(fn [x] (and (candidate? x 64 8) (= 8 (count x))))
		)

; g=9, s=10, n=81, m=90: failed
(backtracker 
		'(0 1)
		(range 3 80)
		(fn [x] (candidate? x 81 9) )
		(fn [x] (and (candidate? x 81 9) (= 9 (count x))))
		)

)

; Spot it section: Strategy for finding two coexisting cliques is to generate all candidates 
; then check cartesian pairs.  Simpler than backtracking but explodes quickly.

#_(
	(def sets-2-of-13 (subsets 2 (range 1 13)))
	(def candidates-3-6 (filter #(candidate? % 13 9999)  (map (partial cons 0)  sets-2-of-13)))
	; Looking fof s=6, g=3, n=13, m=26 3-clique pair
	(take 1 
	 			(filter (fn [x] 
	 					(all-distinct? 
	 					  (crunch-all 
	 					  	  (flatten 
	 					  	    (map #(all-diffs  % 13) x)) 13)))
	 					(cartesian candidates-3-6 candidates-3-6)))
	;(((0 1 4) (0 2 7)))


	(def sets-2-of-15 (subsets 2 (range 1 15)))
	(def candidates-3-7 (filter #(candidate? % 13 5)  (map (partial cons 0)  sets-2-of-15)))
	; Looking fof s=3, g=7, n=15, m=21 3-clique pair
	(take 1 
	 			(filter (fn [x] 
	 					(all-distinct? 
	 					  (crunch-all 
	 					  	  (flatten 
	 					  	    (map #(all-diffs  % 15) x)) 15)))
	 					(cartesian candidates-3-7 candidates-3-7)))
	; ((0 1 3) (0 4 10))



	(def sets-3-of-25 (subsets 3 (range 1 25)))
	(def candidates-4-8 (filter #(candidate? % 25 9999)  (map (partial cons 0)  sets-3-of-25)))
	; Looking fof s=8, g=4, n=25, m=50 4-clique pair
	(take 1 
	 			(filter (fn [x] 
	 					(all-distinct? 
	 					  (crunch-all 
	 					  	  (flatten 
	 					  	    (map #(all-diffs  % 25) x)) 25)))
	 					(cartesian candidates-4-8 candidates-4-8)))

	; () ;; nione so far



	(def sets-4-of-41 (subsets 4 (range 1 41)))
	 (def candidates-5-10 (filter #(candidate? % 41 9999)  (map (partial cons 0)  sets-4-of-41)))
	; Looking for s=10, g=5, n=41, m=82 5-clique pair
	(take 1 
	 			(filter (fn [x] 
	 					(all-distinct? 
	 					  (crunch-all 
	 					  	  (flatten 
	 					  	    (map #(all-diffs  % 41) x)) 41)))
	 					(cartesian candidates-5-10 candidates-5-10)))

	; (((0 1 4 11 29) (0 2 8 17 22)))
	  

	(def sets-4-of-45 (subsets 4 (range 1 45)))
	 (def candidates-5-11 (filter #(candidate? % 45 9)  (map (partial cons 0)  sets-4-of-45)))
	; Looking for s=11, g=5, n=45, m=99 5-clique pairs
	(take 1 
	 			(filter (fn [x] 
	 					(all-distinct? 
	 					  (crunch-all 
	 					  	  (flatten 
	 					  	    (map #(all-diffs  % 45) x)) 45)))
	 					(cartesian candidates-5-11 candidates-5-11)))


	(def sets-5-of-61 (subsets 5 (range 1 61)))
	 (def candidates-6-12 (filter #(candidate? % 61 9999)  (map (partial cons 0)  sets-5-of-61)))
	; Looking for s=12, g=6, n=61, m=122 5-clique pairs
	(take 1 
	 			(filter (fn [x] 
	 					(all-distinct? 
	 					  (crunch-all 
	 					  	  (flatten 
	 					  	    (map #(all-diffs  % 61) x)) 61)))
	 					(cartesian candidates-6-12 candidates-6-12)))
	; TODO t-- big


	; TODO - too big for even the subsets
	(def sets-6-of-85 (subsets 6 (range 1 85)))
	 (def candidates-7-14 (filter #(candidate? % 85 9999)  (map (partial cons 0)  sets-6-of-85)))
	; Looking for s=14, g=7, n=85, m=170 5-clique pairs
	(take 1 
	 			(filter (fn [x] 
	 					(all-distinct? 
	 					  (crunch-all 
	 					  	  (flatten 
	 					  	    (map #(all-diffs  % 85) x)) 85)))
	 					(cartesian candidates-7-14 candidates-7-14)))
	; TODO - too big


)




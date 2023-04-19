(ns spotit.core
  (:gen-class))

(require '[clojure.math.combinatorics :as combinatorics])

; Section: Utilities

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

(defn find-pds [s g]
  (let [n (inc (* (dec g) s))
  		modulus (mod s g)
 		badmod (if (= 0 (mod s g))  999907 (/ n g)) ; TODO this is garbage
		]
  (if (and (not= 0 modulus) (not= 1 modulus))
		:failed
  	(backtracker 
		'(0 1)
		(range 3 n) ; can't be n, can't be n-1, so we're good here
		(fn [x] (candidate? x n badmod) )
		(fn [x] (and (candidate? x n badmod) (= g (count x))))
		))))

#_(


	; Expected - g-1 is prime
	; s=g=4, n=m=13: succeed - (0 1 3 9)
	(find-pds 4 4)

	; Expected - g-1 is prime power
	;; (0,1,4,14,16) on s=g=5, n=21
	(find-pds 5 5)

	; Expected - g-1 is prime
	; s=g=6, n=m=31: (0 1 3 8 12 18)
	(find-pds 6 6)

	; Expected - g-1 is composite
	; :failed
	(find-pds 7 7)

	; Expected - g-1 is prime
	; s=g=8, n=m=57 - (0 1 3 13 32 36 43 52)
	(find-pds 8 8)

	; Expected - g-1 is prime power
	; s=g=9, n=m=73 - (0 1 3 7 15 31 36 54 63)
	; NOTE - these are all 2^n-1 for a bit	
	(find-pds 9 9)

	; Expected - g-1 is prime power
	; s=g=10, n=m=91 - (0 1 3 9 27 49 56 61 77 81)
	(find-pds 10 10)

	;; g=s=11,m=n=111- :failed
	(find-pds 11 11)

	; WAITING g=s=12, m=n=133 - (0 1 3 12 20 34 38 81 88 94 104 109)
	(find-pds 12 12)

	; NOTE: all failed here - no PDS excluding the divider graphs

	;failed
	(find-pds 4 3)
		

	;failed
	(find-pds 5 4)

	;failed
	(find-pds 6 5)

	;failed
	(find-pds 7 6)

	;failed
	(find-pds 8 7)

	;failed
	(find-pds 9 8)

	;failed
	(find-pds 10 9)

	; ...
)


(defn find-multi-pds [s g]
	(let [n (inc (* (dec g) s))
		all-subsets (subsets (dec g) (range 1 n))
		candidates (filter #(candidate? % n 999907) (map (partial cons 0)  all-subsets))
		]
		(take 1 
	 			(filter (fn [x] 
	 					(all-distinct? 
	 					  (crunch-all 
	 					  	  (flatten 
	 					  	    (map #(all-diffs  % n) x)) n)))
	 					(cartesian candidates candidates)))))
						

; Spot it section: Strategy for finding two coexisting cliques is to generate all candidates 
; then check cartesian pairs.  Simpler than backtracking but explodes quickly.

#_(
	(find-multi-pds 6 3)
	;; (((0 1 4) (0 2 7)))

	(find-multi-pds 7 3)
	;; (((0 1 3) (0 4 9)))


	(find-multi-pds 8 4)
	; () 

	(find-multi-pds 10 5)
	; (((0 1 4 11 29) (0 2 8 17 22)))
		

	(find-multi-pds 11 5)
	; ()

	(find-multi-pds 12 6)
	; TODO - mem overflow

)




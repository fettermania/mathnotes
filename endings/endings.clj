(defn numlen [n] (count (str n)))

(defn idem [n] (map first (filter #(= (first %) (second %)) (map list (range n) (map (fn [x] (mod (* x x) n)) (range n))))))

(defn idem? [n modulus]
   (= n (mod (* n n) modulus)))

(defn make-10-to-the-n [n]
  (bigint (apply str (concat '("1") (repeat n "0")))))


(defn prepend-digit [n digit]
  (bigint (str digit (str n))))


(defn next-idem [n len]
  (let [modulus (make-10-to-the-n len)
	products (map (partial prepend-digit n) (range 10))]
 	(first
    	  (filter #(idem? % modulus) products))))


(the answer)
(loop [modulus 10N
       prod 5N
	n 1]
       (when (<= n 100)
       (println (str prod ", 10^" n))
	(recur (* 10 modulus) (mod (* prod prod) (* 10 modulus)) (inc n)))
)

       
3953007319108169802938509890062166509580863811000557423423230896109004106619977392256259918212890625

We know 5^2 - 5 = 10k for some k.  We can also write it as 5(5-1) = 10k

Then we also can say 5^4 - 5^2 = 5^2(5^2 - 1) = 5^2(5+1)(5-1) = 100m then, since the first factor has two 5s, and the second two each are even.  This implies 25^2 - 25 = 100m

Then we can also say that 5^8 - 5^4 = 5^4(5^4 - 1) = 5^4(5^2 + 1)(5^2 - 1) = 5^4(5^2 + 1)(5+1)(5-1) = 1000n, since the first factor contains at least three fives, and the three additional factors are even.

We continue this way and notice that 5^(2^n) mod 10^n will serve as our mystery digits.

Naturally, computing 5^(2^100), say, is prohibitive.  Since we're only looking at the last n digits, and in arithmetic modulo 10^n, those are the only digits that matter, we can simply take our last mystery digits, square them, and take the result modulo 10^n for our next set of digits.

Also consider that if we have a set of digits D, then 1 - D modulo 10^n will also solve the problem, since (1 - D)^2 = 1 - 2D + D^2 = 1 - 2D + D = 1-D.  This is how we find our ...376 solution.

-- note that the digits also cycle:

ones, tens: always 25
hundreds: 6, 1
thousands: 0, 3, 5, 8...
so these all reset every 2^n!



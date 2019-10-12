;;****************************************************************************
;; Solutions to problems to Project Euler (https://projecteuler.net/)
;; in Clojure.
;;
;; Author: Michael Mackenzie <mike.a.mackenzie@gmail.com>
;;****************************************************************************


(ns project-euler-clojure.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.string :as str])
  (:gen-class))


;;============================================================================
;; Helper functions
;;============================================================================

(defn fibonacci
  "Return a lazy sequence of Fibonacci numbers"
  ([]
   (fibonacci 0 1))
  ([a b]
   (lazy-seq
     (cons b (fibonacci b (+ a b))))))

(defn is-factor
  "Determine if y is a factor of x"
  [x y]
  (= 0 (rem x y)))

(def prime?
  "Determine if a number is prime"
  (memoize
    (fn [x]
      (case x
        (0 1) false
        2 true

        (empty? (filter (partial is-factor x)
                        (range 2
                               (+ 1 (Math/ceil (Math/sqrt x))))))))))

(defn primes
  "Return a lazy sequence of prime numbers"
  []
  (lazy-seq
    (cons 2
          (filter prime?
                  (iterate (partial + 2)
                           3)))))

(defn build-sieve
  ([n] (build-sieve n 2 #{}))

  ([n i curr-primes]
   (if (>= i n)
     curr-primes
     (if (not (contains? curr-primes i))
       (recur n (inc i) (into curr-primes
                              (take-nth i (range n))))
       (recur n (inc i) curr-primes)))))

(defn logarithm
  "Compute logarithm of x in the given base"
  [base x]
  (/ (Math/log x) (Math/log base)))

(defn degree-of-factor
  "Compute the degree of the given factor in num"
  [num factor]
  (letfn [(inner [curr degree]
            (if (= 0 (mod curr factor))
              (recur (/ curr factor) (inc degree))
              degree))]
    (inner num 0)))

(defn prime-factors
  "Compute the prime factors of x as a vector of ints"
  [x]
  (let [small-pfs
        (filter (partial is-factor x)
                (take-while (partial > (inc (int (Math/sqrt x)))) (primes)))]
    (apply conj
           (filter prime?
                   (conj (map (fn [y] (/ x (int (Math/pow y (degree-of-factor x y)))))
                              small-pfs)
                         x))
           small-pfs)))

(defn prime-factorization
  "Compute the prime factorization of x. The returned value is a vector of
  2-element vectors of the form [factor degree]."
  [x]
  (map (fn [y] [y (degree-of-factor x y)])
       (prime-factors x)))

(defn merge-factorizations
  "Given a sequence of factorizations (like those returned from
  prime-factorization), return a map with keys consisting of the union of
  all of the factors from all of the factorizations and values consisting of
  the maximum degree for that factor in all of the factorizations."
  [factorizations]
  (loop [max-deg-factors (sorted-map)

         ;; flattens a single level, giving us a sequence of all factors
         ;; and degrees of all factorizations
         remaining (mapcat identity factorizations)]

    (if (empty? remaining)
      max-deg-factors

      (let [curr-factor (first (first remaining))
            curr-degree (second (first remaining))
            max-degree (max (get max-deg-factors curr-factor 0) curr-degree)]
        (recur (assoc max-deg-factors curr-factor max-degree)
               (rest remaining))))))

(defn digit
  "Get the nth digit of x (zero-indexed, ascending).
  i.e.  for the number 123 we have
        (digit 123 0) = 3, (digit 123 1) = 2, (digit 123 2) = 1"
  [x n]
  (let [pow-of-10 (int (Math/pow 10 (inc n)))]
    (/ (- (rem x pow-of-10) (rem x (/ pow-of-10 10))) (/ pow-of-10 10))))

(defn digit-count
  "Returns the number of digits of x"
  [x]
  (int (Math/ceil (logarithm 10 x))))

(defn palindrome?
  "Determine if a given number is a palindrome.
  90009 is a palindrome, but 123 is not."
  [x]
  (case x
    ;; avoid division by 0
    0 true

    (let [n-digits (digit-count x)]
      (every?
        true?
        (map (fn [y]
               (= (digit x y) (digit x (- (dec n-digits) y))))
             (range n-digits))))))

(defn triangle-number [degree]
  (reduce + (range 1 (inc degree))))

(defn pow [base exp] (int (Math/pow base exp)))

(defn all-factors [x]
  ;; todo: make this more efficient by using recursion and memoizing
  (letfn [(ftors [x] (map (memoize #(pow (first x) (inc %))) (range (second x))))]
    (conj (vec (cons 1
                     (mapcat identity
                             (map ftors (prime-factorization x)))))
          x)))


;;============================================================================
;; Solutions to problems
;;============================================================================

(defn problem-1 []
  (letfn [(inner [current count]
            (if (= current 1000)
              count

              (if (or (= (mod current 3) 0)
                      (= (mod current 5) 0))
                (recur (inc current) (+ count current))
                (recur (inc current) count))))]
    (inner 1 0)))

(defn problem-2 []
  (reduce +
          (filter even?
                  (take-while (partial > 4000000) (fibonacci)))))

(defn problem-3 []
  (last (prime-factors 600851475143)))

;; A palindromic number reads the same both ways.
;; The largest palindrome made from the product of two 2-digit numbers
;; is 9009 = 91 × 99.
;;
;; Find the largest palindrome made from the product of two 3-digit numbers.
(defn problem-4 []
  (let [nums-with-3-digits (range 100 1000)]
    (apply max
           (filter palindrome?
                   (map #(apply * %)
                        (combo/cartesian-product nums-with-3-digits
                                                 nums-with-3-digits))))))


(defn problem-5 []
  (reduce *
          (map (fn [x] (int (apply #(Math/pow %1 %2) x)))
               (merge-factorizations (map prime-factorization (range 1 21))))))

(defn problem-6 []
  (letfn [(square [x]
            (* x x))]
    (- (square (reduce +
                       (range 1 101)))
       (reduce +
               (map square (range 1 101))))))

;; What is the 10 001st prime number?
(defn problem-7 []
  (last (take 10001 (primes))))

(defn problem-8 []
  (let [n (str/replace
            "73167176531330624919225119674426574742355349194934
             96983520312774506326239578318016984801869478851843
             85861560789112949495459501737958331952853208805511
             12540698747158523863050715693290963295227443043557
             66896648950445244523161731856403098711121722383113
             62229893423380308135336276614282806444486645238749
             30358907296290491560440772390713810515859307960866
             70172427121883998797908792274921901699720888093776
             65727333001053367881220235421809751254540594752243
             52584907711670556013604839586446706324415722155397
             53697817977846174064955149290862569321978468622482
             83972241375657056057490261407972968652414535100474
             82166370484403199890008895243450658541227588666881
             16427171479924442928230863465674813919123162824586
             17866458359124566529476545682848912883142607690042
             24219022671055626321111109370544217506941658960408
             07198403850962455444362981230987879927244284909188
             84580156166097919133875499200524063689912560717606
             05886116467109405077541002256983155200055935729725
             71636269561882670428252483600823257530420752963450"
            #"\s"
            "")

        digits (vec (map #(Integer/parseInt (.toString %)) n))
        groups-of-13-digits (map #(subvec digits % (+ % 13))
                                 (range (- (count digits) 12)))]
    (apply max (map (partial reduce *)
                    groups-of-13-digits))))

;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;;
;; a2 + b2 = c2
;; For example, 32 + 42 = 9 + 16 = 25 = 52.
;;
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.
(defn problem-9 []
  (let [a-and-b (filter #(< (+ (first %) (second %)) 1000)
                        (combo/cartesian-product (range 1 1000) (range 1 1000)))
        a-b-c (map (fn [x] [(first x) (second x) (- 1000 (first x) (second x))])
                   a-and-b)
        sqr (fn [x] (* x x))]

    (apply *
           (first
             (filter #(= (+ (sqr (first %)) (sqr (second %))) (sqr (get % 2)))
                     a-b-c)))))

(defn problem-10 []
  (reduce + (take-while (partial > 2000000) (primes))))

(defn problem-11 []
  (let [v (map #(Integer/parseInt %)
               (str/split
                 "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
                  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
                  81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
                  52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
                  22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
                  24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
                  32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
                  67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
                  24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
                  21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
                  78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
                  16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
                  86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
                  19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
                  04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
                  88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
                  04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
                  20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
                  20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
                  01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
                 #"\s+"))
        indeces (filter #(and (>= 16 (mod % 20)) (>= 336 %)) (range 400))
        adjacency-offsets [[0 1 2 3] [20 21 22 23] [40 41 42 43] [60 61 62 63]
                           [0 20 40 60] [1 21 41 61] [2 22 42 62] [3 33 43 63]
                           [0 21 42 63] [60 41 22 3]]

        ;; get a list of all of the 4-element adjacent subsequences of the 4x4 matrix
        ;; rooted at index i
        adjacent-sequences-for-submatrix (fn [i]
                                           (map (fn [j] (map #(+ i %) j))
                                                adjacency-offsets))

        submatrices (mapcat identity
                            (map #(adjacent-sequences-for-submatrix %) indeces))]

    (map #(mapv (vec v) %) submatrices)))

(defn problem-12
  [])


(defn problem-13
  []
  (let [sum (reduce + (map bigint (str/split-lines (slurp "resources/problem-13-nums"))))]
    sum))



(defn collatz
  ([n]
   (collatz n []))

  ([n L]
   (cond (= 1 n) (conj L 1)
         (even? n) (recur (/ n 2) (conj L n))
         (odd? n) (recur (+ 1 (* 3 n)) (conj L n)))))


;;============================================================================
;; Main (just prints out solutions)
;;============================================================================

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [problems [["Multiples of 3 and 5" problem-1]
                  ["Even Fibonacci numbers" problem-2]
                  ["Largest prime factor" problem-3]
                  ["Largest palindrome product" problem-4]
                  ["Smallest multiple" problem-5]
                  ["Sum square difference" problem-6]
                  ["10001st prime" problem-7]
                  ["Largest product in a series" problem-8]
                  ["Special Pythagorean triplet" problem-9]
                  ["Summation of primes" problem-10]
                  ["Largest product in a grid" problem-11]]]
    (doseq [[problem-name problem-fn] problems]
      (println problem-name (problem-fn)))))

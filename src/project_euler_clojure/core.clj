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
    0 true                                                  ;; avoid division by 0
    (let [n-digits (digit-count x)]
      (every?
        true?
        (map (fn [y]
               (= (digit x y) (digit x (- (dec n-digits) y))))
             (range n-digits))))))

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

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
;; that the 6th prime is 13.
;;
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

  (defn problem-9 [] "todo")
  (defn problem-10 [] "todo")


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
                    ["Summation of primes" problem-10]]]
      (doseq [problem problems]
        (let [soln (problem)]
          (println (first soln) (second soln))))))

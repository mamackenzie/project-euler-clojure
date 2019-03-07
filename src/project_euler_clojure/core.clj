(ns project-euler-clojure.core
  (:gen-class))


;============================================================================
; Helper functions
;============================================================================

(defn fibonacci
  "Return a lazy sequence of Fibonacci numbers"
  ([]
   (fibonacci 0 1))
  ([a b]
   (lazy-seq
     (cons b (fibonacci b (+ a b))))))

(defn is-factor
  "Determine if y is a factor of x"
  [x y] (= 0 (rem x y)))

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

; in:   vector of factorizations
; out:  vector of the union of all of the factors of the given factorizations
;       with degree being the highest degree for that factor out of all of
;       the input factorizations
(defn merge-factorizations
  "Given a sequence of factorizations (like those returned from
  prime-factorization), return a map with keys consisting of the union of
  all of the factors from all of the factorizations and values consisting of
  the maximum degree for that factor in all of the factorizations."
  [factorizations]
  (loop [max-deg-factors (sorted-map)

         ; flattens a single level, giving us a sequence of all factors
         ; and degrees of all factorizations
         remaining (mapcat identity factorizations)]

    (if (empty? remaining)
      max-deg-factors

      (let [curr-factor (first (first remaining))
            curr-degree (second (first remaining))
            max-degree (max (get max-deg-factors curr-factor 0) curr-degree)]
        (recur (assoc max-deg-factors curr-factor max-degree)
               (rest remaining))))))


;============================================================================
; Solutions to problems
;============================================================================

(def problem-1
  (letfn [(inner [current count]
            (if (= current 1000)
              count

              (if (or (= (mod current 3) 0)
                      (= (mod current 5) 0))
                (recur (inc current) (+ count current))
                (recur (inc current) count))))]
    (inner 1 0)))

(def problem-2
  (reduce +
          (filter even?
                  (take-while (partial > 4000000) (fibonacci)))))

(def problem-3 (last (prime-factors 600851475143)))

(def problem-4
  (letfn [(square [x]
            (* x x))]
    (- (square (reduce +
                       (range 1 101)))
       (reduce +
               (map square (range 1 101))))))

(def problem-5
  (reduce *
          (map (fn [x] (int (apply #(Math/pow %1 %2) x)))
               (merge-factorizations (map prime-factorization (range 1 21))))))


;============================================================================
; Main (just prints out solutions)
;============================================================================

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [problems [["Multiples of 3 and 5" problem-1]
                  ["Even Fibonacci numbers" problem-2]
                  ["Largest prime factor" problem-3]
                  ["Sum square difference" problem-4]
                  ["Smallest multiple" problem-5]]]
    (doseq [p problems]
      (println (first p) (second p)))))

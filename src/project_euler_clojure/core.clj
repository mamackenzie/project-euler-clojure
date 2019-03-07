(ns project-euler-clojure.core
  (:gen-class))


;============================================================================
; Helper functions
;============================================================================

(defn fibonacci
  ([]
   (fibonacci 0 1))
  ([a b]
   (lazy-seq
     (cons b (fibonacci b (+ a b))))))

(defn is-factor [x y] (= 0 (rem x y)))

(def prime?
  (memoize (fn [x]
             (case x
               (0 1) false
               2 true
               (= 0 (count (filter (partial is-factor x)
                                   (range 2
                                          (+ 1 (Math/ceil (Math/sqrt x)))))))))))

(defn primes []
  (lazy-seq
    (cons 2
          (filter prime?
                  (iterate (partial + 2)
                           3)))))

(defn logarithm [base x]
  (/ (Math/log x) (Math/log base)))

(defn degree-of-factor [num factor]
  (letfn [(inner [curr degree]
            (if (= 0 (mod curr factor))
              (recur (/ curr factor) (inc degree))
              degree))]
    (inner num 0)))

(defn prime-factors [x]
  (let [small-pfs
        (filter (partial is-factor x)
                (take-while (partial > (inc (int (Math/sqrt x)))) (primes)))]
    (apply conj
           (filter prime?
                   (conj (map (fn [y] (/ x (int (Math/pow y (degree-of-factor x y)))))
                              small-pfs)
                         x))
           small-pfs)))

(defn prime-factorization [x]
  (map (fn [y] [y (degree-of-factor x y)])
       (prime-factors x)))

; in:   vector of factorizations
; out:  vector of the union of all of the factors of the given factorizations
;       with degree being the highest degree for that factor out of all of
;       the input factorizations
(defn merge-factorizations [factorizations]
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

(defn problem-4 []
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
                  ["Sum square difference" (problem-4)]
                  ["Smallest multiple" problem-5]]]
    (doseq [p problems]
      (println (first p) (second p)))))

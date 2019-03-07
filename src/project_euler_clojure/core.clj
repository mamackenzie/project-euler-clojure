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

(defn prime? [x]
  (case x
    2 true
    (= 0 (count (filter (partial is-factor x)
                        (range 2
                               (+ 1 (Math/ceil (Math/sqrt x)))))))))

(defn primes []
  (lazy-seq
    (cons 2
          (filter (memoize prime?)
                  (iterate (partial + 2)
                           3)))))

(defn logarithm [base x]
  (/ (Math/log x) (Math/log base)))

(defn prime-factors [x]
  (filter (partial is-factor x)
          (take-while (partial > (inc (Math/ceil (Math/sqrt x)))) (primes))))

(defn degree-of-factor [x y]
  (letfn [(inner [curr degree]
            (if (= 0 (mod curr y))
              (recur (/ curr y) (inc degree))
              degree))]
    (inner x 0)))

(defn prime-factorization [x]
  (map (fn [y] [y (degree-of-factor x y)])
       (prime-factors x)))

(defn merge-factorizations [facs]
  )


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

(def problem-5 "incomplete")


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

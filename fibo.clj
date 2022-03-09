(defn  fib [n](if (< n 2)n(+ (fib (- n 1)) (fib (- n 2)))))



(defn  fibl-helper [h1 h2 i n]
  (if (= i n)(+ h1 h2)
    (fibl-helper  h2 (+ h1 h2) (+ i 1) n)))

(defn  fibl [n](if (< n 2)n(fibl-helper 0N 1N 2 n)))
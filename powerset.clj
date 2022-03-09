
(def lst '(1 2 3 4))
(def answer '())
(rest lst)

  


(defn powerset_one [lst answer]
  (if (empty? lst)
    (println answer)
    (do
      (powerset_one (rest lst)  (conj answer (first lst)))
      (powerset_one (rest lst)  answer))))

(powerset_one lst answer)


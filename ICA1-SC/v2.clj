(defrecord Graph [vertices edges])

(defrecord Vertex [label lat lon city-type visited neighbors distance cost-so-far path])
(defn make-vertex [label lat lon city-type]
  (Vertex. label lat lon city-type (ref 0) (ref '()) (ref nil) (ref 0) (ref '())))

(defn make-graph []
  (Graph. (ref {}) (ref {})))

(defn graph-add-vertex! [graph label lat lon city-type]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon city-type)]
    (dosync
      (ref-set vertices (assoc @vertices label new-vertex))))
  nil)

(defrecord Edge [from to label weight])
(defn make-edge [from to label weight]
  (Edge. from to label weight))

(defn graph-edge-key [from to]
  (sort (list from to)))

(defn graph-add-edge! [graph from to label weight]
  (let [edges (:edges graph)
        vertices (:vertices graph)
        from-vertex (get @vertices from)
        to-vertex (get @vertices to)
        from-vertex-neighbors @(:neighbors from-vertex)
        to-vertex-neighbors @(:neighbors to-vertex)
        new-edge (make-edge from to label weight)
        new-edge-key (graph-edge-key from to)]
    (dosync
      (ref-set edges (assoc @edges new-edge-key new-edge))
      (ref-set (:neighbors from-vertex) (conj from-vertex-neighbors to))
      (ref-set (:neighbors to-vertex) (conj to-vertex-neighbors from))
      (ref-set vertices (assoc @vertices from from-vertex))
      (ref-set vertices (assoc @vertices to to-vertex))
      )))


(defn graph-get-neighbors [graph label]
  (let [vertex (get @(:vertices graph) label)]
    (if vertex
      @(:neighbors vertex)
      (do (println (str "Warning: No vertex found for label " label))
          [])))) ; Return an empty list if vertex doesn't exist

(defn graph-has-vertex? [graph label]
  (contains? @(:vertices graph) label))

(defn graph-has-edge? [graph from to]
  (contains? @(:edges graph) (graph-edge-key from to)))

(defn graph-reset! [graph]
  (doseq [vertex (vals @(:vertices graph))]
    (dosync (ref-set (:visited vertex) 0)
            (ref-set (:distance vertex) nil))))

(defn get-edge-weight [graph from to]
  (:weight (get @(:edges graph) (graph-edge-key from to))))

(defn reset-costs! [graph]
  (doseq [vertex (vals @(:vertices graph))]

    (dosync
      (ref-set (:cost-so-far vertex) 0))))

(defn handle-vertex
  [graph vertex path end-city-spec budget max-flights plans]

  (let [current-vertex-data (get (:vertices graph) vertex) ; Retrieve the current vertex data from the graph
        current-cost @(:cost-so-far current-vertex-data)]  ; Get the current cost from the vertex data

    (when (and (or (and (keyword? end-city-spec)
                        (= (:city-type current-vertex-data) end-city-spec))  ; Check if the city type matches the end city spec
                   (and (string? end-city-spec)
                        (= vertex end-city-spec)))  ; OR check if the vertex matches the end city spec
               (<= current-cost budget)  ; Check if the current cost is within the budget
               (<= (count path) max-flights))  ; Check if the path doesn't exceed the maximum number of flights

      ; If all conditions are met, update the plans with the new path and its cost
      (dosync
        (ref-set plans (conj @plans {:path path :cost current-cost}))))))


(defn bfs-find-plans [graph start-label end-city-spec budget max-flights]
  ; Compute the cost of the start city (self-loop).
  (let [start-cost (get-edge-weight graph start-label start-label)
        ; Initialize a queue of paths with the correct start cost.
        queue (ref [[{:vertex start-label :cost (or start-cost 0)}]])
        ; Initialize an empty list to store valid plans.
        plans (ref [])]

    ; Continue searching as long as there are paths in the queue.
    (while (not (empty? @queue))

      ; Dequeue the first path (FIFO).
      (let [path (first @queue)]

        ; Remove the path from the queue.
        (dosync (ref-set queue (rest @queue)))

        ; Extract the current vertex and its cost from the last map in the path.
        (let [current-vertex (-> path last :vertex)
              current-cost (-> path last :cost)
              ; Fetch the data associated with the current vertex from the graph.
              current-vertex-data (get @(:vertices graph) current-vertex)]

          ; Print the current exploring path for debugging purposes.
          ;(println "Exploring path:" (vec (map :vertex path)))

          ; Check if the current vertex is a valid endpoint (either matches the desired type or name)
          ; and the path respects the constraints (cost and number of flights).
          (when (and (or (and (keyword? end-city-spec) (= (:city-type current-vertex-data) end-city-spec))
                         (and (string? end-city-spec) (= current-vertex end-city-spec)))
                     (<= current-cost budget)
                     (<= (count path) max-flights))

            ; If it's a valid plan, add it to the list of plans.
            (dosync (ref-set plans (conj @plans {:path (map (fn [p] {:city (:vertex p) :cost (:cost p)}) path) :total-cost current-cost}))))

          ; Get the neighbors of the current vertex.
          (let [neighbors (graph-get-neighbors graph current-vertex)]
            (doseq [neighbor neighbors]

              ; Determine the cost to travel from the current vertex to this neighbor.
              (let [edge-cost (get-edge-weight graph current-vertex neighbor)
                    total-cost (+ current-cost edge-cost)]

                ; Check if the neighbor hasn't been visited in this path,
                ; the path respects the budget, and the number of flights.
                (when (and (not (some #(= neighbor (:vertex %)) path))
                           (<= total-cost budget)
                           (< (count path) max-flights))

                  ; If valid, enqueue a new path that includes this neighbor.
                  ; Here we update the cost for the new city in the path to be the cumulative cost up to that city.
                  (dosync
                    (alter queue conj (conj path {:vertex neighbor :cost total-cost}))))))))))

    ; Return the list of valid plans.
    @plans))

; Sorting function for the plans
(defn sort-plans [raw-plans]
  (sort-by :cost raw-plans))

(defn find-and-sort-plans [graph start-label end-city-type budget max-connections]
  ; Reset the costs before starting the search
  (reset-costs! graph)
  ; Use the BFS function to find the plans
  (let [raw-plans (bfs-find-plans graph start-label end-city-type budget max-connections)]
    ; Sort the found plans by cost
    (sort-by :cost raw-plans)))




(defn print-plan [plan max-flights end-city-type]
  (let [formatted-path (->> (:path plan)
                            (map (fn [p] (str (p :city) " (" (p :cost) ")")))
                            (clojure.string/join " --> "))
        end-city (last (:path plan))]
    (println "----------------------------------------------------")
    (if (and (= max-flights 3) (not= end-city-type "resort"))
      (println "The destination isn't suitable for families.")
      (do
        (println formatted-path)
        (println "Total Cost: " (:total-cost plan))
        (println "Amount of flights:" (dec (count (:path plan))))
        (println "----------------------------------------------------")))))

(defn format-path [path]
  (let [formatted-path (map (fn [{:keys [city cost]}]
                              (str city " (" cost ")"))
                            path)]
    (str "Path: " (clojure.string/join " --> " formatted-path))))

(defn reverse-engineer-costs [path]                         ;;just lazy to fix it in the BFS it is basically reassigning the cost
  (loop [remaining-path (reverse path) ; Reverse the path so we start from the end
         last-cost (-> path last :cost)
         result []]
    (if (empty? remaining-path)
      (reverse result) ; Return the corrected path order
      (let [current-cost (or (-> remaining-path first :cost) 0)
            calculated-cost (- last-cost current-cost)]
        (recur (rest remaining-path) current-cost
               (conj result (assoc (first remaining-path) :cost calculated-cost)))))))


(defn print-reversed-plans [plans]
  (println "Found valid plans:")
  (println (clojure.string/join "" (repeat 50 "-")))

  (doseq [plan plans]
    (let [{:keys [path total-cost]} plan
          reversed-path (reverse-engineer-costs path)
          formatted-path (format-path reversed-path)]

      ; Print the path
      (println formatted-path)

      ; Print the total cost
      (println "Total Cost:" total-cost)

      ; Print the number of flights
      (println "Amount of flights:" (count path))

      ; Print separator
      (println (clojure.string/join "" (repeat 50 "-"))))))

(let [g (make-graph)]
  ; Adding cities
  (graph-add-vertex! g "Munich" 48.1351 11.5820 "regular")
  (graph-add-vertex! g "Biarritz" 43.4832 -1.5586 "resort")
  (graph-add-vertex! g "Paris" 48.8566 2.3522 "landmark")
  (graph-add-vertex! g "Berlin" 52.5200 13.4050 "landmark")
  (graph-add-vertex! g "Madrid" 40.4168 -3.7038 "regular")
  (graph-add-vertex! g "Lisbon" 38.7223 -9.1393 "resort")

  ; Adding edges/connections
  (graph-add-edge! g "Munich" "Biarritz" "M-B" 500)
  (graph-add-edge! g "Biarritz" "Paris" "B-P" 300)
  (graph-add-edge! g "Munich" "Paris" "M-P" 700)
  (graph-add-edge! g "Munich" "Berlin" "M-Ber" 400)
  (graph-add-edge! g "Berlin" "Paris" "Ber-P" 500)
  (graph-add-edge! g "Munich" "Madrid" "M-Mad" 900)
  (graph-add-edge! g "Madrid" "Lisbon" "Mad-L" 200)
  (graph-add-edge! g "Lisbon" "Paris" "L-P" 800)

  ; Scenario for Families of 3 aiming for a landmark city
  (let [plans (find-and-sort-plans g "Munich" "Paris" 700 3)] ;;change the price to 2100 to see the difference
    (println "Families of 3 aiming for a landmark city:")
    (if (empty? plans)
      (println "No valid plans found!")
      (do
        (print-reversed-plans plans))))

  ; Scenario for Organized tours of 5 aiming for a resort town
  (let [plans (find-and-sort-plans g "Munich" "Biarritz" 1000 4)] ;;cahnge the price to 5000 to see the difference
    (println "Organized tours of 5 aiming for a resort town:")
    (if (empty? plans)
      (println "No valid plans found!")
      (do
        (print-reversed-plans plans)))))

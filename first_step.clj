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
  ; Get the start vertex from the graph based on the start label
  (let [start-vertex (get @(:vertices graph) start-label)
        ; Initialize a queue to store paths with the starting path
        queue (ref [[start-label]])
        ; Initialize a set to keep track of visited vertices
        visited (ref #{})
        ; Initialize an empty list to store plans
        plans (ref [])]
    ; Set the cost-so-far of the start vertex to 0 within a transaction
    (dosync (ref-set (:cost-so-far start-vertex) 0))

    ; While the queue is not empty
    (while (not (empty? @queue))
      ; Get the first path from the queue
      (let [path (first @queue)
            ; Get the current vertex from the last element of the path
            current-vertex (last path)
            ; Get data for the current vertex from the graph
            current-vertex-data (get @(:vertices graph) current-vertex)]
        ; Check if data exists for the current vertex
        (when current-vertex-data
          ; Get the current cost from the current vertex data
          (let [current-cost @(:cost-so-far current-vertex-data)
                ; Get neighbors of the current vertex
                neighbors (graph-get-neighbors graph current-vertex)]
            ; Remove the first path from the queue within a transaction
            (dosync (ref-set queue (rest @queue)))
            ; Check if the current vertex meets the criteria for a plan
            (when (and (or (and (keyword? end-city-spec) (= (:city-type current-vertex-data) end-city-spec))
                           (and (string? end-city-spec) (= current-vertex end-city-spec)))
                       (<= current-cost budget)
                       (<= (count path) max-flights))
              ; Add the current path and cost to the list of plans within a transaction
              (dosync (ref-set plans (conj @plans {:path path :cost current-cost}))))
            ; Iterate through the neighbors of the current vertex
            (doseq [neighbor neighbors]
              ; Get data for the neighbor vertex
              (let [neighbor-data (get @(:vertices graph) neighbor)
                    ; Get the edge cost from the current vertex to the neighbor
                    edge-cost (get-edge-weight graph current-vertex neighbor)
                    ; Calculate the total cost to reach the neighbor
                    total-cost (+ current-cost edge-cost)]
                ; Check if the neighbor has not been visited and the total cost is within budget
                (when (and (not (contains? @visited neighbor))
                           (<= total-cost budget))
                  ; Update the cost-so-far for the neighbor within a transaction
                  ; Add the neighbor to the visited set
                  ; Add a new path to the queue that includes the neighbor
                  (dosync
                    (ref-set (:cost-so-far neighbor-data) total-cost)
                    (alter visited conj neighbor)
                    (alter queue conj (conj path neighbor))))))))))
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


(defn print-plans [plans end-city-type max-flights]
  (doseq [plan plans]
    (if (and (= max-flights 3) (not= end-city-type "resort"))
      (println "The destination isn't suitable for families.")
      (let [path-str (clojure.string/join " --> " (:path plan))
            total (:cost plan)
            end-city (last (:path plan))]
        (println (str path-str " --> " end-city "\nTotal: " total "\n-----------------------------------------"))))))


(defn format-output [plans]
  (doseq [plan plans]
    (println "Path:" (clojure.string/join " --> " (:path plan))
             "Cost:" (get plan :cost))))




(let [g (make-graph)]
  (graph-add-vertex! g "Munich" 48.1351 11.5820 "regular")
  (graph-add-vertex! g "Biarritz" 43.4832 -1.5586 "resort")
  (graph-add-vertex! g "Paris" 48.8566 2.3522 "landmark")
  (graph-add-edge! g "Munich" "Biarritz" "M-B" 500)
  (graph-add-edge! g "Biarritz" "Paris" "B-P" 300)
  (graph-add-edge! g "Munich" "Paris" "M-P" 700)
  (let [plans (find-and-sort-plans g "Munich" "Paris" 1000 3)]
    (if (empty? plans)
      (println "No valid plans found!")
      (do
        (println "Found valid plans:")
        (format-output plans))))) ; assuming format-output prints each plan


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
                     (<= (- (count path) 1) max-flights))

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
                           (< (- (count path) 1) max-flights))

                  ; If valid, enqueue a new path that includes this neighbor.
                  ; Here we update the cost for the new city in the path to be the cumulative cost up to that city.
                  (dosync
                    (alter queue conj (conj path {:vertex neighbor :cost total-cost}))))))))))

    ; Return the list of valid plans.
    @plans))

(defn sort-plans [plans]
  ;(println "Sorting plans..." plans)
  (sort-by (juxt (comp - :total-cost) (comp count :path)) plans))

(defn remove-duplicate-paths [plans]
  (let [seen-flights (atom #{})]
    (filter (fn [plan]
              (let [num-flights (- (count (:path plan)) 1)]
                (if (contains? @seen-flights num-flights)
                  false
                  (do
                    (swap! seen-flights conj num-flights)
                    true))))
            plans)))


(defn get-city-type [graph city-name]
  (let [cities-map @(:vertices graph)
        city-data (get cities-map city-name)]
    (if city-data
      (str(:city-type city-data)) ;; Convert the keyword to a string
      (do
        (println "City not found:" city-name)
        nil))))


(defn find-and-sort-plans [graph start-label end-city-name budget max-flights]
  ; Get the city type for the given city name.
  (let [end-city-type  (get-city-type graph end-city-name)
        traveler-type (if (= end-city-type "resort") "family" "group")
        ; Get the valid range based on traveler type.
        valid-range (case traveler-type
                      "family" [1 3]
                      "group" [1 5]
                      (throw (Exception. (str "Invalid traveler type detected: " traveler-type))))
        ; Check if the input max-flights is within the valid range.
        hard-max-flights (if (and (>= max-flights (first valid-range))
                                  (<= max-flights (second valid-range)))
                           max-flights
                           (second valid-range))]

    (println end-city-type, traveler-type, hard-max-flights, end-city-name)
    ; Handle different traveler types
    (cond
      (and (= end-city-type  "resort" ) (> max-flights hard-max-flights))
      (throw (Exception. (str "Error: You've provided a flights limit of " max-flights " for destination type " end-city-type ". The maximum allowable limit for " end-city-type " is " hard-max-flights ".")))

      (and (= end-city-type "landmark") (> max-flights hard-max-flights))
      (throw (Exception. (str"Warning: You've provided a flights limit of " max-flights " for destination type " end-city-type ". The maximum allowable limit for " end-city-type " is " hard-max-flights ". Using the hard limit for search."))))

    ; Reset the costs before starting the search
    (reset-costs! graph)
    ; Use the BFS function to find the plans with the hard-max-flights as the constraint

    (let [raw-plans (bfs-find-plans graph start-label end-city-name budget hard-max-flights)]
      ;(println "Raw plans:" raw-plans) ; Print raw plans here

      (let [filtered-plans (filter
                             (fn [plan]
                               (and (<= (:total-cost plan) budget)
                                    (< (- (count (:path plan)) 1) hard-max-flights)))
                             raw-plans)]
        ;(println "Filtered plans:" filtered-plans) ; Print filtered plans here

        (let [sorted-plans (sort-plans filtered-plans)
              distinct-plans (remove-duplicate-paths sorted-plans)
              most-expensive-plan (first distinct-plans)
              cheapest-plan (last distinct-plans)]
          ; Check if the two plans are the same, if so, return only one
          (if (= most-expensive-plan cheapest-plan)
            [most-expensive-plan]
            [most-expensive-plan cheapest-plan]))))))



(defn format-path [path]
  (let [formatted-path (map (fn [{:keys [city cost]}]
                              (str city (if (zero? cost) "" (str " (" cost ")"))))
                            path)]
    (str "Path: " (clojure.string/join " --> " formatted-path))))


(defn reverse-engineer-costs [path] ;;just lazy to fix it in the BFS it is basically reassigning the cost
  (loop [remaining-path (reverse path) ; Reverse the path so we start from the end
         last-cost (-> path last :cost)
         result []]
    (if (empty? remaining-path)
      (reverse result) ; Return the corrected path order
      (let [current-cost (or (-> remaining-path first :cost) 0)
            calculated-cost (- last-cost current-cost)]
        (recur (rest remaining-path) current-cost
               (conj result (assoc (first remaining-path) :cost calculated-cost)))))))

(defn print-ascii-ticket [formatted-path total-cost flights]
  (let [art ["|d888888P dP  a88888b. dP     dP  88888888b d888888P"
             "|   88    88 d8'   `88 88   .d8'  88           88   "
             "|   88    88 88        88aaa8P'  a88aaaa       88   "
             "|   88    88 88        88   `8b.  88           88   "
             "|   88    88 Y8.   .88 88     88  88           88   "
             "|   dP    dP  Y88888P' dP     dP  88888888P    dP   "]

        info [(str "Path: " formatted-path)
              (str "Total Cost: " total-cost)
              (str "Amount of flights: " flights)]

        max-info-len (apply max (map count info))
        max-art-len (count (first art))
        total-len (+ max-art-len max-info-len 5)]
    (println (clojure.string/join "" (repeat total-len "-")))
    (doseq [i (range (count art))]
      (let [info-line (nth info (- i 1) nil)]
        (println (str (nth art i) " | "
                      (if info-line
                        (str info-line (apply str (repeat (- max-info-len (count info-line)) " ")))
                        (apply str (repeat max-info-len " ")))
                      " |"))))
      ; Print separator at the end only
      (println (clojure.string/join "" (repeat total-len "-")))))


  (defn print-reversed-plans [plans]
    (doseq [plan plans]
      (let [{:keys [path total-cost]} plan
            reversed-path (reverse-engineer-costs path)
            formatted-path (format-path reversed-path)]
        (print-ascii-ticket formatted-path total-cost (count path)))))




(defn get-all-cities [graph]
  (keys @(:vertices graph)))

(defn choose-city [prompt graph]
  (let [cities (get-all-cities graph)]
    (println prompt)
    (doseq [[idx city] (map vector (range 1 (inc (count cities))) cities)]
      (println (str idx ". " city " (" (get-city-type graph city) ")")))
    (let [choice-str (read-line)
          choice (if (re-matches #"\d+" choice-str) (Integer/parseInt choice-str) 0)]  ; Convert valid string to integer
      (if (and (>= choice 1) (<= choice (count cities)))
        (nth cities (dec choice))
        (do
          (println "Invalid choice. Please choose again.")
          (recur prompt graph))))))

(defn get-user-input [graph]
  (let [start-city (choose-city "Where are you located?" graph)
        end-city (choose-city "Where do you want to go to?" graph)]
    (println "How much do you want to spend?")
    (let [budget (Integer/parseInt (read-line))]
      (println "How many flights can you suffer?")
      (let [max-flights (Integer/parseInt (read-line))]
        [start-city end-city budget max-flights]))))

(defn main []
  (let [g (make-graph)]
        (graph-add-vertex! g "Munich" 48.1351 11.5820 "regular")
        (graph-add-vertex! g "Biarritz" 43.4832 -1.5586 "resort")
        (graph-add-vertex! g "Paris" 48.8566 2.3522 "resort")
        (graph-add-vertex! g "Berlin" 52.5200 13.4050 "landmark")
        (graph-add-vertex! g "Madrid" 40.4168 -3.7038 "regular")
        (graph-add-vertex! g "Lisbon" 38.7223 -9.1393 "landmark")
        (graph-add-vertex! g "Barcelona" 41.3851 2.1734 "regular")
        (graph-add-vertex! g "Rome" 41.9028 12.4964 "landmark")
        (graph-add-vertex! g "Amsterdam" 52.3676 4.9041 "regular")
        (graph-add-vertex! g "Vienna" 48.2082 16.3738 "resort")

        ; Adding edges/connections to make the routes more complex and potentially longer
        (graph-add-edge! g "Munich" "Biarritz" "M-B" 500)
        (graph-add-edge! g "Biarritz" "Paris" "B-P" 300)
        (graph-add-edge! g "Munich" "Paris" "M-P" 700)
        (graph-add-edge! g "Munich" "Berlin" "M-Ber" 400)
        (graph-add-edge! g "Berlin" "Paris" "Ber-P" 500)
        (graph-add-edge! g "Munich" "Madrid" "M-Mad" 900)
        (graph-add-edge! g "Madrid" "Lisbon" "Mad-L" 200)
        (graph-add-edge! g "Lisbon" "Paris" "L-P" 800)
        (graph-add-edge! g "Munich" "Barcelona" "M-Bar" 600)
        (graph-add-edge! g "Barcelona" "Rome" "Bar-R" 450)
        (graph-add-edge! g "Madrid" "Rome" "Mad-R" 400)
        (graph-add-edge! g "Munich" "Amsterdam" "M-Am" 450)
        (graph-add-edge! g "Amsterdam" "Vienna" "Am-V" 500)
        (graph-add-edge! g "Vienna" "Berlin" "V-Ber" 300)
        (graph-add-edge! g "Lisbon" "Vienna" "L-V" 650)
        (graph-add-edge! g "Vienna" "Biarritz" "V-B" 550)
        (graph-add-edge! g "Berlin" "Rome" "Ber-R" 700)
        (graph-add-edge! g "Rome" "Vienna" "R-V" 600)

        (let [[start-city end-city budget max-flights] (get-user-input g)
              plans (find-and-sort-plans g start-city end-city budget max-flights)]
          (println (str "Searching for plans from " start-city " to " end-city " with a budget of " budget " and maximum " max-flights " flights:"))
          ;(println plans)
          (if (nil? (first plans))
            (println "No valid plans found!")
            (print-reversed-plans plans)))))



(main)

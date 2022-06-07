
;; state 0 - not visited
;; state 1 - in open queue
;; state 2 - present ver
;; state 3 - visited


(defrecord Graph [vertices edges]) ;;create a graph file
(defn make-graph [] ;;actualy creating it
  (Graph. (ref {}) (ref {})))

;new record/structure
(defrecord Vertex [label lat lon visited neighbors distance])
(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref 0) (ref '()) (ref nil)))

(defn graph-add-vertex! [graph label lat lon] ;;adding new vertecies
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon)]
    (dosync
      (ref-set vertices (assoc @vertices label new-vertex))))
  nil)

(defrecord Edge [from to label weight]) ;;define an edge
(defn make-edge [from to label weight] ;;creating an edge
  (Edge. from to label weight))

(defn graph-edge-key [from to] ;;sorting func of the edge (from-->to) (?)
  (sort (list from to)))


(defn graph-add-edge! [graph from to label weight] ;;adding new edge
  (let [edges (:edges graph)
        vertices @(:vertices graph)
        from-vertex (get vertices from)
        to-vertex (get vertices to)
        from-vertex-neighbors (:neighbors from-vertex)
        to-vertex-neighbors (:neighbors to-vertex)
        new-edge (make-edge from to label weight)
        new-edge-key (graph-edge-key from to)]
    (dosync
      (ref-set edges (assoc @edges new-edge-key new-edge))
      (ref-set from-vertex-neighbors (conj @from-vertex-neighbors to))
      (ref-set to-vertex-neighbors (conj @to-vertex-neighbors from))
      ))
  nil)

(defn graph-get-neighbors [graph label] ;;getting the neighbors of the vertecies
  @(:neighbors (get @(:vertices graph) label)))

(defn graph-has-vertex? [graph label] ;;checking if there a specific vertex
  (contains? @(:vertices graph) label))

(defn graph-has-edge? [graph from to] ;;checking if there a specific edge (from --> to) 
  (contains? @(:edges graph) (graph-edge-key from to)))

(defn graph-reset! [graph] ;graph total reset
  (doseq [vertex (vals @(:vertices graph))]
    (dosync (ref-set (:visited vertex) 0)
            (ref-set (:distance vertex) nil))))

(defn get-edge-weight [graph from to] ;;getting the weight of the edge
  (:weight (get @(:edges graph) (graph-edge-key from to))))

(defn rest-queue! [queue label] ;;removes from the queue
  (filter
    (fn [vertex] (not (= vertex label))) queue))

(defn al_papi [queue graph] ;;looks for the best vertex and dist
  (loop [queue queue
         bestD nil
         best-vertex nil]
    (if (empty? queue)
      (:label best-vertex)
      (let [queue-label (first queue)
            queue-vertex (get @(:vertices graph) queue-label)]
        (if (or (nil? best-vertex) (< @(:distance queue-vertex) bestD))
          (recur (rest queue) @(:distance queue-vertex) queue-vertex)
          (recur (rest queue) bestD best-vertex))))))

(defn graph_bfs! ;;actual bfs
  ([graph]
   (graph_bfs! graph (first (keys @(:vertices graph)))))
  ([graph start]
   (graph_bfs! graph start (fn [vertex] nil)))
  ([graph start func]
   (graph_bfs! graph start func first))
  ([graph start func func-m]
   (let [vertices @(:vertices graph)]
     (loop [queue (list start)]
       (when (not (empty? queue))
         (let [present-label (if (= func-m al_papi)(func-m queue graph)(func-m queue))
               rest-queue (rest-queue! queue present-label)
               present-vertex (get vertices present-label)
               present-neighbors @(:neighbors present-vertex)
               visited-status (:visited present-vertex)
               
               unseen-neighbors (filter
                                  (fn [label]
                                    (= @(:visited (get vertices label)) 0))
                                  present-neighbors)
               ]
           (dosync (ref-set visited-status 2))
           (func present-vertex)
           (dosync (ref-set visited-status 3))
           (doseq [label unseen-neighbors]
             (dosync
               (ref-set (:visited (get vertices label)) 1)))
           (recur (concat rest-queue unseen-neighbors))))))))


(defn dijkstra_mark! [graph finish use-weights]
  (let [vertices @(:vertices graph)
        start-vertex (get vertices finish)]
    (graph-reset! graph)
    (dosync
      (ref-set (:distance start-vertex) 0))
    (if (not use-weights)
      (graph_bfs! graph
                  finish
                  (fn [vertex]
                    (let [next-distance (inc @(:distance vertex))]
                      (doseq [neighbor-label @(:neighbors vertex)]
                        (let [neighbor (get vertices neighbor-label)]
                          (if (= @(:visited neighbor) 0)
                            (dosync
                              (ref-set (:distance neighbor) next-distance))))))))
      (graph_bfs! graph
                  finish
                  (fn [vertex]
                    (doseq [neighbor-label @(:neighbors vertex)]
                      (let [neighbor (get vertices neighbor-label)
                            next-distance (+ @(:distance vertex) (get-edge-weight graph (:label vertex) neighbor-label))]
                        (when (or (= @(:visited neighbor) 0) (> @(:distance neighbor) next-distance))
                          (dosync
                            (ref-set (:distance neighbor) next-distance))))))
                  al_papi))))

(defn best_neighbor [graph vertices vertex use-weights] ;;looking for the best neighbor 
  (loop [neighbors-labels @(:neighbors vertex)
         bestD nil
         best-vertex nil]
    (if (empty? neighbors-labels)
      best-vertex
      (let [vertex-label (:label vertex)
            distance-vertex @(:distance vertex)
            neighbor-label (first neighbors-labels)
            neighbor-vertex (get vertices neighbor-label)
            neighbor-distance @(:distance neighbor-vertex)
            edge-key (graph-edge-key vertex-label neighbor-label)
            edges @(:edges graph)
            edge-weight (:weight (get edges edge-key))]
        ;;(println edge-weight distance-vertex neighbor-distance)
        (if (not use-weights)
          (if (or (nil? best-vertex) (< neighbor-distance bestD))
            (recur (rest neighbors-labels) neighbor-distance neighbor-vertex)
            (recur (rest neighbors-labels) bestD best-vertex))
          (if (= (- distance-vertex neighbor-distance) edge-weight)
            (if (or (nil? best-vertex) (< neighbor-distance bestD))
              (recur (rest neighbors-labels) neighbor-distance neighbor-vertex)
              (recur (rest neighbors-labels) bestD best-vertex))
            (recur (rest neighbors-labels) bestD best-vertex)))))))

(defn dijkstra_trace [graph start use-weights] ;;the trace
  (let [vertices @(:vertices graph)
        start-vertex (get vertices start)]
    (if (= @(:visited start-vertex) 0)
      (println "There is no path!")
      (loop [present-vertex start-vertex]
        ;(println present-vertex)
        (println (:label present-vertex) )
        (if (> @(:distance present-vertex) 0)
          (recur (best_neighbor graph vertices present-vertex use-weights)))))))

(defn graph_dijkstra! [graph start finish weighted]
  (graph-reset! graph)
  (dijkstra_mark! graph finish weighted)
  (dijkstra_trace graph start weighted))


(load-file "e-roads-2020-full.clj")
;Wieght graph
(println " 
          P
          ")
(println "Problem 1 Non-wieght graph: From: Paris | To: Prague")
(graph_dijkstra! g "Paris" "Prague" false)
(println " ")
(println "Problem 1.1 Non-wieght graph without path: From: Newport, Wales | To: Prague")
(graph_dijkstra! g "Newport, Wales" "Prague" false)
(println "         
          ")
(println "_______________")
(println "          
          ")
(println "Problem 2 Wieght graph: From: Paris | To: Prague")
(graph_dijkstra! g "Paris" "Prague" true)
(println " ")
(println "Problem 2.1 Wieght graph without a path: From: Newport, Wales | To: Prague")
(graph_dijkstra! g "Newport, Wales" "Prague" true)

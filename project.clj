(defproject app "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [ubergraph "0.8.2"]
                 [org.clojure/tools.trace "0.7.9"]]
  :repl-options {:init-ns app.core})

(require 'clojure.tools.trace)
;; state 0 - not encountered at all
;; state 1 - in the open queue
;; state 2 - current vertex
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

(defn al-papi [queue graph] ;;looks for the best vertex and dist
  (loop [queue queue
         best-distance nil
         best-vertex nil]
    (if (empty? queue)
      best-vertex
      (let [queue-label (first queue)
            queue-vertex (get @(:vertices graph) queue-label)]
        (if (or (nil? best-vertex) (< @(:distance queue-vertex) best-distance))
          (recur (rest queue) @(:distance queue-vertex) queue-vertex)
          (recur (rest queue) best-distance best-vertex))))))

(defn graph-bfs!
  ([graph]
   (graph-bfs! graph (first (keys @(:vertices graph)))))
  ([graph start]
   (graph-bfs! graph start (fn [vertex] nil)))
  ([graph start func]
   (graph-bfs! graph start func first))
  ([graph start func func-m]
   (let [vertices @(:vertices graph)]
     (loop [queue (list start)]
       (when (not (empty? queue))
         (let [current-label (if (= func-m al-papi)(func-m queue graph)(func-m queue))
               rest-queue (rest-queue! queue current-label)
               current-vertex (get vertices current-label)
               visited-status (:visited current-vertex)
               current-neighbors @(:neighbors current-vertex)
               unseen-neighbors (filter
                                  (fn [label]
                                    (= @(:visited (get vertices label)) 0))
                                  current-neighbors)
               ]
           (dosync (ref-set visited-status 2))
           (func current-vertex)
           (dosync (ref-set visited-status 3))
           (doseq [label unseen-neighbors]
             (dosync
               (ref-set (:visited (get vertices label)) 1)))
           (recur (concat rest-queue unseen-neighbors))))))))


(defn graph-dijkstra-mark! [graph finish use-weights]
  (let [vertices @(:vertices graph)
        start-vertex (get vertices finish)]
    (graph-reset! graph)
    (dosync
      (ref-set (:distance start-vertex) 0))
    (if (not use-weights)
      (graph-bfs! graph
                  finish
                  (fn [vertex]
                    (let [next-distance (inc @(:distance vertex))]
                      (doseq [neighbor-label @(:neighbors vertex)]
                        (let [neighbor (get vertices neighbor-label)]
                          (if (= @(:visited neighbor) 0)
                            (dosync
                              (ref-set (:distance neighbor) next-distance))))))))
      (graph-bfs! graph
                  finish
                  (fn [vertex]
                    (doseq [neighbor-label @(:neighbors vertex)]
                      (let [neighbor (get vertices neighbor-label)
                            next-distance (+ @(:distance vertex) (get-edge-weight graph (:label vertex) neighbor-label))]
                        (println "There is bfs!")
                        (when (or (= @(:visited neighbor) 0) (> @(:distance neighbor) next-distance))
                          (dosync
                            (ref-set (:distance neighbor) next-distance))))))
                  al-papi))))

(defn best-neighbor [graph vertices vertex use-weights]
  (loop [neighbors-labels @(:neighbors vertex)
         best-distance nil
         best-vertex nil]
    (if (empty? neighbors-labels)
      best-vertex
      (let [vertex-label (:label vertex)
            distance-vertex @(:distance vertex)
            neighbor-label (first neighbors-labels)
            neighbor-vertex (get vertices neighbor-label)
            neighbor-distance @(:distance neighbor-vertex)
            edge-key (graph-edge-key vertex-label neighbor-label)
            edges (:edges graph)
            edge-weight (:weight (get edges edge-key))]
        
        (if (not use-weights)
          (if (or (nil? best-vertex)(< neighbor-distance best-distance))
            (recur (rest neighbors-labels) neighbor-distance neighbor-vertex)
            (recur (rest neighbors-labels) best-distance best-vertex))
          (if (= (- distance-vertex neighbor-distance) edge-weight)
            (if (or (nil? best-vertex)(< neighbor-distance best-distance))
              (recur (rest neighbors-labels) neighbor-distance neighbor-vertex)
              (recur (rest neighbors-labels) best-distance best-vertex))
            (recur (rest neighbors-labels) best-distance best-vertex)))))))

(defn graph-dijkstra-trace [graph start use-weights]
  (let [vertices @(:vertices graph)
        start-vertex (get vertices start)]
    (if (= @(:visited start-vertex) 0)
      (println "There is no path!")
      (loop [current-vertex start-vertex]
        (println (:label current-vertex))
        (if (> @(:distance current-vertex) 0)
          (recur (best-neighbor graph vertices current-vertex use-weights)))))))

(defn graph-dijkstra! [graph start finish weighted]
  (graph-reset! graph)
  (graph-dijkstra-mark! graph finish weighted)
  (graph-dijkstra-trace graph start weighted))


(load-file "e-roads-2020-full.clj")
;Wieght graph
(println "Problem 1")
(graph-dijkstra! g "Berlin" "Prague" false)
(graph-dijkstra! g "Newport, Wales" "Prague" false)

;Non-w graph
(graph-dijkstra! g "Berlin" "Prague" true)
(graph-dijkstra! g "Newport, Wales" "Prague" true)


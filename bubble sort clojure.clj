(defrecord  SListNode [next data])
(defrecord  SList [head])
(defn  make-slist  [] (SList. (ref nil)))

(defn slist-empty? [lst] (nil? (deref (:head lst))))


(defn slist? [lst] (= (class lst) SList))

;; Adding new element to thehead(front) of the list, 2 ways:
(defn  slist-prepend! [lst val]
  (dosync (ref-set  (:head  lst) (SListNode. (ref (deref  (:head  lst))) val))) val)


(defn slist-node-pair? [node]
        (and (not (nil? node))
             (not (nil? (:next @node)))))


(defn slist-pair-sorted? [node]
  (if (< (:data @node) (:data @(:next @node)))
    true
    false))

(defn slist-node-swap [node]
  (let [W node
        W2 @node
        X (:next @node)
        Y (:next @(:next @node))]
    (dosync
     (ref-set W @X)
     (ref-set X @Y)
     (ref-set Y W2))) nil)

(defn slist-print [node]
    (when (not(nil? @node))
      (println (:data @node))
      (slist-print (:next @node))))


(defn slist-sorted? [lst]
  (loop [node (:head  lst)]
    (if (slist-node-pair? node)
      (if (slist-pair-sorted? node)
        (recur (:next  @node))
        false)
      true)))

(defn slist-swap-clever! [node]
  (if (slist-pair-sorted? node)
    false
    (do
      (slist-node-swap node)
      true)))


(defn slist-bubble-iter! [lst]
  (loop [node (:head lst)
         swapped 0]
    (if (slist-node-pair? node)
      (recur (:next @node)
             (if (slist-node-swap node)
               (inc swapped)
               swapped))
      swapped)))

  
(defn slist-bubble-sort! [lst]
  (if (> (slist-bubble-iter! lst) 0)
    (recur lst)))


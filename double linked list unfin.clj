
(let [a (make-list)]
  (dlist-empty? a)
  (dlist-ok? a)
  (dlist-prepend a 3)
  (dlist-prepend a 5)
  (dlist-prepend a 8)
  (dlist-itr a println))



(defrecord make-list-node [prev next data])
(defrecord make-list-head+tail [head tail])

(defn make-list []
  (make-list-head+tail. (ref nil) (ref nil))) ;creating a list

(defn dlist-empty? [lst]
  (if (and (nil? (deref (:head lst))) (nil? (deref (:tail lst))))
    true
    false))

(defn dlist-ok? [lst]
  (if (= (nil? (deref (:head lst))) (nil? (deref (:tail lst))))
    true
    false))

(defn dlist? [lst] (= (class lst) make-list-head))

(defn dlist-prepend [lst val]
  (let [new-node (make-list-node.  (ref nil) (ref (deref (:head lst))) val)] ;;creating new node
    (if (dlist-empty? lst) ;;if the list is a new one we create a new list with values
      (dosync
       (ref-set (:head lst) new-node)
       (ref-set (:head lst) new-node))
      (dosync
       (ref-set (:prev (deref (:head lst))) new-node) ;;creating new head and adding new element
       (ref-set (:head lst) new-node)))))

(defn  dlist-itr [lst  func] ;;runs threwout the whole list
  (loop [node (deref  (:head  lst))]
    (if (not (nil? node))
      (do
        (func (:data  node))
        (recur (deref  (:next  node)))))))

(defn  dlist-itr-rev [lst  func] ;;runs threwout the whole list reverce
  (loop [node (deref  (:tail  lst))]
    (if (not (nil? node))
      (do
        (func (:data  node))
        (recur (deref  (:prev  node)))))))

(defn dlist-rmv-first [lst] ;;removing last element
  (if (not (dlist-empty? lst))
    (= (deref (:head lst))) (deref (:tail lst)))
  (dosync (ref-set (:head lst) nil)
          (ref-set (:tail lst) nil))
  (dosync (ref-set (:head lst)(deref(:next (deref (:head lst)))))
          (ref-set (:prev(deref (:head lst))) nil)))

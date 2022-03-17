
(let [a (make-list)]
  (dlist-empty? a)
  (dlist-ok? a)
  (dlist-prepend a 3)
  (dlist-prepend a 5)
  (dlist-prepend a 8)
  ;(dlist-rem-first! a)
  (dlist-prepend a 4)
  (dlist-prepend a 951753)
  (dlist-prepend a 8)
  ;(dlist-rem-first! a)
  ;(dlist-itr a println)
  (dlist-rest a)
  (println "_______")
  ;(dlist-rev-rest a)
  (println "___TEST___")
  (dlist-append! a 99)
  (dlist-rem-last! a))



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

(defn dlist? [lst] (= (class lst) make-list-head+tail))

(defn dlist-prepend [lst val]
  (let [new-node (make-list-node.  (ref nil) (ref (deref (:head lst))) val)] ;;creating new node
    (if (dlist-empty? lst) ;;if the list is a new one we create a new list with values
      (dosync
       (ref-set (:head lst) new-node)
       (ref-set (:tail lst) new-node))
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

(defn dlist-rem-first! [lst] ;; removing the first element
  (if (not (dlist-empty? lst))
    (if (= (deref (:head lst)) (deref (:tail lst)))
      (dosync (ref-set (:head lst) nil)
              (ref-set (:tail lst) nil))
      (dosync (ref-set (:head lst) (deref (:next (deref (:head lst)))))
              (ref-set (:prev (deref (:head lst))) nil)))))
;;__________________________________________________________________________made in class
(defn dlist-first [lst] ;;if its the val of the first you dont need to touch the tail.
  (:data (deref (:head lst)))) 

(defn dlist-rest [lst] ;; skippes the first and then runs threw out the list but it cheack if the next head is not nil
  (if (nil? (:next (deref (:head lst))))
    nil
    (loop [node (deref  (:next (deref  (:head  lst))))] 
      (if (not (nil? node))
        (do
          (println (:data node))
          (recur (deref  (:next  node))))))))

(defn dlist-last [lst]
  (:data @(:tail lst)))

(defn dlist-rev-rest [lst] ;;start from the end and prints all the list without the last one(first one from end)
  (loop [node (deref (:prev (deref  (:tail  lst))))]
    (if (not (nil? node))
      (do
        (println (:data node))
        (recur (deref  (:prev  node)))))))
  

(defn dlist-append! [lst val]
  (let [new-node (make-list-node. (ref (deref (:tail lst))) (ref nil)  val)] ;;creating new node
    (if (dlist-empty? lst) ;;if the list is a new one we create a new list with values
      (dosync
       (ref-set (:head lst) new-node)
       (ref-set (:tail lst) new-node))
      (dosync
       (ref-set (:next (deref (:tail lst))) new-node) ;;creating new head and adding new element
       (ref-set (:tail lst) new-node)))) nil)

(defn dlist-rem-last! [lst];;removes last
  (if (not (dlist-empty? lst))
    (if (= (deref (:head lst)) (deref (:tail lst)))
      (dosync (ref-set (:head lst) nil)
              (ref-set (:tail lst) nil))
      (dosync (ref-set (:tail lst) (deref (:prev (deref (:tail lst)))))
              (ref-set (:prev (deref (:tail lst))) nil)))))



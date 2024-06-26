(ns airlines.core
  (:require [clojure.core.matrix :as m]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.apache.commons.math3.distribution TDistribution)))

; Loading and Preprocessing the CSV Data
(defn read-csv [filename]
  (with-open [reader (io/reader filename)]
    (doall (csv/read-csv reader))))

(defn parse-row [row]
  {:full-name (nth row 0)
   :yob (Integer/parseInt (nth row 1))
   :departure (nth row 2)
   :destination (nth row 3)
   :paid (Double/parseDouble (nth row 4))})

(defn process-csv [filename]
  (map parse-row (rest (read-csv filename)))) ; 'rest' to skip header

; Print Raw Data for Verification
(defn print-raw-data [filename]
  (let [data (process-csv filename)]
    (doseq [row data]
      (println row))))

; Data Transformation Functions
(defn split-name [record]
  (let [[name surname] (str/split (:full-name record) #" ")]
    (assoc record :name name :surname surname)))

(defn calculate-age [record current-year]
  (assoc record :age (- current-year (:yob record))))

; Utility Functions
(defn adult? [age] (>= age 18))

; Identifying Families and Groups
(defn identify-family-group [transformed-data]
  (let [grouped-by-common-traits (group-by #(vector (:surname %) (:departure %) (:destination %) (:paid %)) transformed-data)]
    (mapcat (fn [[_ group]]
              (let [adults (filter #(adult? (:age %)) group)
                    children (remove #(adult? (:age %)) group)]
                (if (and (>= (count adults) 2) (>= (count children) 1))
                  (map #(assoc % :relation "family") group)
                  (map #(assoc % :relation "group") group))))
            grouped-by-common-traits)))

; Data Transformation Functions (Updated)
(defn transform-data [dataset current-year]
  (->> dataset
       (map #(-> % (split-name) (calculate-age current-year)))
       (identify-family-group)))

(defn calculate-success-rates [transformed-data]
  (let [grouped-by-route (group-by #(vector (:departure %) (:destination %)) transformed-data)
        max-prices (->> grouped-by-route
                        (map (fn [[k v]] [k (apply max (map :paid v))]))
                        (into {}))
        with-max-price-flag (map #(assoc % :max-price-sold (= (:paid %)
                                                              (get max-prices [(:departure %) (:destination %)])))
                                 transformed-data)
        grouped-by-category (group-by #(vector (:relation %) (:departure %) (:destination %))
                                      with-max-price-flag)]
    (->> grouped-by-category
         (map (fn [[k v]]
                (let [total (count v)
                      max-price-sold-count (count (filter :max-price-sold v))
                      success-rate (float (/ max-price-sold-count total))]
                  {:group-type (nth k 0)
                   :departure (nth k 1)
                   :destination (nth k 2)
                   :success-rate success-rate})))
         (into []))))

(defn predict-future-sales-with-demand [transformed-data]
  (let [grouped-by-route (group-by #(vector (:departure %) (:destination %)) transformed-data)
        max-prices (->> grouped-by-route
                        (map (fn [[k v]] [k (apply max (map :paid v))]))
                        (into {}))
        with-max-price-flag (map #(assoc % :max-price-sold (= (:paid %)
                                                              (get max-prices [(:departure %) (:destination %)])))
                                 transformed-data)
        grouped-by-category (group-by #(vector (:relation %) (:departure %) (:destination %))
                                      with-max-price-flag)
        max-price-proportion (->> grouped-by-category
                                  (map (fn [[k v]]
                                         (let [total (count v)
                                               max-price-sold-count (count (filter :max-price-sold v))]
                                           [k (float (/ max-price-sold-count total))])))
                                  (into {}))
        total-sales (->> grouped-by-category
                         (map (fn [[k v]] [k (count v)]))
                         (into {}))
        predicted-sales (map (fn [[k max-price-prop]]
                               (let [total (get total-sales k)]
                                 {:group-type (nth k 0)
                                  :departure (nth k 1)
                                  :destination (nth k 2)
                                  :max-price-proportion max-price-prop
                                  :total-sales total
                                  :predicted-demand (* max-price-prop total)}))
                             max-price-proportion)]
    predicted-sales))

(defn calculate-purchase-probability-with-new-analysis
  [transformed-data increase-percentage statistics success-rates future-sales-predictions]
  (let [increase-fn (fn [paid] (+ paid (* paid increase-percentage 0.01)))
        with-new-max-price (map #(assoc % :new-max-price (increase-fn (:paid %))) transformed-data)
        merge-fn (fn [record]
                   (let [matching-statistics (first (filter #(and (= (:group-type %) (:relation record))
                                                                  (= (:departure %) (:departure record))
                                                                  (= (:destination %) (:destination record)))
                                                            statistics))
                         matching-success-rates (first (filter #(and (= (:group-type %) (:relation record))
                                                                     (= (:departure %) (:departure record))
                                                                     (= (:destination %) (:destination record)))
                                                               success-rates))
                         matching-future-sales (first (filter #(and (= (:group-type %) (:relation record))
                                                                    (= (:departure %) (:departure record))
                                                                    (= (:destination %) (:destination record)))
                                                              future-sales-predictions))]
                     (merge record matching-statistics matching-success-rates matching-future-sales)))
        merged-data (map merge-fn with-new-max-price)
        calculate-probability (fn [row]
                                (if (<= (:new-max-price row) (:max row))
                                  (min (* (:success-rate row) (:predicted-demand row)) 1)
                                  0))
        with-probability (map #(assoc % :probability-with-increase (calculate-probability %)) merged-data)
        filtered-data (filter #(> (:probability-with-increase %) 0) with-probability)]
    (distinct (map #(select-keys % [:group-type :departure :destination :probability-with-increase]) filtered-data))))


(defn mean [values]
  (let [sum (reduce + 0 values)
        count (count values)]
    (/ sum count)))

(defn calculate-statistics [transformed-data]
  (let [grouped-data (group-by #(vector (:relation %) (:departure %) (:destination %)) transformed-data)
        calculate-stats (fn [[k v]]
                          (let [paid-values (map :paid v)
                                max-val (apply max paid-values)
                                min-val (apply min paid-values)
                                mean-val (mean paid-values) ; Using the custom mean function
                                count-val (count paid-values)]
                            {:group-type (nth k 0)
                             :departure (nth k 1)
                             :destination (nth k 2)
                             :max max-val
                             :min min-val
                             :mean mean-val
                             :count count-val}))]
    (map calculate-stats grouped-data)))

(defn run-analysis [filename current-year increase-percentage]
  (let [raw-data (process-csv filename)
        transformed-data (transform-data raw-data current-year)
        statistics (calculate-statistics transformed-data)
        success-rates (calculate-success-rates transformed-data)
        future-sales-predictions (predict-future-sales-with-demand transformed-data)
        purchase-probabilities (calculate-purchase-probability-with-new-analysis transformed-data increase-percentage statistics success-rates future-sales-predictions)]
    (println "Basic Statistics:")
    (doseq [stat statistics]
      (println stat))
    (println "\nSuccess Rates:")
    (doseq [rate success-rates]
      (println rate))
    (println "\nPredicted Future Sales:")
    (doseq [prediction future-sales-predictions]
      (println prediction))
    (println "\nPurchase Probabilities:")
    (doseq [probability purchase-probabilities]
      (println probability))))


(run-analysis "F:/clojure-course/airlines/src/airlines/sales_team_4.csv" 2023 4)



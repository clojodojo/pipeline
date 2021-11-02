(ns demo.ui
 (:require
    [reagent.core :as r]
    [sci.core :as sci]))

(defn remove-from-vector [vector i]
  (vec (concat (subvec vector 0 i)
               (subvec vector (inc i)))))

#_(remove-from-vector [:a :b :c :d] 2)

(defonce state
  (r/atom
    {:initial-input "{:x 1\n:y 2}"
     :steps [{:code "(fn [i] (+ (:x i) (:y i)))"}]}))

(defn change-initial-input! [e]
  (swap! state assoc :initial-input
    (.. e -target -value)))

(defn add-new-step! []
  (swap! state update :steps conj {:code "(fn [i] i)"}))

(defn remove-step! [i]
  (swap! state update :steps remove-from-vector i))

(defn calculate-results! []
   (try
     (vec (reductions (fn [memo f]
                       (f memo))
              (sci/eval-string (:initial-input @state))
              (map (fn [step]
                     (sci/eval-string (:code step)))
                   (:steps @state))))
     (catch js/Error e
       [])))

#_(calculate-results!)

(defn app-view []
  (let [results (calculate-results!)]
   [:div
    [:textarea {:value (:initial-input @state)
                :on-change change-initial-input!}]
    (for [[index step] (map-indexed vector (:steps @state))]
      [:div
       [:button {:on-click (fn [_] (remove-step! index))} "x"]
       [:textarea {:value (:code step)
                   :on-change (fn [e]
                               (swap! state assoc-in [:steps index :code] (.. e -target -value)))}]

       [:span {} (pr-str (get results (inc index)))]])
    [:button {:on-click add-new-step!} "+"]]))


;; deleting
;; moving
;; ineserting in middle

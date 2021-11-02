(ns demo.ui
 (:require
    [reagent.core :as r]
    [sci.core :as sci]))

(defonce state
  (r/atom
    {:initial-input "{:x 1\n:y 2}"
     :steps [{:code "(fn [i] (+ (:x i) (:y i)))"}]}))

(defn change-initial-input! [e]
  (swap! state assoc :initial-input
    (.. e -target -value)))

(defn add-new-step! []
  (swap! state update :steps conj {:code "(fn [i] i)"}))

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
       [:textarea {:value (:code step)
                   :on-change (fn [e]
                               (swap! state assoc-in [:steps index :code] (.. e -target -value)))}]

       [:span {} (pr-str (get results (inc index)))]])
    [:button {:on-click add-new-step!} "+"]]))


;; deleting
;; moving
;; ineserting in middle

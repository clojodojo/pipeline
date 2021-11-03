(ns demo.ui
 (:require
    [reagent.core :as r]
    [sci.core :as sci]))

(defn remove-from-vector [vector i]
  (vec (concat (subvec vector 0 i)
               (subvec vector (inc i)))))

#_(remove-from-vector [:a :b :c :d] 2)

(defn insert-in-vector [vector i value]
  (vec (concat (subvec vector 0 i)
               [value]
               (subvec vector i))))


#_(insert-in-vector [:a :b :c] 0 :new)

(defonce state
  (r/atom
    {:initial-input "{:x 1\n:y 2}"
     :steps [{:code "(fn [i] (+ (:x i) (:y i)))"}]}))

(defn change-initial-input! [e]
  (swap! state assoc :initial-input
    (.. e -target -value)))

(defn insert-step-before! [i]
  (swap! state update :steps insert-in-vector i {:code "(fn [i] i)"}))

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
    [:div.initial-value
     [:textarea {:value (:initial-input @state)
                 :on-change change-initial-input!}]]
    [:div
     [:button {:on-click (fn [_] (insert-step-before! 0))} "+"]]
    (for [[index step] (map-indexed vector (:steps @state))]
      ^{:key index}
      [:<>
       [:div.step
        [:textarea {:value (:code step)
                    :on-change (fn [e]
                                (swap! state assoc-in [:steps index :code] (.. e -target -value)))}]

        [:span {} (pr-str (get results (inc index)))]
        [:button {:on-click (fn [_] (remove-step! index))} "x"]]
       [:div
        [:button {:on-click (fn [_] (insert-step-before! (inc index)))} "+"]]])]))


;; deleting
;; moving
;; ineserting in middle

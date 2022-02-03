(ns demo.ui
 (:require
    [reagent.core :as r]
    [sci.core :as sci]
    [demo.toposort :as toposort]))

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
    {:steps [{:label "$A"
              :code "[10 15 26]"}
             {:label "$B"
              :code "(map inc $A)"}
             {:label "$C"
              :code "(reduce + $B)"}
             {:label "$D"
              :code "(count $A)"}
             {:label "$E"
              :code "(/ $C $D)"}]}))

(defn generate-new-label [steps]
  (let [letters (mapv char (range (.charCodeAt "A") (.charCodeAt "Z")))
        new-label (apply str "$" (repeatedly 3 #(rand-nth letters)))]
    (if (some (fn [step] (= (step :label) new-label)) steps)
     (recur steps)
     new-label)))

(defn insert-step-before! [label]
  (swap! state update :steps
    (fn [steps]
      (let [[before after] (split-with (fn [step] (not= (step :label) label)) steps)]
       (concat before
               [{:label (generate-new-label steps)
                 :code "nil"}]
               after)))))

(defn remove-step! [label]
  (swap! state update :steps (fn [steps] (remove (fn [step] (= (step :label) label)) steps))))

(defn edit-step-code! [label code]
  (swap! state update :steps (fn [steps]
                               (map (fn [step]
                                     (if (= (:label step)
                                            label)
                                      (assoc step :code code)
                                      step))
                                    steps))))


(defn analyze
  "For each step, identifies which steps it depends on"
  [steps]
  (->> steps
       (map (fn [step]
              [(:label step) (set (re-seq #"\$[A-Z]+" (:code step)))]))
       (into {})))

#_(toposort/toposort (analyze [{:label "$A"
                                :code "[10 15 26]"}
                               {:label "$B"
                                :code "(map inc $A)"}
                               {:label "$C"
                                :code "(reduce + $B)"}
                               {:label "$E"
                                :code "(/ $C $D)"}
                               {:label "$D"
                                :code "(count $A)"}]))

#_{"$A" #{}
   "$B" #{"$A"}
   "$C" #{"$B"}
   "$D" #{"$A"}
   "$E" #{"$C" "$D"}}


(defn analyze-and-reorder [steps]
  (let [labels-in-order (toposort/toposort (analyze steps))
        label->step (zipmap (map :label steps)
                            steps)]
    (map (fn [l] (label->step l)) labels-in-order)))

#_(analyze-and-reorder [{:label "$A"
                         :code "[10 15 26]"}
                        {:label "$B"
                         :code "(map inc $A)"}
                        {:label "$C"
                         :code "(reduce + $B)"}
                        {:label "$E"
                         :code "(/ $C $D)"}
                        {:label "$D"
                         :code "(count $A)"}])

(defn re-order! []
  (swap! state update :steps analyze-and-reorder))

(defn calculate-results! [steps]
  (try
   (loop [context {}
          remaining-steps (analyze-and-reorder steps)]
    (let [{:keys [label code]} (first remaining-steps)
          result (try
                   (sci/eval-string code {:namespaces {'user context}})
                   (catch js/Error e
                     e))]
     (cond
       (= (type result) ExceptionInfo)
       (assoc context (symbol label) result)

       (seq remaining-steps)
       (recur (assoc context (symbol label) result)
              (rest remaining-steps))

       :done
       context)))
   (catch js/Error e
     {})))

#_(let [steps [{:label "A"
                :code "[10 15 26]"}
               {:label "B"
                :code "(map inc A)"}
               {:label "C"
                :code "(reduce + B)"}
               {:label "D"
                :code "(count A)"}
               {:label "E"
                :code "(/ C D)"}]]
    (calculate-results! steps))


#_(sci/eval-string "(+ A 3)" {:namespaces {'user {'A 7}}})
#_(calculate-results!)

(defn app-view []
  [:div
   [:button {:on-click (fn [] (re-order!))} "Re-order"]
   (let [results (calculate-results! (@state :steps))]
    [:table
     [:tbody
      (for [{:keys [label code]} (:steps @state)
            :let [result (get results (symbol label) ::NO-RESULT)]]
        ^{:key label}
        [:<>
         [:tr
          [:td
           [:button {:on-click (fn [_] (insert-step-before! label))} "+"]]]
         [:tr.step
          [:td label]
          [:td
           [:textarea {:value code
                       :on-change (fn [e]
                                    (edit-step-code! label (.. e -target -value)))}]]
          [:td
           [:span "=>"]]
          [:td
           [:span {}
             (cond
              (= (type result) ExceptionInfo)
              (.-message result)
              (= result ::NO-RESULT)
              ""
              :else
              (pr-str result))]]
          [:td
           [:button {:on-click (fn [_] (remove-step! label))} "x"]]]])
      [:tr
       [:td
        [:button {:on-click (fn [_] (insert-step-before! nil))} "+"]]]]])])


;; inserting
;; renaming label
;; change :label and :code to :step/label and :step/code
;; maybe explore using specter
;; code autoformatting
;; uploading a csv

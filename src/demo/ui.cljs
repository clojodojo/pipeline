(ns demo.ui
 (:require
    [reagent.core :as r]
    [clojure.string :as string]
    [clojure.set :as set]
    [sci.core :as sci]
    [react-flow-renderer :default ReactFlow :refer [Handle]]
    [react-flow-renderer :as flow]
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

(defn normalize-label
  "Returns normalize label or nil if can't normalize"
  [s]
  (let [s (-> s
              (string/upper-case)
              (string/replace #"[^A-Z]" ""))]
   (when-not (string/blank? s)
    (str "$" s))))

#_(normalize-label "e % 2")

(defn label-exists? [steps label]
  (some (fn [step] (= (step :label) label)) steps))

(defn analyze
  "For each step, identifies which steps it depends on"
  [steps]
  (let [labels (set (map :label steps))]
   (->> steps
        (map (fn [step]
               [(:label step)
                ;; remove edges that don't exist
                ;; b/c ELK explodes downstream if there
                ;; are references to non-existing nodes
                (set/intersection labels
                                  (set (re-seq #"\$[A-Z]+" (:code step))))]))
        (into {}))))

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

(defn calculate-results
  "Gotcha: returned keys are symbols not strings or keywords"
  [steps]
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
    (calculate-results steps))

#_(sci/eval-string "(+ A 3)" {:namespaces {'user {'A 7}}})

(defn state->elk [steps results]
  {:id "root"
   :layoutOptions {:elk.algorithm "mrtree"}
   :children (map (fn [step]
                    {:id (:label step)
                     :width 200
                     :height 60})
                  steps)
   :edges (->> (analyze steps)
               (mapcat (fn [[target-id source-ids]]
                        (map (fn [source-id]
                               {:id (str source-id "-" target-id)
                                :sources [source-id]
                                :targets [target-id]})
                             source-ids))))})

;; TODO all changes to steps should go through this fn
;; TODO should probably rename
(defn layout! [steps]
  (let [elk (js/ELK.)
        results (calculate-results steps)]
   (-> ^js/Object elk
        (.layout (clj->js (state->elk steps results)))
        (.then (fn [layout]
                 (swap! state assoc :elk layout :steps steps :results results)))
        (.catch js/console.error))))

(defn rename-step [steps old-label new-label]
  (if (label-exists? steps new-label)
   steps
   (map (fn [step]
         (-> step
             (update :code (fn [code] (string/replace code #"\$[A-Z]+"
                                                      (fn [label]
                                                        (if (= label old-label)
                                                         new-label
                                                         label)))))
             (update :label (fn [label] (if (= label old-label)
                                         new-label
                                         label)))))
        steps)))

(defn re-order! []
  (layout! (analyze-and-reorder (:steps @state))))

(defn rename-step! [old-label new-label]
  (when-let [new-label (normalize-label new-label)]
   (layout! (rename-step (:steps @state) old-label new-label))))

(defn generate-new-label [steps]
  (let [letters (mapv char (range (.charCodeAt "A") (.charCodeAt "Z")))
        new-label (apply str "$" (repeatedly 3 #(rand-nth letters)))]
    (if (label-exists? steps new-label)
     (recur steps)
     new-label)))

(defn insert-step-before! [label]
  (->> (:steps @state)
       ((fn [steps]
          (let [[before after] (split-with (fn [step] (not= (step :label) label)) steps)]
           (concat before
                   [{:label (generate-new-label steps)
                     :code "nil"}]
                   after))))
       (layout!)))

(defn remove-step! [label]
  (->> (:steps @state)
       (remove (fn [step] (= (step :label) label)))
       (layout!)))

(defn edit-step-code! [label code]
  (->> (:steps @state)
       (map (fn [step]
              (if (= (:label step)
                     label)
               (assoc step :code code)
               step)))
       (layout!)))

#_{"$A" #{}
   "$B" #{"$A"}
   "$C" #{"$B"}
   "$D" #{"$A"}
   "$E" #{"$C" "$D"}}

(defn node-view [props]
  #_(println props (js->clj (:data props)))
  [:<> {}
   [:> Handle {:type "target" :position "top"}]
   [:div {:style {:border "1px solid black"
                  :background "white"}}
     [:div (:id props)]
     [:input {:value (get (js->clj (:data props)) "label")
              :on-change (fn [e]
                           (edit-step-code! (:id props) (.. e -target -value)))}]
     [:div
      (let [result (get (js->clj (:data props)) "result")]
       (cond
        (= (type result) ExceptionInfo)
        (.-message result)
        (= result ::NO-RESULT)
        ""
        :else
        (pr-str result)))]]
   [:> Handle {:type "source" :position "bottom" :id "a"}]
   [:> Handle {:type "source" :position "bottom" :id "b"}]])

(defn state->react-flow
  [{:keys [steps elk results]}]
  (let [elk-layout (->> (get (js->clj elk) "children")
                        (map (fn [{:strs [id x y]}]
                              [id {:x x :y y}]))
                        (into {}))]
   (concat
     ;; nodes
     (map (fn [step]
            {:id (:label step)
             :type "node"
             :data {:label (:code step)
                    :result (get results (symbol (:label step)) ::NO-RESULT)}
             :position (get elk-layout (:label step))})
          steps)
     ;; wires
     (->> (analyze steps)
          (mapcat (fn [[target-id source-ids]]
                   (map (fn [source-id]
                          {:id (str source-id "-" target-id)
                           :source source-id
                           ;; :sourceHandle
                           :target target-id})
                        source-ids)))))))
;; OUT OF DATE
#_(state->react-flow {:steps [{:label "$A"
                               :code "[10 15 26]"}
                              {:label "$B"
                               :code "(map inc $A)"}
                              {:label "$C"
                               :code "(reduce + $B)"}
                              {:label "$D"
                               :code "(count $A)"}
                              {:label "$E"
                               :code "(/ $C $D)"}]})

(defn graph-view []
  [:div {:style {:height 400 :border "solid 1px #DDDDDD"}}
   [:> ReactFlow {:elements (state->react-flow @state)
                  :nodeTypes #js {:node (r/reactify-component node-view)}}
    [:> flow/Background]]])

(defn classic-view []
 [:table
  [:tbody
   (doall
    (for [{:keys [label code]} (:steps @state)
          :let [result (get (:results @state) (symbol label) ::NO-RESULT)]]
      ^{:key label}
      [:<>
       [:tr
        [:td
         [:button {:on-click (fn [_] (insert-step-before! label))} "+"]]]
       [:tr.step
        [:td [:button {:on-click (fn [] (rename-step! label (js/prompt "What to rename?")))} label]]
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
         [:button {:on-click (fn [_] (remove-step! label))} "x"]]]]))
   [:tr
    [:td
     [:button {:on-click (fn [_] (insert-step-before! nil))} "+"]]]]])

(defn app-view []
  [:div
   (when (:elk @state)
    [graph-view])
   [:button {:on-click (fn [] (re-order!))} "Re-order"]
   [:button {:on-click (fn [] (layout! (:steps @state)))} "Layout"]
   [classic-view]])

;; change :label and :code to :step/label and :step/code
;; maybe explore using specter
;; code autoformatting
;; uploading a csv

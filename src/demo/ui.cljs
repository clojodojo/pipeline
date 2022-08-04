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

(defn generate-id []
  ;; elk can't handle object uuids
  (str (random-uuid)))

(defonce state
  (r/atom
    {:steps [{:id (generate-id)
              :label "$A"
              :code "[10 15 26]"}
             {:id (generate-id)
              :label "$B"
              :code "(map inc $A)"}
             {:id (generate-id)
              :label "$C"
              :code "(reduce + $B)"}
             {:id (generate-id)
              :label "$D"
              :code "(count $A)"}
             {:id (generate-id)
              :label "$E"
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
  (let [labels (set (map :label steps))
        label->id (zipmap (map :label steps)
                          (map :id steps))]
   (->> steps
        (map (fn [step]
               [(:id step)
                (->> (set (re-seq #"\$[A-Z]+" (:code step)))
                     ;; remove edges that don't exist
                     ;; b/c ELK explodes downstream if there
                     ;; are references to non-existing nodes
                     (set/intersection labels)
                     (map label->id)
                     set)]))
        (into {}))))

#_(toposort/toposort (analyze [{:id (generate-id)
                                :label "$A"
                                :code "[10 15 26]"}
                               {:id (generate-id)
                                :label "$B"
                                :code "(map inc $A)"}
                               {:id (generate-id)
                                :label "$C"
                                :code "(reduce + $B)"}
                               {:id (generate-id)
                                :label "$E"
                                :code "(/ $C $D)"}
                               {:id (generate-id)
                                :label "$D"
                                :code "(count $A)"}]))

(defn analyze-and-reorder [steps]
  (let [step-ids-in-order (toposort/toposort (analyze steps))
        id->step (zipmap (map :id steps)
                         steps)]
    (map id->step step-ids-in-order)))

#_(analyze-and-reorder [{:id (generate-id)
                         :label "$A"
                         :code "[10 15 26]"}
                        {:id (generate-id)
                         :label "$B"
                         :code "(map inc $A)"}
                        {:id (generate-id)
                         :label "$C"
                         :code "(reduce + $B)"}
                        {:id (generate-id)
                         :label "$E"
                         :code "(/ $C $D)"}
                        {:id (generate-id)
                         :label "$D"
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
       (let [label->id (zipmap (map :label steps)
                               (map :id steps))]
        (->> context
             ;; keys are the step labels (as symbols)
             (map (fn [[k result]]
                    [(label->id (name k)) result]))
             (into {}))))))
   (catch js/Error e
     {})))

#_(let [steps [{:id (generate-id)
                :label "A"
                :code "[10 15 26]"}
               {:id (generate-id)
                :label "B"
                :code "(map inc A)"}
               {:id (generate-id)
                :label "C"
                :code "(reduce + B)"}
               {:id (generate-id)
                :label "D"
                :code "(count A)"}
               {:id (generate-id)
                :label "E"
                :code "(/ C D)"}]]
    (calculate-results steps))

#_(sci/eval-string "(+ A 3)" {:namespaces {'user {'A 7}}})

(defn state->elk [steps results]
  {:id "root"
   :layoutOptions {:elk.algorithm "mrtree"}
   :children (map (fn [step]
                    {:id (:id step)
                     :width 200
                     :height 85})
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

(defn insert-step-before! [id]
  (->> (:steps @state)
       ((fn [steps]
          (let [[before after] (split-with (fn [step] (not= (step :id) id)) steps)]
           (concat before
                   [{:id (generate-id)
                     :label (generate-new-label steps)
                     :code "nil"}]
                   after))))
       (layout!)))

(defn remove-step! [id]
  (->> (:steps @state)
       (remove (fn [step] (= (step :id) id)))
       (layout!)))

(defn edit-step-code! [id code]
  (->> (:steps @state)
       (map (fn [step]
              (if (= (:id step)
                     id)
               (assoc step :code code)
               step)))
       (layout!)))

#_{"$A" #{}
   "$B" #{"$A"}
   "$C" #{"$B"}
   "$D" #{"$A"}
   "$E" #{"$C" "$D"}}

(defn node-view [props]
  (let [{:strs [id label result code]} (js->clj (:data props))]
   [:<> {}
    [:> Handle {:type "target" :position "top"}]
    [:div {:style {:border "1px solid black"
                   :background "white"}}
      [:input {:value label
               :style {:display "block"
                       :font-family "monospace"
                       :font-size "0.75rem"
                       :padding "0.5em"
                       :border "none"}
               :on-change (fn [e]
                            (rename-step! label
                              (.. e -target -value)))}]
      [:input {:value code
               :class "nodrag"
               :style {:background "black"
                       :display "block"
                       :color "white"
                       :font-family "monospace"
                       :font-size "0.75rem"
                       :border "none"
                       :padding "0.5em"}
               :on-change (fn [e]
                            (edit-step-code! id (.. e -target -value)))}]
      [:div.output
       {:class "nodrag"
        :style {:padding "0.5em"
                :font-family "monospace"
                :font-size "0.75rem"}}
       (cond
        (= (type result) ExceptionInfo)
        (.-message result)
        (= result ::NO-RESULT)
        ""
        :else
        (pr-str result))]]
    [:> Handle {:type "source" :position "bottom" :id "a"}]
    [:> Handle {:type "source" :position "bottom" :id "b"}]]))

(defn state->react-flow
  [{:keys [steps elk results]}]
  (let [elk-layout (->> (get (js->clj elk) "children")
                        (map (fn [{:strs [id x y]}]
                              [id {:x x :y y}]))
                        (into {}))]
   (concat
     ;; nodes
     (map (fn [step]
            {:id (:id step)
             :type "node"
             :data {:id (:id step)
                    :label (:label step)
                    :code (:code step)
                    :result (get results (:id step) ::NO-RESULT)}
             :position (get elk-layout (:id step))})
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
#_(state->react-flow {:steps [{:id (generate-id)
                               :label "$A"
                               :code "[10 15 26]"}
                              {:id (generate-id)
                               :label "$B"
                               :code "(map inc $A)"}
                              {:id (generate-id)
                               :label "$C"
                               :code "(reduce + $B)"}
                              {:id (generate-id)
                               :label "$E"
                               :code "(/ $C $D)"}
                              {:id (generate-id)
                               :label "$D"
                               :code "(count $A)"}]})

(defn graph-view []
  [:div {:style {:height 500 :border "solid 1px #DDDDDD"}}
   [:> ReactFlow {:elements (state->react-flow @state)
                  :nodeTypes #js {:node (r/reactify-component node-view)}}
    [:> flow/Background]]])

(defn classic-view []
 [:table
  [:tbody
   (doall
    (for [{:keys [id label code]} (:steps @state)
          :let [result (get (:results @state) id ::NO-RESULT)]]
      ^{:key id}
      [:<>
       [:tr
        [:td
         [:button {:on-click (fn [_] (insert-step-before! id))} "+"]]]
       [:tr.step
        [:td [:button {:on-click (fn [] (rename-step! label (js/prompt "What to rename?")))} label]]
        [:td
         [:textarea {:value code
                     :on-change (fn [e]
                                  (edit-step-code! id (.. e -target -value)))}]]
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
         [:button {:on-click (fn [_] (remove-step! id))} "x"]]]]))
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

(ns demo.toposort)

(defn remove-from-graph [graph cell]
  (->> (dissoc graph cell)
       (map (fn [[k v]] [k (disj v cell)]))
       (into {})))

#_(remove-from-graph {:D #{:C}
                      :C #{:A :B}
                      :A #{}
                      :B #{}} :A)

(defn cell-with-no-inputs [graph]
  (some (fn [[k v]] (when (empty? v) k)) graph))

#_(cell-with-no-inputs {:D #{:C}
                        :C #{:A :B}
                        :A #{}
                        :B #{}})

(defn toposort
  "implements kahn's algorithm for topological sorting of a directed acyclic graph

  find a cell that has no inputs
    store it; remove it from the graph
    repeat until graph is empty
  if at any point, can't find a cell with no inputs
    but still have cells in graph
    then we have a cycle! (abort)"
  [graph]
  (loop [list []
         graph graph]
    (if (empty? graph)
      list
      (if-let [cell (cell-with-no-inputs graph)]
       (recur (conj list cell)
              (remove-from-graph graph cell))
       (throw (ex-info "Cycle Detected" {}))))))


#_(toposort {:D #{:C}
             :C #{:A :B}
             :A #{}
             :B #{}})

#_[:A :B :C :D]
#_[:B :A :C :D]

;; cycle
#_(toposort {:D #{:C}
             :C #{:A :B}
             :A #{:C}
             :B #{}})
                      
; {:D #{:C}
;  :C #{:A :B}
;  :A #{}
;  :B #{}}
;
; [:A]
;
; {:D #{:C}
;  :C #{:B}
;  :B #{}}
;
; [:A :B]
;
; {:D #{:C}
;  :C #{}}
;
; [:A :B :C]
;
; {:D #{}}
;
; [:A :B :C :D]

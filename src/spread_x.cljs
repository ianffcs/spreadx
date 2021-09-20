(ns spread-x
  (:require
    [reagent.core :as r]
    [reagent.dom :as dom]
    [sci.core :refer [init]]
    [eval-spread :as evaluator]))

(def spread-x-start
  "Initial state"
  {:focused-cell nil
   :cells        {}
   :sci-ctx      (init {})
   :columns      10
   :rows         51})

(defn az-range
  "Columns from A-Z (or less)"
  [columns]
  (map char
       (take columns
             (if (<= columns (- 91 65))
               (range 65 91)))))

(defn table-lines
  "Rows from 0-100 (or less)"
  [rows]
  (take rows (range 0 100)))

(defn possible-cells
  [{:keys [rows columns]}]
  (set (for [s (az-range columns)
             n (table-lines rows)]
         (keyword (str s n)))))

;; UI impl

(def overflow-style
  {:overflow "auto"})

(defn light-border-style
  [width]
  {:border  "1px solid #ccc"
   :width   width
   :padding "0.5em"})

(defn header-fn
  [width chars]
  ^{:key chars}
  [:td {:style (light-border-style width)} chars])

(defn focus-cell!
  "Change focus"
  [*state cell-id _]
  (swap! *state assoc :focused-cell cell-id))

(defn submit-cell!
  "Store cells input, and evaluate dependent cells"
  [*state cell-id event]
  (.preventDefault event)
  (swap! *state
         #(-> %
              (evaluator/eval-spread cell-id)
              (dissoc :focused-cell))))

(defn change-cell!
  "Update input on change"
  [*state cell-id event]
  (swap! *state assoc-in [:cells cell-id :input] (.. event -target -value)))

(defn coll-fn
  "UI representation of a cell"
  [{:keys [focused-cell cells]}
   {:keys [focus-cell! submit-cell! change-cell!]} cell-width l c]
  (let [cell-id (keyword (str c l))]
    ^{:key cell-id}
    [:td {:style           (light-border-style cell-width)
          :data-testid     cell-id
          :on-double-click (partial focus-cell! cell-id)}
     (if (= cell-id focused-cell)
       [:form {:style       {:border "1px solid #ccc"}
               :id          cell-id
               :data-testid (str "form-" (name cell-id))
               :on-submit   (partial submit-cell! cell-id)}
        [:input {:style         {:margin  "0px"
                                 :padding "0px"
                                 :width   "100%"}
                 :type          "text"
                 :data-testid   (str "input-" (name cell-id))
                 :auto-focus    true
                 :default-value (get-in cells [cell-id :input])
                 :on-change     (partial change-cell! cell-id)}]]
       (get-in cells [cell-id :output]))]))

#_:clj-kondo/ignore

(defn row-fn
  "UI representation of a row of cells"
  [cells actions-map cell-width l]
  ^{:key l}
  [:tr {:style {:display "table-row"}}
   (concat
     [^{:key l}
      [:td {:style (light-border-style 42)} l]
      (map (partial coll-fn cells actions-map cell-width l) (az-range (:columns cells)))])])

(defn change-width!
  "Set table width to the 90% of the window"
  [state]
  (swap! state assoc :window-width (* 0.9 (.-innerWidth js/window))))

(defn add-row!
  "Extend the table with one row"
  [*cells]
  (swap! *cells update :rows #(min (inc %) 100)))

(defn row-btn
  "Button for add row"
  [add-row!]
  [^{:key "btn-row"}
   [:tr
    [:td
     [:button.button.is-primary
      {:on-click add-row!}
      "Add row"]]]])

(defn add-col!
  "Extend table with one column"
  [*cells]
  (swap! *cells update :columns #(min (inc %) 26)))

(defn coll-btn
  "button for adding column"
  [add-col!]
  [^{:key "btn-col"}
   [:th
    [:button.button.is-primary
     {:on-click add-col!}
     "Add column"]]])

(defn table-head
  "Head of the table, with add column button"
  [cells
   cell-width
   add-col!]
  [:thead {:style       {:display "table-header-group"}
           :data-testid "thead"}
   [:tr {:style (light-border-style cell-width)}
    (concat
      [^{:key :n} [:th {:style (light-border-style cell-width)}]]
      (map (partial header-fn cell-width) (az-range (:columns cells)))
      (coll-btn add-col!))]])

(defn table-body
  [{:keys [rows] :as cells} cell-width actions-map add-row!]
  [:tbody {:style       {:display "table-row-group"}
           :data-testid "tbody"}
   (concat
     [^{:key :n} [:tr (merge (light-border-style cell-width) overflow-style)]]
     (map (partial row-fn cells actions-map cell-width)
          (table-lines rows))
     (row-btn add-row!))])

(defn spread-x-ui
  "Main UI of cells"
  ([]
   (r/with-let [*spread-x (r/atom spread-x-start)]
     [spread-x-ui *spread-x]))
  ([*cells]
   (.addEventListener js/window "resize" (partial change-width! *cells))
   (change-width! *cells)
   [:div.panel.is-primary
    {:style {:margin "auto"
             :width  "100%"}}
    [:div.panel-heading "Spreadsheets"]
    [:div {:style {:height   (* 0.5 (.-innerHeight js/window))
                   :overflow "scroll"}}
     [:table {:id          "table"
              :data-testid "table"
              :style       {:width        "100%"
                            :table-layout "fixed"
                            :max-width    "80vw"}}
      [table-head @*cells "10rem" (partial add-col! *cells)]
      [table-body @*cells "10rem" {:focus-cell!  (partial focus-cell! *cells)
                                   :submit-cell! (partial submit-cell! *cells)
                                   :change-cell! (partial change-cell! *cells)}
       (partial add-row! *cells)]]]]))

(defn run
  []
  (dom/render
    [spread-x-ui]
    (js/document.getElementById "app")))
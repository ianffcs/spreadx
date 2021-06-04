(ns eval-spread
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]
    [instaparse.core :refer [transform] :refer-macros [defparser]]
    [sci.core :refer [eval-form]]))

(def kw->op
  "Supported operations map."
  {:add      '+
   :subtract '-
   :div      '/
   :mul      '*
   :mod      'mod
   :sum      '+
   :prod     '*})

#_:clj-kondo/ignore
;; defining a syntax to the parser
(defparser excel-like
           "
         formula = decimal / textual / eval
         eval    = (<'='> expr)
         expr    = range / cell / decimal / app
         app     = ident <'('> (expr <','>)* expr <')'>
         range   = cell <':'> cell
         cell    = #'[A-Za-z]\\d+'
         textual = #'[^=].*'
         ident   = #'[a-zA-Z_]\\w*'
         decimal = #'-?\\d+(\\.\\d*)?'
         ")

(defn exsplit
  "Splitting keyword."
  [k]
  (let [[_ k v] (re-matches #"([A-Z]+)([0-9]+)" (name k))]
    [k
     (edn/read-string v)]))

(defn range-cells-get
  "Get cells in a range."
  [[a b]]
  (let [[colla vala] (exsplit a)
        [collb valb] (exsplit b)
        [valmin valmax] (sort [(int vala) (int valb)])
        [collmin collmax] (sort [colla collb])]
    (for [collv (range (.charCodeAt collmin) (inc (.charCodeAt collmax)))
          v (range valmin (inc valmax))]
      (keyword (str (char collv) v)))))

(defn parse-input
  "Conditional to not try to parse empty or nil text."
  [input]
  (cond (nil? input) ""
        (str/blank? input) ""
        :else (excel-like input)))

#_:clj-kondo/ignore

(defn input->raw-ast
  "Transform the parsed input into raw-ast using instaparse transform."
  [input]
  (let [[err parsed-input] (try
                             [false (parse-input input)]
                             (catch :default ex
                               [ex]))]
    (if-not err
      (transform
        {:decimal edn/read-string
         :ident   identity
         :textual identity
         :cell    keyword
         :range   (fn [& args]
                    (range-cells-get args))
         :app     (fn [id & args]
                    (concat [(get kw->op (keyword id))] (if (and (seq? (first args)))
                                                          (let [res (flatten args)]
                                                            (if-not (symbol? (first res))
                                                              res
                                                              args))
                                                          args)))
         :expr    (fn [& args] (first args))
         :eval    identity
         :formula identity} parsed-input)
      err)))

(defn raw-ast->dependencies
  "Pick a raw-ast and make depencies vector from it."
  [raw-ast]
  (cond
    (coll? raw-ast) (mapcat raw-ast->dependencies raw-ast)
    (keyword? raw-ast) [raw-ast]
    :else nil))

(defn eval-sheets-raw-ast
  "Transform the sheets env to have raw-ast to every cell."
  [{:keys [cells]
    :as   env}]
  (reduce (fn [env cell-id]
            (if-let [input (get-in cells [cell-id :input])]
              (let [raw-ast (input->raw-ast input)
                    deps (raw-ast->dependencies raw-ast)
                    env-new (-> (assoc-in env
                                          [:cells cell-id :raw-ast]
                                          raw-ast)
                                (assoc-in [:cells cell-id :dependencies]
                                          deps))]
                env-new)
              env))
          env
          (keys cells)))

(defn dep-builder
  "Build dependency tree for evaluation of a cell only entry way."
  ([env key]
   (dep-builder env key #{}))
  ([{:keys [cells] :as env} key path]
   (let [dependencies (cond (contains? path key) (-> (str "duplicated keys: " key)
                                                     (ex-info {:cognitect.anomalies/category :cognitect.anomalies/conflict
                                                               :dupe                         key
                                                               :seen                         path})
                                                     (throw))
                            (nil? (get-in cells [key :dependencies])) (conj path key)
                            :else (let [result (for [d (get-in cells [key :dependencies])]
                                                 (dep-builder env d (conj path key)))]
                                    (mapcat identity result)))]
     (concat (distinct (vec dependencies))))))

(defn dependency-buildn2
  "Build entry and out dependencies for evaluation of a cell."
  [{:keys [cells]
    :as   env} key]
  (let [reverse-dependent (keep (fn [[k v]]
                                  (when (some #(= key %) (:dependencies v))
                                    k)) cells)]
    (concat (remove #(= key %)
                    (dep-builder env key #{})) [key] reverse-dependent)))

(defn add-eval-tree
  "Add the evaluation tree onto cells env."
  [env init-key]
  (let [[err result] (try
                       [false (dependency-buildn2 env init-key)]
                       (catch :default ex
                         [ex]))]
    (if-not err
      (merge env {:eval-tree result})
      (assoc-in env [:cells init-key :output] (ex-message err)))))

(defn get-data-rec
  "Recursive get-data from a cell that is evaluating."
  [cells raw-ast]
  (letfn [(get-data-rec*
            [cells v]
            (cond (keyword? v) (-> cells
                                   (get v)
                                   :output)
                  (seq? v) (map #(get-data-rec* cells %) v)
                  :else v))]
    (map #(get-data-rec* cells %) raw-ast)))

(defn ast-element-evaluator
  "Eval of every element of a raw-ast, making a ast and evaluting the ast onto sci."
  [sci-ctx env raw-ast cells cell-id]
  (cond (keyword? raw-ast) (let [{:keys [output]} (get cells raw-ast)
                                 env-after (assoc-in env [:cells cell-id :ast] output)
                                 current-output (eval-form sci-ctx output)]
                             (assoc-in env-after [:cells cell-id :output]
                                       current-output))
        (seq? raw-ast) (let [form (get-data-rec cells raw-ast)
                             output (eval-form sci-ctx form)]
                         (assoc-in env [:cells cell-id :output] output))
        :else (let [output (eval-form sci-ctx raw-ast)]
                (update-in env [:cells cell-id] assoc
                           :ast raw-ast
                           :output output))))

(defn eval-spread
  [env cell-id]
  (let [{:keys [sci-ctx eval-tree]
         :as   env-new} (-> env
                            eval-sheets-raw-ast
                            (add-eval-tree cell-id))
        rf (fn [{:keys [cells]
                 :as   env} cell-id]
             (let [{:keys [raw-ast]} (get cells cell-id)]
               (ast-element-evaluator sci-ctx env raw-ast cells cell-id)))]
    (reduce rf env-new eval-tree)))

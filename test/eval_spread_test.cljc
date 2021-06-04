(ns eval-spread-test
  (:require
    [cljs.test :as t
     :include-macros true
     :refer [are is]]
    [eval-spread :refer [range-cells-get
                         input->raw-ast
                         eval-sheets-raw-ast
                         dependency-buildn2
                         add-eval-tree
                         eval-spread
                         get-data-rec]]
    [nubank.workspaces.core :as ws]
    [sci.core :refer [init]]))

(ws/deftest range-cells-get-test
  (are [expected actual] (= expected (range-cells-get actual))
                         '(:A0 :A1) [:A0 :A1]
                         '(:A0 :A1 :A2) [:A0 :A2]
                         '(:A0 :A1 :B0 :B1) [:B0 :A1]
                         '(:B0 :B1 :B2 :B3 :B4 :B5 :B6 :B7 :B8 :B9 :B10) [:B0 :B10]))

(ws/deftest parse-input->raw-ast-test
  (are [expected actual] (= expected (input->raw-ast actual))
                         1 "1"
                         :A1 "=A1"
                         "abc" "abc"
                         '(+ 1 2) "=add(1,2)"
                         '(+ :A1 :A2) "=add(A1,A2)"
                         '(+ :A3 (* 2 :A2)) "=add(A3,mul(2,A2))"
                         '(+ :A0 :A1 :A2 :A3) "=add(A0:A3)"
                         '(* :B0 :B1 :B2 :B3 :B4 :B5 :B6 :B7 :B8 :B9 :B10) "=mul(B0:B10)"
                         '(* :B0 :B1 2) "=mul(B0:B1,2)"
                         '(+ :B0 :B1) "=add(B0:B1)"))

(ws/deftest eval-sheets-raw-ast-test
  (let [env {:sci-ctx (init {})
             :cells   {:A0 {:input        "=add(B0:B1)",
                            :raw-ast      '(+ :B0 :B1),
                            :ast          '(+ nil nil),
                            :output       0,
                            :dependencies '(:B0 :B1)}
                       :B0 {:input "1"}
                       :B2 {:input "=add(B0:B1)"}}}]
    (is (= {:A0 {:input        "=add(B0:B1)",
                 :raw-ast      '(+ :B0 :B1),
                 :ast          '(+ nil nil),
                 :output       0,
                 :dependencies '(:B0 :B1)}
            :B0 {:input "1" :raw-ast 1 :dependencies nil}
            :B2 {:input        "=add(B0:B1)"
                 :raw-ast      '(+ :B0 :B1)
                 :dependencies '(:B0 :B1)}}
           (:cells (eval-sheets-raw-ast env))))))

(ws/deftest fix-loop-deps
  (let [looping-deps0 {:cells {:A0 {:dependencies [:A0]}}}
        looping-deps1 {:cells {:A0 {:dependencies [:A1]}
                               :A1 {:dependencies [:A2 :A0]}
                               :A2 {:dependencies []}}}]
    (is (= "duplicated keys: :A0" (ex-message
                                    (try (dependency-buildn2 looping-deps0 :A0)
                                         (catch :default ex
                                           ex)))))
    (is (= "duplicated keys: :A0" (ex-message
                                    (try (dependency-buildn2 looping-deps1 :A0)
                                         (catch :default ex
                                           ex)))))))

(ws/deftest not-looping-dependency
  (let [env {:cells {:A0 {:input "1", :raw-ast 1}
                     :A1 {:input "=A0", :raw-ast :A0, :dependencies '[:A0]}
                     :A2 {:input "=A1", :raw-ast :A1, :dependencies '[:A1]}
                     :B0 {:input        "=sum(A0:A9)",
                          :raw-ast      '(+ :A0 :A1 :A2),
                          :dependencies '(:A0 :A1 :A2)}}}
        s1 {:cells {:A0 {}
                    :B0 {:dependencies [:A0]}}}
        s2 {:cells {:A0 {}
                    :A1 {}
                    :B0 {:dependencies [:A0 :A1]}}}
        s3 {:cells {:A0 {}
                    :A1 {:dependencies [:A0]}
                    :B0 {:dependencies [:A0 :A1]}}}]
    (is (= '(:A0 :A1 :A2 :B0) (dependency-buildn2 env :B0)))
    (is (= '(:A0 :B0) (dependency-buildn2 s1 :B0)))
    (is (= '(:A0 :A1 :B0) (dependency-buildn2 s2 :B0)))
    (is (= '(:A0 :A1 :B0) (dependency-buildn2 s3 :B0)))))

(ws/deftest dependencies-builder-from-sheets
  (let [env {:sci-ctx (init {})
             :cells   {:A0 {:input        "=add(B0,B1)",
                            :raw-ast      '(+ :B0 :B1),
                            :ast          '(+ nil nil),
                            :output       0,
                            :dependencies '(:B0 :B1)}
                       :B0 {:input "1"}
                       :B2 {:input "=add(B0,B2)"}}}
        env1 {:cells {:A1 {:input "=add(A3,mul(2,A2))"}
                      :A3 {:input "5"}
                      :A2 {:input "6"}}}
        env-reverse {:cells {:A0 {:input        "=mul(B0,B1)",
                                  :raw-ast      '(* :B0 :B1),
                                  :dependencies (:B0 :B1)},
                             :B0 {:input "8", :raw-ast 5},
                             :B1 {:input "10", :raw-ast 10}}}]
    (is (= "duplicated keys: :B2"
           (ex-message (try (dependency-buildn2 (eval-sheets-raw-ast env) :B2)
                            (catch :default ex
                              ex)))))
    (is (= '(:A3 :A2 :A1) (dependency-buildn2 (eval-sheets-raw-ast env1) :A1)))
    (is (= '(:A3 :A1) (dependency-buildn2 (eval-sheets-raw-ast env1) :A3)))
    (is (= '(:B0 :A0) (dependency-buildn2 (eval-sheets-raw-ast env-reverse) :B0)))))

(ws/deftest add-eval-tree-test
  (let [env {:sci-ctx (init {})
             :cells   {:A0 {:input "=add(B0,B1)"}
                       :B0 {:input "1"}
                       :B2 {:input "=add(B0,3)"}}}
        {:keys [eval-tree cells]} (add-eval-tree (eval-sheets-raw-ast env) :B2)]
    (is (= [:B0 :B2] eval-tree))
    (is (= {:A0 {:input        "=add(B0,B1)",
                 :raw-ast      '(+ :B0 :B1),
                 :dependencies '(:B0 :B1)},
            :B0 {:input "1", :raw-ast 1 :dependencies nil},
            :B2 {:input "=add(B0,3)", :raw-ast '(+ :B0 3), :dependencies '(:B0)}} cells))))

(defn assert-cell
  [env cell]
  (get-in (eval-spread env cell) [:cells cell :output]))

(defn table
  [cells]
  {:sci-ctx (init {})
   :cells   (into {} (for [[k v] cells] [k {:input v}]))})

(ws/deftest eval-spread-test
  (let [env-simple-subs {:sci-ctx (init {})
                         :cells   {:A0 {:input "=B0"}
                                   :B0 {:input "1"}}}
        env-simple-op {:sci-ctx (init {})
                       :cells   {:A0 {:input "=add(B0,B1)"}
                                 :B0 {:input "1"}
                                 :B1 {:input "10"}}}
        env-mul {:sci-ctx (init {})
                 :cells   {:A0 {:input "=mul(B0,B1)"}
                           :B0 {:input "5"}
                           :B1 {:input "10"}}}
        env-ranged-op {:sci-ctx (init {})
                       :cells   {:A0 {:input        "=mul(B0:B1)"
                                      :raw-ast      '(* :B0 :B1)
                                      :dependencies (:B0 :B1)}
                                 :B0 {:input "8"}
                                 :B1 {:input "10"}}}]
    (are [expected actual] (= expected actual)
                           1 (assert-cell env-simple-subs :A0)
                           11 (assert-cell env-simple-op :A0)
                           50 (assert-cell env-mul :A0)
                           80 (assert-cell env-ranged-op :A0)
                           9 (assert-cell (table {:A0 "=sum(A1:A9)"
                                                  :A1 "1"
                                                  :A2 "1"
                                                  :A3 "1"
                                                  :A4 "1"
                                                  :A5 "1"
                                                  :A6 "1"
                                                  :A7 "1"
                                                  :A8 "1"
                                                  :A9 "1"}) :A0)
                           "duplicated keys: :A0" (assert-cell (table {:A0 "=A0"}) :A0)
                           "" (assert-cell (table {:A1 ""}) :A1)
                           1 (assert-cell (table {:A1 "1"}) :A1)
                           "A" (assert-cell (table {:A1 "A"}) :A1)
                           "" (assert-cell (table {:A1 ""
                                                   :A2 "=A1"}) :A1)
                           "duplicated keys: :A1" (assert-cell (table {:A1 "=A1"}) :A1)
                           11 (assert-cell (table {:A1 "=add(A2:A5)"
                                                   :A3 "5"
                                                   :A2 "6"}) :A1)
                           17 (assert-cell (table {:A1 "=add(A3,mul(2,A2))"
                                                   :A3 "5"
                                                   :A2 "6"}) :A1)
                           21 (assert-cell (table {:A1 "=add(A3,mul(2,add(A2,2)))"
                                                   :A3 "5"
                                                   :A2 "6"}) :A1)
                           5 (assert-cell (table {:A1 "=add(A2,3)"
                                                  :A2 "2"}) :A1)
                           (* 2 5 (+ 2 5) (+ 5 6)) (assert-cell (table {:A1 "=mul(2,A3,add(A0,A3),add(A3,A2))"
                                                                        :A3 "5"
                                                                        :A2 "6"
                                                                        :A0 "2"}) :A1)
                           3 (assert-cell (table {:A0 "1"
                                                  :A1 "=A0"
                                                  :A2 "=A1"
                                                  :B0 "=sum(A0:A2)"}) :B0)
                           16 (assert-cell (table {:A1 "=add(add(A0,A3),add(A3,A2))"
                                                   :A3 "5"
                                                   :A2 "6"}) :A1)
                           (* 2 5 (+ (* 2 2) 5) (+ 5 6)) (assert-cell (table {:A1 "=mul(2,A3,add(mul(A0,2),A3),add(A3,A2))"
                                                                              :A3 "5"
                                                                              :A2 "6"
                                                                              :A0 "2"}) :A1)
                           (* (+ (* (+ 2 6) 2) 5) 2) (assert-cell (table {:A1 "=mul(add(mul(add(A0,A2),2),A3),2)"
                                                                          :A3 "5"
                                                                          :A2 "6"
                                                                          :A0 "2"}) :A1))))

(ws/deftest get-data-rec-test
  (let [cells {:A1 {:input        "=add(A3,mul(2,A2))",
                    :raw-ast      '(+ :A3 (* 2 :A2)),
                    :dependencies (:A3 :A2)},
               :A3 {:input "5", :raw-ast 5, :ast 5, :output 5},
               :A2 {:input "6", :raw-ast 6, :ast 6, :output 6}}
        raw-ast '(+ :A3 (* 2 :A2))]
    (is (= '(+ 5 (* 2 6)) (get-data-rec cells raw-ast)))))

(ws/deftest nested-exp-eval-spread-test
  (let [env-composed {:sci-ctx (init {})
                      :cells   {:A1 {:input "=add(A3,mul(2,A2))"}
                                :A3 {:input "5"}
                                :A2 {:input "6"}}}]
    (is (= 17 (get-in (eval-spread env-composed :A1) [:cells :A1 :output])))))

(ws/deftest update-updates-deep-dependent-cells
  (let [env {:sci-ctx (init {})
             :cells   {:B1 {:input "3"}
                       :B2 {:input "=B1"}
                       :B3 {:input "=add(B1,B2)"}}}]
    (is (= 6 (get-in (eval-spread env :B3) [:cells :B3 :output])))))

(ws/deftest update-after-circular-dependency
  (let [env {:sci-ctx (init {})
             :cells   {:C1 {:input        "100",
                            :raw-ast      :C1,
                            :dependencies [:C1],
                            :output       "duplicated keys: :C1"},
                       :C2 {:input "=C1", :raw-ast :C1, :dependencies [:C1]}}}]
    (is (= {:C1 {:input        "100",
                 :raw-ast      100,
                 :dependencies nil,
                 :output       100,
                 :ast          100},
            :C2 {:input        "=C1",
                 :raw-ast      :C1,
                 :dependencies [:C1],
                 :ast          100,
                 :output       100}} (-> env (eval-spread :C1) :cells)))))

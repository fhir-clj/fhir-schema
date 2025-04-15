(ns fhir.schema-test
  (:require [clojure.test :as t]
            [matcho.core :as matcho]
            [fhir.schema :as subj]))

(def ctx {:schemas {"HumanName" {:elements {:family {:type "string"}
                                            :given  {:array true :type "string"}
                                            :use    {:type "Coding"}}}
                    "Extension" {:elements {:id {:type "code"}
                                            :url {:type "url"}
                                            :valueString {:type "string"}}}
                    "Patient" {:base "Resource"
                               :elements {:name {:array true :type "HumanName"}}}
                    "Coding" {:elements {:code {:type "string"}
                                         :system {:type "string"}}}
                    "Resource" {:elements {:resourceType {:type "code"}
                                           :id {:type "string"}}}
                    "code"   {:kind "primitive-type" :type "string"}
                    "url"    {:kind "primitive-type" :type "string"}
                    "string" {:kind "primitive-type" :type "string"}}})

(defmacro match-errors [data errors-pat]
  `(let [res# (subj/validate ctx [] ~data)]
     (matcho/match (:errors res#) ~errors-pat)
     res#))

(defmacro match-schema [schema data errors-pat]
  `(let [res# (subj/validate-schemas ctx #{~schema} ~data)]
     (matcho/match (:errors res#) ~errors-pat)
     res#))

(t/deftest test-schema

  (match-schema {:type "string"} "string" empty?)

  (match-schema {:type "string"} 1 [{:type :type :path []}])
  (match-schema {:type "string"} true [{:type :type :path []}])

  (match-schema {:elements {:name {:type "string"}}} {:name "ok"} empty?)

  (match-schema {:elements {:name {:type "string"}}}
                {:unknown "ups"}
                [{:type :element/unknown, :path [:unknown]}])

  (match-schema {:elements {:name {:elements {:family {:type "string"}}}}}
                {:name {:family "family"}}
                empty?)

  (match-schema {:elements {:name {:elements {:family {:type "string"}}}}}
                {:name {:unknown "ups"}}
                [{:type :element/unknown, :path [:name :unknown]}])

  (match-schema {:elements {:name {:array true
                                   :elements {:family {:type "string"}}}}}
                {:name [{:family "family"}]}
                empty?)

  (match-schema {:elements {:name {:array true
                                   :elements {:family {:type "string"}}}}}
                {:name {:family "family"}}
                [{:type :type/array, :message "Expected array", :path [:name], :value {:family "family"}, :schema-path [:name :array]}])

  (match-schema {:elements {:name {:array true :elements {:family {:type "string"}}}}}
                {:name [{:family "family" :ups "x"}]}
                [{:type :element/unknown, :path [:name 0 :ups]}])

  (match-schema {:elements {:name {:array true
                                   :elements {:family {:type "string"}
                                              :type {:elements {:code {:type "string"}}}}}}}
                {:name [{:family "family" :type {:code "ok"}}]}
                empty?)

  (match-schema {:elements {:name {:array true
                                   :elements {:family {:type "string"}
                                              :type {:elements {:code {:type "string"}}}}}}}
                {:name [{:family "family" :type {:code 1}}]}
                [{:type :type,
                  :message "Expected type string",
                  :value 1,
                  :schema-path [:name :type :code :type "string" :type],
                  :path [:name 0 :type :code]}])

  (match-schema {:elements {:name {:array true :type "HumanName"}}}
                {:name [{:family "f" :given ["g1" "g2"]}]}
                empty?)

  (match-schema {:elements {:name {:array true :type "HumanName"}}}
                {:name [{:family 1}]}
                [{:type :type,
                  :message "Expected type string",
                  :value 1,
                  :schema-path [:name :type "HumanName" :family :type "string" :type],
                  :path [:name 0 :family]}])

  (match-schema {:choices {:value ["valueString" "valueCode"]}
                 :elements {:label {:type "string"}
                            :valueString {:type "string" :choiceOf "value"}
                            :valueCode   {:type "code" :choiceOf "value"}}}
                {:valueString "a" :valueCode "c" :label "x"}
                [{:type :choices/multiple,
                  :path [:value],
                  :message "Only one choice element is allowd",
                  :value {:valueString "a", :valueCode "c"}}])

  (match-schema {:choices {:value ["valueString"]}
                 :elements {:label {:type "string"}
                            :valueString {:type "string" :choiceOf "value"}
                            :valueCode   {:type "code" :choiceOf "value"}}}
                {:valueCode "c" :label "x"}
                [{:type :choice/excluded,
                  :message "Choice element value is not allowed, only valueString",
                  :path [:value],
                  :schema-path [:choices]}])

  (match-schema {:required ["name"]
                 :elements {:name {:type "string"}}}
                {:name "john"}
                empty?)

  (match-schema {:required ["name"]
                 :elements {:name {:type "string"}}}
                {:_name {:extension [{:url "ext" :valueString "ok"}]}}
                empty?)

  (match-schema {:required ["name"]
                 :elements {:name {:type "string"}}}
                {}
                [{:type :require :path [:name]}])

  (match-schema {:elements {:name {:array true :type "string"  :min 1 :max 2}}}
                {:name []}
                [{:type :min
                  :message "expected min=1 got 0",
                  :value 0
                  :expected 1
                  :path [:name]}])

  (match-schema {:elements {:name {:array true :type "string"  :min 1 :max 2}}}
                {:name ["a" "b" "c"]}
                [{:type :max
                  :message "expected max=2 got 3"
                  :value 3
                  :expected 2
                  :path [:name]}])

  (match-schema {:elements {:name {:elements {:use {:type "string" :pattern {:string "home"}}}}}}
                {:name {:use "home"}}
                empty?)

  (match-schema {:elements {:name {:elements {:use {:type "string" :pattern {:string "home"}}}}}}
                {:name {:use "hotel"}}
                [{:type :pattern :expected "home"}])

  {:elements {:type "string"}}

  {:kind "primitive-type"
   :url "http://hl7.org/fhir/StructureDefinition/string" ;; <type-name>
   :typeName "string"
   :primitiveType "string"
   ;; hostType
   ;; primitiveType
   }

  {:kind "primitive-type"
   :url "http://hl7.org/fhir/StructureDefinition/code"
   :typeName "string"
   :primitiveType "string"
   ;; hostType
   ;; primitiveType
   }

  ;;  dates, niumbers etc
  ;; maxValue/minValue
  ;; TODO: take a look at IG tooling for magical extension

  ;;<- string types
  ;; What means type is type of string
  ;; maxLenght -> type <- string
  ;; regex

  ;;TODO fixed should be exact match
  ;;TODO: extensions -> nested resolution - Lloyd test for http(s) -> means extension
  ;;TODO: bundles -> {:type "Resource"}
  ;;TODO: slices
  ;;TODO: constraints
  )

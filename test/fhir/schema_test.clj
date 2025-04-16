(ns fhir.schema-test
  (:require [clojure.test :as t :refer [testing deftest]]
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
                    "boolean" {:kind "primitive-type" :type "boolean"}
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

(deftest test-schema

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
                [{:type :pattern
                  :expected "home"
                  :schema-path [:name :use :pattern]
                  :got "hotel"
                  :path [:name :use]}])

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

  (testing "Patient"
    (match-schema {:base "Resource"
                   :elements {:resourceType {:type "code"}
                              :name {:array true :type "HumanName"}
                              :active {:type "boolean"}
                              :extension {:array true :type "Extension"}}}
                  {:resourceType "Patient"
                   :name [{:family "Smith"
                           :given ["John" "Jacob"]
                           :use "official"}
                          {:family "Smith"
                           :given ["Johnny"]
                           :use "nickname"}]
                   :active true
                   :extension [{:url "http://example.org/fhir/StructureDefinition/preferred-contact-method"
                                :valueString "email"}]}
                  empty?))

  (testing "Nested extensions"
    (match-schema {:elements {:extension {:array true
                                          :type "Extension"
                                          :elements {:extension {:array true
                                                                 :type "Extension"}}}}}
                  {:extension [{:url "http://example.org/parent"
                                :extension [{:url "http://example.org/child"
                                             :valueString "nested value"}]}]}
                  empty?))

  ;;TODO fixed should be exact match
  )

(t/deftest primitive-types-extensions-quirks
  (match-schema {:required ["gender"]
                 :elements {:gender {:type "string"}}}
                {:gender "male"}
                empty?)

  (t/testing "omit actual gender value,
              provide primitive extension with data-absent reason,
              :required check should pass"
    (match-schema {:required ["gender"]
                   :elements {:gender {:type "string"}}}
                  {:_gender {:extension [{:url "data-absent-reason" :valueCode "asked-unknown"}]}}
                  empty?))

  (t/testing "null alignment and overall null usage in primitive extensions"
    (t/testing "done right"
      (match-schema {:elements {:code {:type "string"}}}
                    {:code ["au" "nz"]
                     :_code
                     [nil
                      {:extension
                       [{:url "http://hl7.org/fhir/StructureDefinition/display"
                         :valueString "New Zealand a.k.a Kiwiland"}]}]}
                    empty?))

    (t/testing "done right, one primitive sub part provided for empty value, and one as aligned extension for actual value"
      (match-schema {:elements {:code {:type "string"}}}
                    {:code [nil "nz"]
                     :_code
                     [{:extension [{:url "data-absent-reason" :valueCode "error"}]}
                      {:extension
                       [{:url "http://hl7.org/fhir/StructureDefinition/display"
                         :valueString "New Zealand a.k.a Kiwiland"}]}]}
                    empty?))

    (t/testing "done wrong, all primitive sub-parts are nulled"
      (match-schema {:elements {:code {:type "string"}}}
                    {:code ["au" "nz"]
                     :_code [nil nil]}
                    [{}]) ;;??
      )

    (t/testing "done terribly wrong"
      (match-schema {:elements {:code {:type "string"}}}
                    {:code [nil nil]
                     :_code [nil nil]}
                    [{} {}]) ;; two errors about actual null values? what about empty _code?
      )
    )


  )

(t/deftest primitive-types
  (match-schema {:elements {:gender {:type "string"}}}
                {:gender "male"}
                empty?)

  (t/testing "basic type mismatch"
    (match-schema {:elements {:gender {:type "string"}}}
                  {:gender 1}
                  [{:type :type
                    :message "Expected type string"
                    :value 1
                    :schema-path [:gender :type "string" :type]
                    :path [:gender]}]))

  (t/testing "array non array shape mismatch"
    (match-schema {:elements {:gender {:type "string"}}}
                 {:gender ["male"]}
                 [{:type :type/array
                   :message "Expected not array"
                   :path [:gender]
                   :value ["male"]}])

    (match-schema {:elements {:value {:type "string" :array true}}}
                  {:value "male"}
                  [{:type :type/array
                    :message "Expected array"
                    :path [:value]
                    :value "male"
                    :schema-path [:value :array]}]))

  (t/testing "passing object-shape instead of the primitive"
    (match-schema {:elements {:gender {:type "string"}}}
                 {:gender {:value "male"}}
                 [{:type :type
                   :message "Expected type string"
                   :value {:value "male"}
                   :schema-path [:gender :type "string" :type]
                   :path [:gender]}
                  {:type :element/unknown :path [:gender :value]}] ;; Should we really need this check?
                 )))

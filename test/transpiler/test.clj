(ns transpiler.test
  (:require [clojure.test :refer [deftest testing]]
            [transpiler.fhir-schema :refer [parse-path get-common-path calculate-actions translate]]
            [matcho.core :as matcho]))

(deftest test-algorythm
  (matcho/match
   (parse-path {:path "R.a"})
    [{:el :a}])

  (matcho/match
   (parse-path  {:path "R.a.b"})
    [{:el :a} {:el :b}])

  (matcho/match
   (calculate-actions (parse-path {:path "R.a"}) (parse-path {:path "R.b"}))
    [{:type :exit, :el :a} {:type :enter, :el :b}])

  (matcho/match
   (calculate-actions (parse-path {:path "R.a"}) (parse-path {:path "R.b.c"}))
    [{:type :exit, :el :a} {:type :enter, :el :b} {:type :enter, :el :c}])

  (calculate-actions (parse-path {:path "R.b.c"}) (parse-path {:path "R.x"}))

  (matcho/match
   (calculate-actions (parse-path {:path "R.a.b.c"}) [])
    [{:type :exit, :el :c} {:type :exit, :el :b} {:type :exit, :el :a}])

  (matcho/match
   (get-common-path
    (conj (parse-path  {:path "R.a.c" :sliceName "s1"}) {:el :b})
    (conj (parse-path  {:path "R.a.c" :sliceName "s2"})))
    [{:el :a} {:el :c}])

  (matcho/match
   (calculate-actions
    []
    (conj (parse-path {:path "R.a" :sliceName "s1"}) {:el :b}))
    [{:type :enter, :el :a}
     {:type :enter-slice, :sliceName "s1"}
     {:type :enter, :el :b}])

  (matcho/match
   (calculate-actions
    (conj (parse-path  {:path "R.a" :sliceName "s1"}) {:el :b})
    (conj (parse-path  {:path "R.a" :sliceName "s2"})))
    [{:type :exit, :el :b}
     {:type :exit-slice  :sliceName "s1"}
     {:type :enter-slice :sliceName "s2"}])

  (matcho/match
   (calculate-actions
    (conj (parse-path  {:path "R.a" :sliceName "s1"}) {:el :b})
    [])
    [{:type :exit, :el :b}
     {:type :exit-slice, :sliceName "s1"}
     {:type :exit, :el :a}])

  (calculate-actions
   [{:sliceName :s1, :el :x} {:el :b}]
   [{:sliceName :s1, :el :x} {:el :b, :sliceName :z1}])

  (def els
    [{:path "R"       :short :a}
     {:path "R.a"     :short :a}
     {:path "R.b"     :short :b}
     {:path "R.c"     :short :c}
     {:path "R.c.d"   :short :c.d}
     {:path "R.c.d.f" :short :c.d.f}
     {:path "R.c.d.i" :short :c.d.i}
     {:path "R.x"     :short :x}
     {:path "R.x" :slicing {:discriminator [{:short "pattern" :path "a"}]}}
     {:path "R.x" :sliceName "s1"}
     {:path "R.x.a"   :short :x.s1.a :patternString "s1"}
     {:path "R.x.b"   :short :x.s1.b}
     {:path "R.x" :sliceName "s2"}
     {:path "R.x.a"   :short :x.s2.a :patternString "s2"}
     {:path "R.x.b"   :short :x.s2.b}])

  ;; FOCUS
  (matcho/match
   (translate {:differential {:element els}})
    {:elements
     {:a {:short :a},
      :b {:short :b},
      :c {:short :c,
          :elements {:d {:short :c.d,
                         :elements {:f {:short :c.d.f}
                                    :i {:short :c.d.i}}}}},
      :x {:short :x,
          :slicing {:slices {:s1 {:match {:a "s1"}
                                  :schema {:elements {:a {:short :x.s1.a}
                                                      :b {:short :x.s1.b}}}}
                             :s2 {:match {:a "s2"}
                                  :schema {:elements {:a {:short :x.s2.a}
                                                      :b {:short :x.s2.b}}}}}}}}})

  (def els-sl
    [{:path "R.x"        :short :x  :slicing {:discriminator [{:type "pattern" :path "a"}]}}
     {:path "R.x"        :sliceName "s1"}
     {:path "R.x.a"      :short :x.s1.a :patternString "s1"}
     {:path "R.x.b"      :short :x.s1.b :slicing {:discriminator [{:type "pattern" :path "f.ff"}]}}
     {:path "R.x.b"      :sliceName "z1"}
     {:path "R.x.b.f"    :short :x.s1.b.z1.f}
     {:path "R.x.b.f.ff" :short :x.s1.b.z1.ff :patternCoding {:code "z1"}}
     {:path "R.x.b"      :sliceName "z2"}
     {:path "R.x.b.f"    :short :x.s1.b.z2.f}
     {:path "R.x.b.f.ff" :short :x.s1.b.z2.ff :patternCoding {:code "z2"}}
     {:path "R.x"        :sliceName "s2"}
     {:path "R.x.a"      :short :x.s2.a :patternString "s2"}
     {:path "R.x.b"      :short :x.s2.b}
     {:path "R.z"        :short :z}])

  (matcho/match
   (translate {:differential {:element [{:path "A.x" :min 1 :max "*"}]}})
    {:required #{"x"}
     :elements {:x {:array true, :min 1 :max nil?}}})

  (matcho/match
   (translate {:differential {:element [{:path "A.x" :min 1 :max "10"}]}})
    {:required #{"x"}
     :elements {:x {:array true, :_required nil? :min 1 :max 10}}})

  (matcho/match
   (translate {:differential {:element [{:path "A.x" :type [{:code "string"}]}]}})
    {:elements {:x {:type "string"}}})

  (matcho/match
   (translate {:differential {:element els-sl}})
    {:elements
     {:z {:short :z}
      :x {:short :x,
          :slicing
          {:discriminator [{:type "pattern" :path "a"}]
           :slices
           {:s1
            {:match {:a "s1"}
             :schema
             {:elements
              {:a {:short :x.s1.a},
               :b {:short :x.s1.b,
                   :slicing {:slices
                             {:z1 {:match {:f {:ff {:code "z1"}}}
                                   :schema {:elements {:f {:short :x.s1.b.z1.f, :elements {:ff {:short :x.s1.b.z1.ff}}}}}},
                              :z2 {:match {:f {:ff {:code "z2"}}}
                                   :schema {:elements {:f {:short :x.s1.b.z2.f, :elements {:ff {:short :x.s1.b.z2.ff}}}}}}}}}}}},
            :s2 {:match {:a "s2"}
                 :schema {:elements {:a {:short :x.s2.a}, :b {:short :x.s2.b}}}}}}}}})

  (def union-els
    [{:path "R.value[x]"  :type [{:code "string"} {:code "Quantity"}]}
     {:path "R.valueQuantity.unit" :short "unit"}])

  (matcho/match
   (translate {:differential {:element union-els}})
    {:elements
     {:value         {:choices ["valueString" "valueQuantity"]},
      :valueString   {:type "string" :choiceOf "value"},
      :valueQuantity {:type "Quantity" :choiceOf "value"
                      :elements {:unit {:short "unit"}}}}})

  ;; (clojure.pprint/pprint (translate els))

  (matcho/match
   (translate
    {:differential
     {:element
      [{:path "R.category", :slicing {:discriminator [{:type "pattern", :path "$this"}], :rules "open"}, :min 1, :max "10", :mustSupport true}
       {:path "R.category", :sliceName "LaboratorySlice", :min 2, :max "3",
        :patternCodeableConcept {:coding [{:system "CodeSystem", :code "LAB"}]}, :mustSupport true}
       {:path "R.category", :sliceName "Radiologylice", :min 4, :max "5",
        :patternCodeableConcept {:coding [{:system "CodeSystem", :code "RAD"}]}, :mustSupport true}]}})
    {:required #{"category"}
     :elements
     {:category
      {:mustSupport true,
       :min 1 :max 10
       :slicing
       {:discriminator [{:type "pattern", :path "$this"}]
        :rules "open"
        :slices
        {:LaboratorySlice
         {:schema {:pattern {:type "CodeableConcept", :value {:coding [{:system "CodeSystem", :code "LAB"}]}}, :mustSupport true}
          :_required true,
          :min 2 :max 3
          :match {:coding [{:system "CodeSystem", :code "LAB"}]}},

         :Radiologylice
         {:schema {:pattern {:type "CodeableConcept", :value {:coding [{:system "CodeSystem", :code "RAD"}]}}, :mustSupport true}
          :_required true,
          :min 4 :max 5
          :match {:coding [{:system "CodeSystem", :code "RAD"}]}}}}}}})

;;0.1ms
  (translate
   {:url "http://hl7.org/fhir/StructureDefinition/Profile",
    :baseDefinition "http://hl7.org/fhir/StructureDefinition/Observation"
    :kind "resource",
    :derivation "specialization",
    :differential
    {:element
     [{:id "Observation", :path "Observation", :short "US Core Blood Pressure Profile"}
      {:id "Observation.code", :path "Observation.code", :short "Blood Pressure", :type [{:code "CodeableConcept"}], :patternCodeableConcept {:coding [{:system "http://loinc.org", :code "85354-9"}]}, :mustSupport true}
      {:id "Observation.component", :path "Observation.component", :slicing {:discriminator [{:type "pattern", :path "code"}], :ordered false, :rules "open"}, :short "Component observations", :min 2, :max "*", :mustSupport true}
      {:id "Observation.component:systolic", :path "Observation.component", :sliceName "systolic", :short "Systolic Blood Pressure", :min 1, :max "1", :mustSupport true}
      {:id "Observation.component:systolic.code", :path "Observation.component.code", :short "Systolic Blood Pressure Code", :min 1, :max "1", :patternCodeableConcept {:coding [{:system "http://loinc.org", :code "8480-6"}]}, :mustSupport true}
      {:id "Observation.component:systolic.valueQuantity", :path "Observation.component.valueQuantity", :short "Vital Sign Component Value", :type [{:code "Quantity"}], :mustSupport true}
      {:id "Observation.component:systolic.valueQuantity.value", :path "Observation.component.valueQuantity.value", :min 1, :max "1", :type [{:code "decimal"}], :mustSupport true}
      {:id "Observation.component:systolic.valueQuantity.unit", :path "Observation.component.valueQuantity.unit", :min 1, :max "1", :type [{:code "string"}], :mustSupport true}
      {:id "Observation.component:systolic.valueQuantity.system", :path "Observation.component.valueQuantity.system", :min 1, :max "1", :type [{:code "uri"}], :fixedUri "http://unitsofmeasure.org", :mustSupport true}
      {:id "Observation.component:systolic.valueQuantity.code", :path "Observation.component.valueQuantity.code", :min 1, :max "1", :type [{:code "code"}], :fixedCode "mm[Hg]", :mustSupport true}
      {:id "Observation.component:diastolic", :path "Observation.component", :sliceName "diastolic", :short "Diastolic Blood Pressure", :min 1, :max "1", :mustSupport true}
      {:id "Observation.component:diastolic.code", :path "Observation.component.code", :short "Diastolic Blood Pressure Code", :min 1, :max "1", :patternCodeableConcept {:coding [{:system "http://loinc.org", :code "8462-4"}]}, :mustSupport true}
      {:id "Observation.component:diastolic.valueQuantity", :path "Observation.component.valueQuantity", :short "Vital Sign Component Value", :type [{:code "Quantity"}], :mustSupport true}
      {:id "Observation.component:diastolic.valueQuantity.value", :path "Observation.component.valueQuantity.value", :min 1, :max "1", :type [{:code "decimal"}], :mustSupport true}
      {:id "Observation.component:diastolic.valueQuantity.unit", :path "Observation.component.valueQuantity.unit", :min 1, :max "1", :type [{:code "string"}], :mustSupport true}
      {:id "Observation.component:diastolic.valueQuantity.system", :path "Observation.component.valueQuantity.system", :min 1, :max "1", :type [{:code "uri"}], :fixedUri "http://unitsofmeasure.org", :mustSupport true}
      {:id "Observation.component:diastolic.valueQuantity.code", :path "Observation.component.valueQuantity.code", :min 1, :max "1", :type [{:code "code"}], :fixedCode "mm[Hg]", :mustSupport true}]}})

  (matcho/match
   (calculate-actions
    (parse-path {:path "Patient.name", :mustSupport false})
    (parse-path {:sliceName "race"
                 :path "Patient.extension",
                 :min 0,
                 :max "1",
                 :type [{:code "Extension", :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"]}],
                 :mustSupport false}))

    [{:type :exit, :el :name}
     {:type :enter, :el :extension}
     {:type :enter-slice, :sliceName "race"}
     nil?])

  (matcho/match
   (translate
    {:differential
     {:element
      [{:path "Patient", :mustSupport false}
       {:path "Patient.name", :mustSupport true}
       {:sliceName "race"
        :path "Patient.extension",
        :min 1, :max "1",
        :type [{:code "Extension", :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"]}]}
       {:sliceName "ethnicity"
        :path "Patient.extension",
        :min 0, :max "1",
        :type [{:code "Extension", :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"]}]}
       {:sliceName "tribal"
        :path "Patient.extension",
        :min 0, :max "8"
        :type [{:code "Extension", :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-tribal-affiliation"]}]}
       {:sliceName "birthsex",
        :path "Patient.extension",
        :min 0, :max "1",
        :short "Birth Sex Extension",
        :type [{:code "Extension", :profile ["http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"]}]}]}})

    {:extensions {:race       {:url "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"
                               :min 1 :max 1},
                  :ethnicity  {:url "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"
                               :max 1},
                  :tribal     {:url "http://hl7.org/fhir/us/core/StructureDefinition/us-core-tribal-affiliation",
                               :array nil?
                               :max 8},
                  :birthsex   {:short "Birth Sex Extension",
                               :url "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"}}
     :elements {:name {:mustSupport true}}})

  (matcho/match
   (translate
    {:differential
     {:element
      [{:path "CareTeam.member",
        :type [{:code "Reference", :targetProfile "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"}
               {:code "Reference", :targetProfile "http://hl7.org/fhir/StructureDefinition/Practitioner"}
               {:code "Reference", :targetProfile "http://hl7.org/fhir/StructureDefinition/RelatedPerson"}
               {:code "Reference"}]}]}})
    {:elements
     {:member
      {:type "Reference",
       :refers [{:profile "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient"}
                {:resource "Practitioner"}
                {:resource "RelatedPerson"}
                nil?]}}})

  (matcho/match
   (translate
    {:differential
     {:element
      [{:path "CareTeam.member",
        :type [{:code "Reference"}]}]}})
    {:elements
     {:member
      {:type "Reference",
       :refers nil?}}})

  {:id "Extension.extension:ombCategory"
   :path "Extension.extension",
   :sliceName "ombCategory",
   :min 0, :max "1"
   :definition "The 2 ethnicity category codes according to the [OMB Standards for Maintaining, Collecting, and Presenting Federal Data on Race and Ethnicity, Statistical Policy Directive No. 15, as revised, October 30, 1997](https://www.govinfo.gov/content/pkg/FR-1997-10-30/pdf/97-28653.pdf).",
   :short "Hispanic or Latino|Not Hispanic or Latino",
   :mapping [{:identity "iso11179", :map "/ClinicalDocument/recordTarget/patientRole/patient/ethnicGroupCode"}],
   :type [{:code "Extension"}],
   :mustSupport true}

;; that's does not work
  ;; why just slicing does not work
  (matcho/match
   (translate
    {:type "Extension"
     :differential
     {:element
      [{:path "Extension", :short "US Core ethnicity Extension", :min 0, :max "1"}
       {:path "Extension.extension", :slicing {:discriminator [{:type "value", :path "url"}], :rules "open"}, :min 1}
       {:path "Extension.extension", :min 0, :short "Hispanic or Latino|Not Hispanic or Latino", :type [{:code "Extension"}], :mustSupport true, :sliceName "ombCategory", :max "1"}
       {:path "Extension.extension.url", :min 1, :max "1", :type [{:code "uri"}], :fixedUri "ombCategory"}
       {:path "Extension.extension.value[x]", :min 1, :max "1", :type [{:code "Coding"}], :binding {:strength "required", :valueSet "http://hl7.org/fhir/us/core/ValueSet/omb-ethnicity-category"}}
       {:path "Extension.extension", :min 0, :short "Extended ethnicity codes", :type [{:code "Extension"}], :sliceName "detailed", :max "*"}
       {:path "Extension.extension.url", :min 1, :max "1", :type [{:code "uri"}], :fixedUri "detailed"}
       {:path "Extension.extension.value[x]", :min 1, :max "1", :type [{:code "Coding"}], :binding {:strength "required", :valueSet "http://hl7.org/fhir/us/core/ValueSet/detailed-ethnicity"}}
       {:path "Extension.extension", :min 1, :short "ethnicity Text", :type [{:code "Extension"}], :mustSupport true, :sliceName "text", :max "1"}
       {:path "Extension.extension.url", :min 1, :max "1", :type [{:code "uri"}], :fixedUri "text"}
       {:path "Extension.extension.value[x]", :min 1, :max "1", :type [{:code "string"}]}
       {:path "Extension.url", :min 1, :max "1", :fixedUri "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"}
       {:path "Extension.value[x]", :min 0, :max "0"}]}})
    {:type "Extension",
     :class "extension",
     :required #{"url" "extension"}
     :extensions
     {:ombCategory {:url "ombCategory",
                    :type "Extension",
                    :required #{"url" "value" "valueCoding"}
                    :elements {;; TODO: better to remove
                               :url {:type "uri", :pattern {:type "Uri", :value "ombCategory"}},
                               :value {:binding {:strength "required", :valueSet "http://hl7.org/fhir/us/core/ValueSet/omb-ethnicity-category"},
                                       :choices ["valueCoding"]}
                               :valueCoding {:type "Coding",
                                             :binding {:strength "required", :valueSet "http://hl7.org/fhir/us/core/ValueSet/omb-ethnicity-category"},
                                             :choiceOf "value"}}}
      :detailed {:url "detailed",
                 :type "Extension",
                 :array true,
                 :required #{"url" "value" "valueCoding"}
                 :elements {:value {:binding {:strength "required", :valueSet "http://hl7.org/fhir/us/core/ValueSet/detailed-ethnicity"},
                                    :choices ["valueCoding"]},
                            :valueCoding {:type "Coding",
                                          :binding {:strength "required", :valueSet "http://hl7.org/fhir/us/core/ValueSet/detailed-ethnicity"},
                                          :choiceOf "value"}}},
      :text {:url "text",
             :type "Extension",
             :required #{"url" "value" "valueString"}
             :elements {:url {:type "uri", :pattern {:type "Uri", :value "text"}},
                        :value {:choices ["valueString"]},
                        :valueString {:type "string", :choiceOf "value"}}}},
     :elements {:url {:pattern {:type "Uri", :value "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity"}},
                :value {:choices []}}})

  {:extensions
   {:ombCategory {:url "ombCategory"}
    :detailed    {:url "detailed"}}}

  (matcho/match
   (translate
    {:differential
     {:element
      [{:path "DocumentReference"}
       {:path "DocumentReference.context.related",
        :type [{:code "Reference", :targetProfile ["http://hl7.org/fhir/uv/genomics-reporting/StructureDefinition/genomics-report"]}]}]}})

    {:class "unknown",
     :elements
     {:context
      {:type nil? :refers nil?
       :elements
       {:related
        {:type "Reference",
         :refers [{:profile "http://hl7.org/fhir/uv/genomics-reporting/StructureDefinition/genomics-report"}]}}}}}))

(comment)
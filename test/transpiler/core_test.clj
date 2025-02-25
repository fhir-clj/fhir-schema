(ns transpiler.core-test
  (:require [clojure.test :refer :all]
            [transpiler.fhir-schema :as fhir-schema]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [matcho.core :as matcho]))

(defn golden-fhirschema [filename]
  (let [content (slurp filename)
        parsed (json/parse-string content true)
        fhirschema-filename (str/replace filename #".sd.json$" ".fs.json")
        fhirschema (json/generate-string (fhir-schema/translate parsed) {:pretty true})]
    (spit fhirschema-filename fhirschema)))

(defn load-golden [filename]
  (let [content (slurp (str "test/golden/" filename))
        parsed (json/parse-string content true)] parsed))

(deftest structure-definition-test
  (matcho/match
   (let [sd (load-golden "patient.sd.json")
         fs (load-golden "patient.fs.json")]
     (fhir-schema/translate sd) fs))

  (testing "patient"
    (let [sd (load-golden "patient.sd.json")
          fs (load-golden "patient.fs.json")]
      (is (= (fhir-schema/translate sd) fs))))

  #_(testing "unsignedInt"
      (let [int-sd (load-structure-definition "unsignedInt.json")]
        (is (= (fhir-schema/translate int-sd)
               {:url         "http://hl7.org/fhir/StructureDefinition/unsignedInt|4.0.1"
                :base        "http://hl7.org/fhir/StructureDefinition/integer|4.0.1"
                :type        "unsignedInt" #_"http://hl7.org/fhir/StructureDefinition/unsignedInt|4.0.1"
                :system-type ""           #_"http://hl7.org/fhirpath/System.String"
                :kind        "primitive-type"
                :derivation  "specialization"
                :description "Base StructureDefinition for unsignedInt type: An integer with a value that is not negative (e.g. >= 0)"
                "regex"       "[0]|([1-9][0-9]*)"}))))

  #_(testing "element"
      (let [element-sd (load-structure-definition "element")]
        (is (= (fhir-schema/translate element-sd)
               {:constraints {:ele-1 {:human "All FHIR elements must have a @value or children",
                                      :severity "error",
                                      :expression "hasValue() or (children().count() > id.count())"}}
                :elements {:id {:type "http://hl7.org/fhir/StructureDefinition/string|4.0.1" :scalar true}
                           :extension {:slicing {:discriminator [{:type "value", :path "url"}],
                                                 :description "Extensions are always sliced by (at least) url",
                                                 :rules "open"}
                                       :type "http://hl7.org/fhir/StructureDefinition/Extension|4.0.1",
                                       :array true}},
                :description "Base StructureDefinition for Element Type: Base definition for all elements in a resource.",
                :type "Element" #_"http://hl7.org/fhir/StructureDefinition/Element|4.0.1",
                :id   "Element",
                :kind "complex-type",
                :url  "http://hl7.org/fhir/StructureDefinition/Element|4.0.1"})))))

(comment
  (run-tests 'transpiler.core-test)
  (io/resource "golden/patient.sd.json")

  (golden-fhirschema "test/golden/patient.sd.json")
  (fhir-schema/translate (golden-fhirschema "patient.sd.json"))



  (-> (java.io.File. ".") .getAbsolutePath))
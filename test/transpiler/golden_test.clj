(ns transpiler.golden-test
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is run-tests]]
   [fhir.schema.translate :as fhir-schema]
   [golden.core :as golden]))

(defn sd->fhir-schema [sd-filename]
  (-> (slurp sd-filename)
      (json/parse-string true)
      (fhir-schema/translate)))

(deftest structure-definition-test
  (golden/as-json "test/golden/patient.fs.json"
                  (sd->fhir-schema "test/golden/patient.sd.json"))

  (golden/as-json "test/golden/questionnaire.fs.json"
                  (sd->fhir-schema "test/golden/questionnaire.sd.json"))

  (golden/as-json "test/golden/bundle.fs.json"
                  (sd->fhir-schema "test/golden/bundle.sd.json"))

  (golden/as-json "test/golden/primitive/string.fs.json"
                  (sd->fhir-schema "test/golden/primitive/string.sd.json"))

  (golden/as-json "test/golden/primitive/unsignedInt.fs.json"
                  (sd->fhir-schema "test/golden/primitive/unsignedInt.sd.json"))

  (golden/as-json "test/golden/primitive/boolean.fs.json"
                  (sd->fhir-schema "test/golden/primitive/boolean.sd.json"))

  (golden/as-json "test/golden/complex/element.fs.json"
                  (sd->fhir-schema "test/golden/complex/element.sd.json"))

  (golden/as-json "test/golden/complex/address.fs.json"
                  (sd->fhir-schema "test/golden/complex/address.sd.json"))

  (golden/as-json "test/golden/complex/extension.fs.json"
                  (sd->fhir-schema "test/golden/complex/extension.sd.json"))

  (golden/as-json "test/golden/complex/backbone-element.fs.json"
                  (sd->fhir-schema "test/golden/complex/backbone-element.sd.json")))

(comment
  (run-tests 'transpiler.core-test)
  (io/resource "golden/patient.sd.json"))

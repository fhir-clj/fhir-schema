(ns transpiler.core-test
  (:require [clojure.test :refer :all]
            [transpiler.fhir-schema :as fhir-schema]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [matcho.core :as matcho]))

(defn update-golden? []
  (= "true" (System/getenv "UPDATE_GOLDEN")))

(defn golden-file-content [expect-filename actual-content]
  (let [expect-content (when (.exists (io/file expect-filename))
                         (slurp expect-filename))]
    (cond
      (or (nil? expect-content)
          (update-golden?))
      (do (spit expect-filename actual-content)
          [actual-content actual-content])

      :else
      [expect-content actual-content])))

(defn fhir-schema-content [sd-filename]
  (let [fs-filename (str/replace sd-filename #".sd.json$" ".fs.json")
        sd          (-> (slurp sd-filename)
                        (json/parse-string true))
        fs-json     (-> sd
                        (fhir-schema/translate)
                        (json/generate-string {:pretty true}))]
    (golden-file-content fs-filename fs-json)))

(defmacro golden-test [get-content filename & [as-text]]
  (let [f (if as-text identity #(json/parse-string % true))]
    `(let [[expect-content# actual-content#] (->> (~get-content ~filename)
                                                  (map ~f))]
       (is (= expect-content# actual-content#)
           (str "golden-fhir-schema check fail for: " ~filename)))))

(deftest structure-definition-test
  (golden-test fhir-schema-content "test/golden/patient.sd.json")
  (golden-test fhir-schema-content "test/golden/string.sd.json"))

(comment
  (run-tests 'transpiler.core-test)
  (io/resource "golden/patient.sd.json"))

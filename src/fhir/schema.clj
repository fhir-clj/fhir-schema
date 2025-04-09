(ns fhir.schema
  (:require [clojure.set]
            [clojure.string :as str]))

(defn reduce-indexed
  ([f init coll]
   (reduce (fn [acc [idx x]]
             (f acc idx x))
           init
           (map-indexed vector coll)))
  ([f coll]
   (reduce-indexed f (first coll) (rest coll))))

(declare *validate)

(defn add-error [vctx error]
  (update vctx :errors conj (update error :path (fn [x] (or x (:path vctx))))))

(defn resolve-type   [vctx type-ref]
  ;; (println :resolve-type type-ref (get-in vctx [:ctx :schemas type-ref]))
  (get-in vctx [:ctx :schemas type-ref]))

(defn resolve-schema [vctx schema-ref]
  (get-in vctx [:ctx :schemas schema-ref]))

(defn add-schema-ref [vctx schema-ref schema-path]
  (if-let [sch (resolve-schema vctx schema-ref)]
    (update vctx :schemas conj {:schema sch :path (conj schema-path schema-ref)})
    (add-error vctx {:type :schema/unknown :schema schema-ref :schema-path schema-path})))

(defn add-type-schema [vctx type-ref schema-path]
  (assert (string? type-ref) (pr-str type-ref))
  ;; (println :add-type-schema type-ref)
  (if-let [sch (resolve-type vctx type-ref)]
    (update vctx :schemas conj {:schema sch :path (conj schema-path type-ref)})
    (add-error vctx {:type :type/unknown :schema type-ref :schema-path schema-path})))

;; TODO: schema path
(defn validate-minmax [vctx v]
  (let [min-sch (->> (:schemas vctx)
                     (filter (fn [x] (get-in x [:schema :min])))
                     (sort-by (fn [x] (get-in x [:schema :min])))
                     first)
        min-v (get-in min-sch [:schema :min])
        max-sch (->> (:schemas vctx)
                     (filter  (fn [x] (get-in x [:schema :max])))
                     (sort-by (fn [x] (get-in x [:schema :max])))
                     last)
        max-v (get-in min-sch [:schema :max])
        cnt (count v)]
    (cond-> vctx
      (and min-sch (< cnt min-v))
      (add-error {:type :min :message (str "expected min=" min-v " got " cnt)
                  :value cnt
                  :expected min-v
                  :schema-paths [(conj (:path min-sch) :min)]})

      (and max-sch (> cnt max-v))
      (add-error {:type :max
                  :message (str "expected max=" max-v " got " cnt)
                  :value cnt
                  :expected max-v
                  :schema-paths [(conj (:path max-sch) :max)]}))))

(defn validate-slicing [vctx v]
  vctx)

(def ARRAY_RULES {:minmax #'validate-minmax
                  :slicing #'validate-slicing})

(defn validate-array-rules [vctx v]
  (->> ARRAY_RULES
       (reduce (fn [vctx [_rule-name rule-fn]]
                 (rule-fn vctx v)) vctx)))

(defn is-array? [vctx]
  (->> (:schemas vctx)
       (filter (fn [x] (get-in x [:schema :array])))
       (first)))


;; handle primitive extensions
(defn data-elements [vctx data]
  (->> data
       (reduce (fn [data-els [k v]]
                 (if (str/starts-with? (name k) "_")
                   ;;TODO: smart merge for ["ups" nil "ups"]
                   (let [el-k (keyword (subs (name k) 1))]
                     (update data-els el-k merge {:key el-k :path (conj (:path vctx) k) :extension v}))
                   (update data-els k merge {:key k :path (conj (:path vctx) k) :value v})))
               {})))

(defn validate-string [vctx schemas data]
  ;; (println "validate-string" (pr-str data) (string? data))
  (if (not (string? data))
    (add-error vctx {:type :type
                     :message "Expected type string"
                     :value data :schema-path (:path (first schemas))})
    vctx))

(def TYPE_VALIDATORS {"string" #'validate-string})

;; How to test with string?
;; 1. kind = primitive, elements.value.type = http://hl7.org/fhirpath/System.String 
;; 2. baseDefinition = http://hl7.org/fhir/StructureDefinition/string  - cross version


;; should we use kind?
(defn validate-type [vctx schemas data]
  (let [type-idx (group-by :schema schemas)]
    (->> type-idx
         (reduce (fn [vctx [type schemas]]
                   (if-let [vld (get TYPE_VALIDATORS type)]
                     (vld vctx schemas data)
                     ;; ignore if no type validator
                     vctx ;;(add-error vctx {:type :type/unknown :value type :path (:path vctx)})
                     ))
                 vctx))))


;; TODO: fix schema path
(defn validate-choices [vctx schemas data]
  ;; TODO extra data walk
  (let [choice-elements (->> data
                             (reduce (fn [acc [k v]]
                                       ;; extra walk over schemas
                                       (if-let [choice-of (->> schemas (some (fn [s] (get-in s [:parent :elements k :choiceOf]))))]
                                         (assoc-in acc [(keyword choice-of) k] v)
                                         acc))
                                     {}))]
    (->> choice-elements
         (reduce-kv (fn [vctx k value]
                      (let [vctx (if (< 1 (count value))
                                   (add-error vctx {:type :choices/multiple
                                                    :path (conj (:path vctx) k)
                                                    :message (str "Only one choice element is allowd")
                                                    :schema-path ()
                                                    :value value})
                                   vctx)]
                        (->> schemas
                             (reduce (fn [vctx {sch :schema schema-path :path}]
                                       (if-let [choices (get sch k)]
                                         (if (contains? (into #{} choices) (first (keys value)))
                                           vctx
                                           (add-error vctx {:type :choice/excluded
                                                            :message (str "Choice element " (name k) " is not allowed, only " (str/join ", " choices))
                                                            :path (conj (:path vctx) k)
                                                            :schema-path schema-path}))
                                         vctx))
                                     vctx))))
                    vctx))))

;; {schemas: #{ {schema: ['name', 'birthDateh], schema-path: [], parent-schema: {}}}}
(defn validate-required [vctx schemas data]
  (let [elements-idx  (->> schemas
                           (reduce (fn [acc {sch :schema :as schema-entry}]
                                     (->> sch
                                          (reduce (fn [acc element-name]
                                                    (update acc element-name conj schema-entry))
                                                  acc))) {}))]
    (->> elements-idx
         (reduce (fn [vctx [el schemas]]
                   (if (or (not (nil? (get data (keyword el))))
                           (not (nil? (get data (keyword (str "_" el))))))
                     vctx
                     (add-error vctx {:type :require
                                      :path (conj (:path vctx) (keyword el))
                                      :message (str "Element " el " is required")
                                      :schema-paths (mapv :path schemas)})))
                 vctx))))

(defn match-pattern [pat data]
  (= pat data))

(defn validate-pattern [vctx schemas data]
  (->> schemas
       (reduce
        (fn [vctx {sch :schema sch-path :path}]
          (let [pat (first (vals sch))]
            (if (match-pattern pat data)
              vctx
              (add-error vctx {:type :pattern
                               :expected pat
                               :schema-path sch-path
                               :got data}))))
        vctx)))

(def VALUE_RULES {:type     #'validate-type
                  :choices  #'validate-choices
                  :required #'validate-required
                  :pattern  #'validate-pattern
                  ;; :excluded
                  ;; :constraints
                  ;; :constants
                  ;; :min/maxValue <- type dependant
                  ;; :maxLength    <- type dependant
                  ;; :bindings     <- type dependant
                  })

;; us-patient-> base.Patient-> name -> HumanName -> assert

;; provide all schemas
(defn validate-value-rules [vctx data]
  ;; (println :validate-value (:schemas vctx) data)
  (->> VALUE_RULES
       (reduce
        (fn [vctx [rule-name rule-fn]]
          (if-let [schemas (->> (:schemas vctx)
                                (mapcat (fn [{sch :schema path :path}]
                                          ;; (println :sch sch rule-name)
                                          (when-let [rule (get sch rule-name)]
                                            [{:schema rule :parent sch :path (conj path rule-name)}])))
                                (seq))]
            (rule-fn vctx schemas data)
            vctx))
        vctx)))

(def ELEMENTS_RULES [:required :expected :choices :contstraint])

(defn validate-elements-rules [vctx elements data]
  (->> ELEMENTS_RULES
       (reduce (fn [vctx rule-name] vctx) vctx)))


;; add schemas to schema-set - data.resourceTyp, data.meta.profile
;; schema.type, schema.profile (sometimes with data - for example bundle type: Resource -> data.resourceType/meta.profile -> resolution)
;;   entry: {type: Resource} ; {resourceType, profiles} => resolve(resourceType), resolve(prifles)
;;   name: {type: HumanName} + resolve(HumanName)
;;   extension: {type: Extensions} +  {url: 'exurl'} -> resolve(url)
;;   nexted extensions - url is slice name not definition url
;;   can be mixed with normal extensions (extensions on extensions?)
;    Algorithm if we in nested extension and there is slice with this url - do not resolve url for this extensions
;; initial:  Resource, data.resourceType/meta.profile
;; extension: {slicing: {slice: {'ombCategory', {special-flag: ..}}}} look extension context before resolving url
;; may be based on string format - ask Grahame and community
(defn add-schemas [vctx data]
  (let [vctx (cond-> vctx
               ;; todo check schema - should have type: Resource
               (:resourceType data) (add-type-schema (:resourceType data) [])
               #_#_(:type data)         (add-type-schema (:type data) []))
        vctx (->> (get-in data [:meta :profile])
                  (reduce (fn [vctx profile] (add-schema-ref vctx profile [])) vctx))]
    (->> (:schemas vctx)
         (reduce (fn [vctx {schema :schema path :path}]
                   (let [vctx (if-let [tp   (:type schema)] (add-type-schema vctx tp   (conj path :type)) vctx)
                         vctx (if-let [base (:base schema)] (add-type-schema vctx base (conj path :base)) vctx)]
                     vctx))
                 vctx))))

(def primitive-schema {:elements {:id {:type "code"} :extension {:array true :type "Extension"}}})
;; handle primitive elements the right way
(defn get-element-schemas [vctx k]
  ;; (println :get-schemas k (:schemas vctx))
  (let [schemas (if (str/starts-with? (name k) "_") #{{:schema primitive-schema :path []}} #{})
        schemas (->> (:schemas vctx)
                     (mapcat (fn [{schema :schema path :path}]
                               (when-let [el-schema (get-in schema [:elements k])]
                                 [{:schema el-schema :path (conj path k)}])))
                     (into schemas))]
    ;; (println :< schemas)
    schemas))

(defn validate-element [{path :path :as vctx} v]
  ;; (println :validate-el (:path data-element) :is-array (is-array? vctx))
  (if-let [array-schema  (is-array? vctx)]
    (if-not (sequential? v)
      (add-error vctx {:type :type/array :message (str "Expected array") :path path :value v :schema-path (conj (:path array-schema) :array)})
      (let [vctx (validate-array-rules vctx v)]
        (->> v (reduce-indexed (fn [vctx idx v] (*validate (assoc vctx :path (conj path idx)) v)) vctx))))
    (if (sequential? v)
      (add-error vctx {:type :type/array :message (str "Expected not array") :path path :value v})
      (*validate (assoc vctx :path path) v))))

(defn *validate [vctx data]
  ;; (println :*validate (:schemas vctx) data)
  (let [vctx  (add-schemas vctx data)
        vctx  (validate-value-rules vctx data)
        schemas (:schemas vctx)
        path (:path vctx)]
    ;;TODO: handle primitive extensions, especially arrays
    (if (map? data)
      (->> data
           (reduce
            (fn [vctx [k v]]
              (if-let [element-schemas (seq (get-element-schemas (assoc vctx :path path :schemas schemas) k))]
                (validate-element (assoc vctx :schemas element-schemas :path (conj path k)) v)
                (add-error vctx {:type :element/unknown :path (conj path k)})))
            vctx))
      vctx)))

(defn mk-validation-context [ctx schema-refs resource]
  (let [vctx {:ctx ctx :errors [] :deferreds [] :resource resource :path [] :schemas #{}}]
    (reduce (fn [vctx schema-ref] (add-schema-ref vctx schema-ref [schema-ref])) vctx schema-refs )))

(defn validate-schemas [ctx schemas resource]
  (let [vctx (mk-validation-context ctx [] resource)
        vctx (->> schemas
             (reduce (fn [vctx sch] (update vctx :schemas conj {:schema sch :path []}))
                     vctx))]
    (select-keys (*validate vctx resource) [:errors :deferreds])))

(defn validate [ctx schema-refs resource]
  (let [vctx (mk-validation-context ctx schema-refs resource)]
    (select-keys (*validate vctx resource) [:errors :deferreds])))

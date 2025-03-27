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
  (get-in vctx [:ctx :schemas type-ref]))

(defn resolve-schema [vctx schema-ref]
  (get-in vctx [:ctx :schemas schema-ref]))

(defn add-schema-ref [vctx schema-ref schema-path]
  (if-let [sch (resolve-schema vctx schema-ref)]
    (update vctx :schemas conj {:schema sch :path (conj schema-path schema-ref)})
    (add-error vctx {:type :schema/unknown :schema schema-ref :schema-path schema-path})))

(defn add-type-schema [vctx type-ref schema-path]
  (assert (string? type-ref) (pr-str type-ref))
  (println :add-type-schema type-ref)
  (if-let [sch (resolve-type vctx type-ref)]
    (update vctx :schemas conj {:schema sch :path (conj schema-path type-ref)})
    (add-error vctx {:type :type/unknown :schema type-ref :schema-path schema-path})))

(def ARRAY_RULES [:min :max :slicing])

(defn validate-array-rules [vctx v]
  (->> ARRAY_RULES
       (reduce (fn [vctx rule-name] vctx) vctx)))

(defn is-array? [vctx]
  (->> (:schemas vctx)
       (filter (fn [x] (get-in x [:schema :array])))
       (first)))

(defn validate-element [vctx {path :path v :value :as data-element}]
  (println :validate-el (:path data-element) :is-array (is-array? vctx))
  (if-let [array-schema  (is-array? vctx)]
    (if-not (sequential? v)
      (add-error vctx {:type :type/array
                       :message (str "Expected array")
                       :path path
                       :value v
                       :schema-path (conj (:path array-schema) :array)})
      (let [vctx (validate-array-rules vctx v)]
        (->> v (reduce-indexed (fn [vctx idx v] (*validate (assoc vctx :path (conj path idx)) v)) vctx))))
    (*validate (assoc vctx :path (:path data-element)) v)))

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
  (println "validate-string" (pr-str data) (string? data))
  (if (not (string? data))
    (add-error vctx {:type :type
                     :message "Expected type string"
                     :value data :schema-path (:path (first schemas))})
    vctx))

(def TYPE_VALIDATORS {"string" #'validate-string})

;; should we use kind?
(defn validate-type [vctx schemas data]
  (let [type-idx (group-by :schema schemas)]
    (->> type-idx
         (reduce (fn [vctx [type schemas]]
                   (if-let [vld (get TYPE_VALIDATORS type)]
                     (vld vctx schemas data)
                     (add-error vctx {:type :type/unknown :value type :path (:path vctx)})))
                 vctx))))

(defn validate-kind [vctx schemas data]
  (let [kinds-idx (group-by :schema schemas)]
    (->> kinds-idx
         (reduce (fn [vctx [kind schemas]]
                   ;; (println :validate-kind kind schemas)
                   vctx
                   ) vctx))))

(def VALUE_RULES {:type #'validate-type
                  ;; :kind #'validate-kind
                  })

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

(defn add-schemas [vctx data]
  (let [vctx (cond-> vctx
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

(defn get-element-schemas [vctx {k :key :as data-element}]
  ;; (println :get-schemas k (:schemas vctx))
  (->> (:schemas vctx)
       (mapcat (fn [{schema :schema path :path}]
                 (when-let [el-schema (get-in schema [:elements k])]
                   [{:schema el-schema :path (conj path k)}])))
       (into #{})))

(defn *validate [vctx data]
  (println :*validate (:schemas vctx) data)
  (let [vctx  (add-schemas vctx data)
        vctx (validate-value-rules vctx data)]
    (if (map? data)
      (let [elements (data-elements vctx data)
            vctx (validate-elements-rules vctx elements data)]
        (->> elements
             (reduce (fn [vctx [_ data-element]]
                       (->
                        (if-let [element-schemas (seq (get-element-schemas vctx data-element))]
                          (validate-element (assoc vctx :schemas element-schemas :path (:path data-element)) data-element)
                          (add-error vctx {:type :element/unknown :path (:path data-element)}))
                        (assoc :schemas (:schemas vctx))))
                     vctx)))
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





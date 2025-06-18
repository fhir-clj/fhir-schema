(ns fhir.schema.translate
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            #_[clj-yaml.core]))

(defn required-element?
  [element]
  (= 1 (:min element)))

(defn coerce-element-max-cardinality
  [^String value]
  (if (= "*" value)
    Integer/MAX_VALUE
    (when (string? value)
      (parse-long value))))

(defn array-element?
  [element]
  (or (= "*" (:max element))
      (and (:min element) (>= (:min element) 2))
      (and (:max element) (>= (coerce-element-max-cardinality (:max element)) 2))))

(defn parse-int [x]
  (try (Integer/parseInt x)
       (catch Exception _e nil)))

(defn parse-path [el]
  (let [p (:path el)
        path (->> (str/split p #"\.")
                  (rest)
                  (mapv (fn [x] {:el (keyword x)})))
        mx   (and (:max el) (not (= "*" (:max el))) (parse-int (:max el)))
        path-item (select-keys el [:slicing :sliceName])
        path-item (cond

                    (:slicing el)
                    (update path-item :slicing
                            (fn [x] (-> (assoc x :min (:min el))
                                        (cond-> mx (assoc :max mx)))))

                    (:sliceName el)
                    (update path-item :slice
                            (fn [x] (-> (assoc x :min (:min el))
                                        (cond-> mx (assoc :max mx)))))

                    :else path-item)]
    (update path (dec (count path)) merge path-item)))

;; calculate common path
(defn get-common-path [p1 p2]
  (loop [[px & pxs] p1
         [py & pxy] p2
         cp []]
    (if (or (nil? px) (nil? py)
            (not (= (:el px) (:el py))))
      cp
      (recur pxs pxy (conj cp px)))))

;; copy slices from previous path
(defn enrich-path [p1 p2]
  (loop [[px & pxs] p1
         [py & pxy :as p2] p2
         cp []]
    (if (or (nil? px) (nil? py) (not (= (:el px) (:el py))))
      (if py (into cp p2) (into cp pxy))
      (recur pxs pxy (conj cp (merge (select-keys px [:slicing :sliceName]) py))))))

(defn slice-changed? [pi ni]
  (and (:sliceName pi) (:sliceName ni) (not (= (:sliceName pi) (:sliceName ni)))))

(defn exit-slice-action [pi]
  {:type :exit-slice :sliceName (:sliceName pi) :slicing (:slicing pi) :slice (:slice pi)})

;; TBD: tidy
(defn calculate-exits [prev-c cp-c prev-path new-path]
  (loop [i prev-c exits []]
    (if (= i cp-c)
      (if (< 0 i)
        (let [pi (nth prev-path (dec i))
              ni (nth new-path (dec i))]
          (if (slice-changed? pi ni)
            (conj exits (exit-slice-action pi))
            exits))
        exits)
      (recur (dec i)
             (let [pi (nth prev-path (dec i))]
               (-> exits
                   (cond-> (:sliceName pi) (conj (exit-slice-action pi)))
                   (conj {:type :exit :el (:el pi)})))))))

;; TBD: tidy
(defn calculate-enters [cp-c new-c exits prev-path new-path]
  (loop [i cp-c  enters exits]
    (if (= i new-c)
      (if (= new-c cp-c)
        (let [pi (nth prev-path (dec i) nil)
              ni (nth new-path (dec i) nil)]
          (if (and (:sliceName ni) (not (= (:sliceName pi) (:sliceName ni))))
            (conj enters {:type :enter-slice :sliceName (:sliceName ni)})
            enters))
        enters)
      (let [pi (nth new-path i)]
        (recur (inc i)
               (cond-> (conj enters {:type :enter :el (:el (nth new-path i))})
                 (:sliceName pi) (conj {:type :enter-slice :sliceName (:sliceName pi)})))))))

(defn calculate-actions [prev-path new-path]
  (let [prev-length          (count prev-path)
        new-length           (count new-path)
        common-path-length   (count (get-common-path prev-path new-path))
        exits                (calculate-exits prev-length common-path-length prev-path new-path)
        enters               (calculate-enters common-path-length new-length exits prev-path new-path)]
    enters))

(defn pop-and-update
  "peek from stack and update value on top of stack"
  [stack update-fn]
  (let [peek-v (peek stack)
        prev-stack (pop stack)
        last-v (peek prev-stack)
        prev-prev-stack (pop prev-stack)]
    (conj prev-prev-stack (update-fn last-v peek-v))))

;; TODO: if no discriminator - add :dynamic instruction
;; TODO: support reslicing
;; TODO: honor type
(defn build-match-for-slice [sl peek-v]
  (->> (:discriminator sl)
       (filterv (fn [{tp :type}]
                  (contains? #{"pattern" "value" nil} tp)))
       (reduce (fn [match {p :path}]
                 (if (= (str/trim p) "$this")
                   (merge match (get-in peek-v [:pattern :value]))
                   (let [pattern-p  (mapv keyword (str/split p #"\."))
                         pp (into [:elements] (interpose :elements pattern-p))
                         v  (get-in peek-v (conj pp :pattern))]
                     (assoc-in match pattern-p (:value v))))) {})))

(defn build-slice-node [peek-v match slice]
  (cond-> {:match match :schema peek-v}
    (:min slice)
    (assoc :min (:min slice))

    (and (:min slice) (> (:min slice) 1))
    (assoc :_required true)

    (:max slice)
    (assoc :max (:max slice))))

(defn build-slice [{sn :sliceName  sl :slicing slice :slice :as _item} last-v peek-v]
  (let [match (build-match-for-slice sl peek-v)
        slice-node (build-slice-node peek-v match slice)]
    (-> last-v
        (update :slicing merge sl)
        (assoc-in [:slicing :slices (keyword sn)] slice-node))))

(defn slicing-to-extensions [slicing]
  (->> (get-in slicing [:slicing :slices])
       (reduce (fn [acc [k {match :match schema :schema :as slice}]]
                 (assoc acc k (merge {:url (:url match)}
                                     (dissoc slice :schema :match)
                                     schema)))
               {})))

(defn add-element [el last-v peek-v]
  (let [base (if (= :extension el)
               (assoc-in last-v [:extensions] (slicing-to-extensions peek-v)) last-v)]
    (cond-> (assoc-in base [:elements el] (dissoc peek-v :_required))
      (:_required peek-v) (update :required (fn [x] (conj (or x #{}) (name el)))))))

;; (defn debug-action [item stack]
;;   (println :> item)
;;   (doseq [x (reverse stack)]
;;     (println "  |-----------------------------------")
;;     (println
;;      (->> (str/split (clj-yaml.core/generate-string x) #"\n")
;;           (mapv (fn [x] (str "  | " x)))
;;           (str/join "\n"))))
;;   (println "  |-----------------------------------"))

(defn apply-actions [value-stack actions value]
  ;; TODO: if next is enter - enter with empty value
  (loop [stack value-stack
         [a & as] actions]
    (if (nil? a)
      stack
      (let [{tp :type el :el :as action} a
            ;; _ (debug-action action stack)
            value (if (= :enter (:type (first as))) {} value) ;; we need this is we enter several items in a path, i.e .a .b.c.d
            stack (case tp
                    :enter       (conj stack value)
                    :enter-slice (conj stack value)
                    :exit        (pop-and-update stack (fn [last-v peek-v] (add-element el last-v peek-v)))
                    :exit-slice  (pop-and-update stack (fn [last-v peek-v] (build-slice action last-v peek-v))))]
        (recur stack as)))))

;; alorythm
;; enrich path from previous
;; calcluate enter/exit from path
;; execute enter/exit logic while building final structure
(def EMPTY_PATH [])

(defn choice? [e]
  (or (str/ends-with? (:path e) "[x]")
      ;; There might be multiple types
      (and (> (count (:type e)) 1)
           ;; But! there might be multiple types with the same name
           ;; and that's not a choice type.
           (> (count (set (map :code (:type e)))) 1))))

(defn capitalize [s]
  (if (seq s) (str (str/upper-case (subs s 0 1)) (subs s 1)) s))

(defn uncapitalize [s]
  (if (and (string? s)
           (seq s))
    (str (clojure.string/lower-case (subs s 0 1)) (subs s 1))
    s))

(defn union-elements [{p :path :as e}]
  (let [prefix (str/replace p #"\[x\]" "")
        fs-prefix (last (str/split prefix #"\."))]
    (->> (:type e)
         (mapv (fn [{c :code :as tp}]
                 (-> e
                     (dissoc :binding)
                     (assoc :path (str prefix (capitalize c)) :type [tp] :choiceOf fs-prefix))))
         (into [(-> (assoc e :path prefix)
                    (dissoc :type)
                    (assoc :choices (->> (:type e) (mapv (fn [{c :code}] (str fs-prefix (capitalize c)))))))]))))

(defn pattern-type-normalize [n]
  (get {"Instant" "instant"
        "Time" "time"
        "Date" "date"
        "DateTime" "dateTime"
        "Decimal" "decimal"
        "Boolean" "boolean"
        "Integer" "integer"
        "String" "string"
        "Uri" "uri"
        "Base64Binary" "base64Binary"
        "Code" "code"
        "Id" "id"
        "Oid" "oid"
        "UnsignedInt" "unsignedInt"
        "PositiveInt" "positiveInt"
        "Markdown" "markdown"
        "Url" "url"
        "Canonical" "canonical"
        "Uuid" "uuid"} n n))

(defn process-patterns [e]
  (let [e-with-pattern
        (->> e
             (reduce
              (fn [acc [k v]]
                (cond
                  (str/starts-with? (name k) "pattern")
                  (assoc acc :pattern {:type (-> (name k)
                                                 (str/replace #"^pattern" "")
                                                 (pattern-type-normalize))
                                       :value v})

                  (str/starts-with? (name k) "fixed")
                  (assoc acc :pattern {:type (-> (name k)
                                                 (str/replace #"^fixed" "")
                                                 (pattern-type-normalize))
                                       :value v})

                  :else (assoc acc k v)))
              {}))]
    (cond-> e-with-pattern
      (and (nil? (:type e-with-pattern))
           (some? (get-in e-with-pattern [:pattern :type])))
      (assoc :type (get-in e-with-pattern [:pattern :type])))))

(defn base-profile? [tp]
  (re-matches #"http://hl7.org/fhir/StructureDefinition/[a-zA-Z]+" tp))

(defn build-refers [tps]
  (->> tps
       (mapcat
        (fn [{tp :targetProfile :as item}]
          (when tp
            (->> (if (vector? tp) tp [tp])
                 (mapv (fn [tp]
                         (assert (string? tp) [tp item]) tp
                         #_(if (base-profile? tp) ;; commented - decided to follow the specification https://fhir-schema.github.io/fhir-schema/reference/element.html?highlight=reference#schema-5
                             {:resource (last (str/split tp #"/"))}
                             {:profile tp})))))))))

(defn preprocess-element [e]
  (let [tp (get-in e [:type 0 :code])]
    (cond (= tp "Reference")
          (let [refers (->> (build-refers (:type e))
                            (distinct)
                            (sort)
                            (vec))]
            (cond-> (assoc e :type [{:code "Reference"}])
              (seq refers) (assoc :refers refers)))
          :else e)))

(defn get-extension [exts url]
  (->> exts
       (filter #(= url (:url %)))
       (first)))

(def binding-name-ext "http://hl7.org/fhir/StructureDefinition/elementdefinition-bindingName")
(def default-type-ext "http://hl7.org/fhir/StructureDefinition/elementdefinition-defaulttype")

(defn build-element-binding [e structure-definition]
  (let [normalize-binding (fn [binding]
                            (let [exts (:extension binding)
                                  binding-name-ext (get-extension exts binding-name-ext)]
                              (cond-> (select-keys binding [:strength :valueSet])
                                (some? binding-name-ext)
                                ;; TODO: new field for codegen. Need to update FHIR Schema spec
                                (assoc :bindingName (:valueString binding-name-ext)))))]
    (cond
      (:choices e)
      (dissoc e :binding)

      (:choiceOf e)
      (let [decl-path (str (:id structure-definition) "." (:choiceOf e) "[x]")
            decl (->> (get-in structure-definition [:snapshot :element])
                      (filter #(= decl-path (:path %)))
                      (first))]
        (cond-> e
          (:binding decl)
          (assoc :binding (normalize-binding (:binding decl)))))

      (:valueSet (:binding e))
      (assoc e :binding (normalize-binding (:binding e)))

      :else ;; NOTE: dissoc to remove uncommon structure, e.g.:
      ;; http://hl7.org/fhir/StructureDefinition/Task statusReason
      (dissoc e :binding))))

(defn build-element-constraints [e]
  (cond-> e
    (:constraint e)
    (update
     :constraint
     (fn [cs]
       (->> cs
            (reduce (fn [acc {k :key :as c}]
                      (assoc acc k (dissoc c :key :xpath)))
                    {}))))))

(defn parse-max [{mx :max :as _e}]
  (when (and mx (not (= "*" mx)))
    (parse-int mx)))

(defn parse-min [{mn :min :as _e}]
  (when (and mn (> mn 0))
    mn))

(defn build-element-extension [e]
  (let [tp (get-in e [:type 0 :code])
        ext-url (when (= tp "Extension") (get-in e [:type 0 :profile 0]))
        mn (parse-min e)
        mx (parse-max e)]
    (if-not ext-url
      e
      (cond-> (assoc e :url ext-url)
        mn (assoc :min mn)
        mx (assoc :max mx)))))

(defn build-element-cardinality [e]
  (let [mn (parse-min e) mx (parse-max e)]
    (cond-> (dissoc e :min :max)
      (not (:url e))
      (cond->
       (array-element? e)    (-> (assoc :array true)
                                 (cond-> mn (assoc :min mn)
                                         mx  (assoc :max mx)))
       (required-element? e) (assoc :_required true)))))

(defn extract-type-from-extension
  "THAT'S A WEIRD CASE IDK WHY IT EXISTS I WISH IT DIDN'T"
  [e]
  (let [extension (get-in e [:type 0 :extension 0])]
    (when (= (get-in extension [:url]) "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type")
      (get-in extension [:valueUrl]))))

(defn warn [msg]
  (println (format "WARNING: %s" msg)))

(defn build-element-type [e structure-definition]
  (when-not (<= (count (get-in e [:type])) 1)
    (warn (str "More than one type specified: " (get-in e [:type]))))
  (let [type-from-extension (extract-type-from-extension e)
        def-type-ext (get-extension (:extension e) default-type-ext)
        tp (get-in e [:type 0 :code])]
    (cond-> (cond type-from-extension (assoc e :type type-from-extension)
                  (:type e)           (assoc e :type tp)
                  :else               e)
      ;; TODO: add defaultType to FHIR Schema spec
      (and (= (:kind structure-definition) "logical")
           (some? def-type-ext)) (assoc :defaultType (:valueCanonical def-type-ext)))))

(defn clear-element [e]
  (dissoc e :path :slicing :sliceName :id :mapping :example :alias :condition :comment :definition :requirements))

(defn content-reference->element-reference [content-reference structure-definition]
  (let [[_ & path-parts] (-> content-reference (subs 1) (str/split #"[.]"))]
    (reduce (fn [result part] (conj (conj result "elements") part))
            [(:url structure-definition)]
            path-parts)))

(defn build-element-content-reference [elem structure-definition]
  (if-let [ref (:contentReference elem)]
    (-> elem
        (assoc :elementReference
               (content-reference->element-reference ref structure-definition))
        (dissoc :contentReference))
    elem))

(comment
  (build-element-content-reference
   {:contentReference "#Bundle.link"}
   {:id "Bundle"
    :url "http://hl7.org/fhir/StructureDefinition/Bundle"}))

;; TODO add test for constraint
(defn build-element [e structure-definition]
  (-> e
      preprocess-element
      clear-element
      (build-element-binding structure-definition)
      build-element-constraints
      (build-element-content-reference structure-definition)
      build-element-extension
      build-element-cardinality
      (build-element-type structure-definition)
      process-patterns
      (dissoc :extension)))

(defn build-resource-header [structure-definition]
  (-> (select-keys structure-definition [:name :type :url :version :description :package_name :package_version :package_id :kind :derivation])
      (cond-> (:baseDefinition structure-definition) (assoc :base (:baseDefinition structure-definition)))
      (cond-> (:abstract structure-definition) (assoc :abstract true))
      (assoc :class
             (cond
               (and (= "resource" (:kind structure-definition)) (= "constraint" (:derivation structure-definition))) "profile"
               (= "Extension" (:type structure-definition)) "extension"
               :else (or (:kind structure-definition) "unknown")))))

(defn get-differential [structure-definition]
  (->> (get-in structure-definition [:differential :element])
       (filterv (fn [{p :path}] (str/includes? p ".")))))

(defn normalize-fhir-schema [schema]
  (->> schema
       (walk/postwalk
        (fn [form]
          (cond
            (set? form) (->> form sort vec)
            :else form)))
       (walk/keywordize-keys)))

;; TODO: context {elements for elements, elements for resoruce}
;; TODO: discriminator [50%]
;; TODO: array and scalar only for resources or logical models
;; TODO: reslicing (low prioroty)
;; TODO: delete example bindings
;; TODO: slicing on choices

;; Algorithm
;; 1 loop over elements
;; 2 calculate path
;; 3 calculate actions from path and previous path
;; 4 apply actions

(defn translate
  ([structure-definition]
   (translate {} structure-definition))
  ([{package-meta :package-meta} structure-definition]
   (let [is-primitive (= "primitive-type" (:kind structure-definition))
         res (cond-> (build-resource-header structure-definition)
               package-meta (assoc :package-meta package-meta))]
     (loop [value-stack [res]
            prev-path EMPTY_PATH
            [elem & rest-elems] (get-differential structure-definition)
            idx 0]
       (cond
         (or (nil? elem)
             is-primitive)
         (let [actions (calculate-actions prev-path EMPTY_PATH)
               new-value-stack (apply-actions value-stack actions {:index idx})]
           (assert (= 1 (count new-value-stack)))
           (normalize-fhir-schema (first new-value-stack)))

         (choice? elem)
         (recur value-stack prev-path (into (union-elements elem) rest-elems) (inc idx))

         :else
         (let [new-path        (enrich-path prev-path (parse-path elem))
               actions         (calculate-actions prev-path new-path)
               fs-elem         (-> elem
                                   (build-element structure-definition)
                                   (assoc :index idx))
               new-value-stack (apply-actions value-stack
                                              actions
                                              fs-elem)]
           (recur new-value-stack new-path rest-elems (inc idx))))))))

;; (defn transform-constraints [element]
;;   (->> (:constraint element)
;;        (reduce (fn [acc constraint]
;;                  (assoc acc (keyword (:key constraint))
;;                         (select-keys constraint [:human :severity :expression])))
;;                {})))

;; (defn translate [structure-definition]
;;   (let [elements (get-in structure-definition [:differential :element])
;;         base-info (build-resource-header structure-definition)
;;         transformed-elements (reduce (fn [acc element]
;;                                        (let [path (keyword (last (str/split (:path element) #"\.")))]
;;                                          (-> acc
;;                                              (cond-> (str/includes? (:path element) ".") (assoc-in [:elements path] (build-element element))) ;; is that a good idea to use path with "." ?
;;                                              (update :constraints merge (transform-constraints element)))))
;;                                      {} elements)]
;;     (merge base-info transformed-elements)))

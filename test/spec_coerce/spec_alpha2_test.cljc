(ns spec-coerce.spec-alpha2-test
  #?(:cljs (:require-macros [cljs.test :refer [deftest testing is are run-tests]]))
  (:require
   #?(:clj [clojure.test :refer [deftest testing is are]])
   [clojure.alpha.spec :as spec2]
   [clojure.string :as str]
   [clojure.test.check :as tc]
   [clojure.test.check.generators]
   [clojure.test.check.properties :as prop]
   [clojure.alpha.spec.test :as spec2.test]
   #?(:clj
      [clojure.test.check.clojure-test :refer [defspec]])
   #?(:cljs [clojure.test.check.clojure-test :refer-macros [defspec]])
   [spec-coerce.spec-alpha2 :as sc2])
  #?(:clj
     (:import (java.net URI))))

#?(:clj (spec2.test/instrument))

(spec2/def ::infer-int int?)
(spec2/def ::infer-and-spec (spec2/and int? #(> % 10)))
(spec2/def ::infer-and-spec-indirect (spec2/and ::infer-int #(> % 10)))
(spec2/def ::infer-form (spec2/coll-of int?))
(spec2/def ::infer-nilable (spec2/nilable int?))

#?(:clj (spec2/def ::infer-decimal? decimal?))

(sc2/def ::some-coercion sc2/parse-long)

(spec2/def ::first-layer int?)
(sc2/def ::first-layer (fn [x _] (inc (sc2/parse-long x))))

(spec2/def ::second-layer ::first-layer)
(spec2/def ::second-layer-and (spec2/and ::first-layer #(> % 10)))

(spec2/def ::or-example (spec2/or :int int? :double double? :bool boolean?))

(spec2/def ::nilable-int (spec2/nilable ::infer-int))
(spec2/def ::nilable-pos-int (spec2/nilable (spec2/and ::infer-int pos?)))
(spec2/def ::nilable-string (spec2/nilable string?))

(spec2/def ::int-set #{1 2})
(spec2/def ::float-set #{1.2 2.1})
(spec2/def ::boolean-set #{true})
;(spec2/def ::symbol-set #{'foo/bar 'bar/foo})
;(spec2/def ::ident-set #{'foo/bar :bar/foo})
(spec2/def ::string-set #{"hey" "there"})
(spec2/def ::keyword-set #{:a :b})
;(spec2/def ::uuid-set #{#uuid "d6e73cc5-95bc-496a-951c-87f11af0d839"
;                    #uuid "a6e73cc5-95bc-496a-951c-87f11af0d839"})
(spec2/def ::nil-set #{nil})
;#?(:clj (spec2/def ::uri-set #{(URI. "http://site.com")
;                           (URI. "http://site.org")}))
#?(:clj (spec2/def ::decimal-set #{42.42M 1.1M}))

(def enum-set #{:a :b})
(spec2/def ::referenced-set enum-set)

(def enum-map {:foo "bar"
               :baz "qux"})
;(spec2/def ::calculated-set (->> enum-map keys (into #{})))

(spec2/def ::nilable-referenced-set (spec2/nilable enum-set))
(spec2/def ::nilable-calculated-set (spec2/nilable (->> enum-map keys (into #{}))))

(spec2/def ::nilable-referenced-set-kw (spec2/nilable ::referenced-set))
;(spec2/def ::nilable-calculated-set-kw (spec2/nilable ::calculated-set))

;(spec2/def ::unevaluatable-spec (letfn [(pred [x] (string? x))]
;                                  (spec2/spec pred)))

(defn even-number? [x]
  (and (int? x) (even? x)))
(sc2/def spec-coerce.spec-alpha2-test/even-number? sc/parse-long)
(spec2/def ::even-number even-number?)

(defn safe-form
  "Return the spec form or nil."
  [spec]
  (if (contains? (spec2/registry) spec)
    (spec2/form spec)))



(deftest test-coerce-from-registry
  (testing "it uses the registry to coerce a key"
    (is (= (sc2/coerce ::some-coercion "123") 123)))

  (testing "it returns original value when it a coercion can't be found"
    (is (= (sc2/coerce ::not-defined "123") "123")))

  (testing "go over nilables"
    (is (= (sc2/coerce ::infer-nilable "123") 123))
    (is (= (sc2/coerce ::infer-nilable "nil") nil))
    (is (= (sc2/coerce ::nilable-int "10") 10))
    (is (= (sc2/coerce ::nilable-pos-int "10") 10))

    (is (= (sc2/coerce ::nilable-string nil) nil))
    (is (= (sc2/coerce ::nilable-string 1) "1"))
    (is (= (sc2/coerce ::nilable-string "") ""))
    (is (= (sc2/coerce ::nilable-string "asdf") "asdf")))

  (testing "specs given as sets"
    (is (= (sc2/coerce ::int-set "1") 1))
    (is (= (sc2/coerce ::float-set "1.2") 1.2))
    (is (= (sc2/coerce ::boolean-set "true") true))
    ;;(is (= (sc/coerce ::symbol-set "foo/bar") 'foo/bar))
    (is (= (sc2/coerce ::string-set "hey") "hey"))
    (is (= (sc2/coerce ::keyword-set ":b") :b))
    (is (= (sc2/coerce ::keyword-set "b") :b))
    (is (= (sc2/coerce ::string-set :hey) "hey"))
    ;(is (= (sc2/coerce ::uuid-set "d6e73cc5-95bc-496a-951c-87f11af0d839") #uuid "d6e73cc5-95bc-496a-951c-87f11af0d839"))
    (is (= (sc2/coerce ::nil-set "nil") nil))
    ;;#?(:clj (is (= (sc/coerce ::uri-set "http://site.com") (URI. "http://site.com"))))
    #?(:clj (is (= (sc2/coerce ::decimal-set "42.42M") 42.42M)))

    (is (= (sc2/coerce ::referenced-set ":a") :a))

    ;; The following tests can't work without using `eval`. We will avoid this
    ;; and hope that spec2 will give us a better way.

    ;;(is (= (sc/coerce ::calculated-set ":foo") :foo))
    ;;(is (= (sc/coerce ::nilable-referenced-set ":a") :a))
    ;;(is (= (sc/coerce ::nilable-calculated-set ":foo") :foo))
    ;;(is (= (sc/coerce ::nilable-referenced-set-kw ":a") :a))
    ;;(is (= (sc/coerce ::nilable-calculated-set-kw ":foo") :foo))

    (is (= (sc2/coerce ::unevaluatable-spec "just a string") "just a string")))

  (testing "registering a coercion to a predicate symbol"
    (is (= 98 (sc2/coerce ::even-number "98")))))

(deftest test-coerce!
  (is (= (sc2/coerce! ::infer-int "123") 123))
  (is (= (sc2/coerce! :infer-int "123") "123"))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) #"Failed to coerce value" (sc2/coerce! ::infer-int "abc"))))

(deftest test-conform
  (is (= (sc2/conform ::or-example "true") [:bool true])))

(deftest test-coerce-from-predicates
  (are [predicate input output] (= (sc2/coerce predicate input) output)
       `number? "42" 42.0
       `number? "foo" "foo"
       `integer? "42" 42
       `int? "42" 42
       `int? 42.0 42
       `int? 42.5 42
       `pos-int? "42" 42
       `neg-int? "-42" -42
       `nat-int? "10" 10
       `even? "10" 10
       `odd? "9" 9
       `float? "42.42" 42.42
       `double? "42.42" 42.42
       `double? 42.42 42.42
       `double? 42 42.0
       `string? 42 "42"
       `string? :a "a"
       `string? :foo/bar ":foo/bar"
       `boolean? "true" true
       `boolean? "false" false
       `ident? ":foo/bar" :foo/bar
       `ident? "foo/bar" 'foo/bar
       `simple-ident? ":foo" :foo
       `qualified-ident? ":foo/baz" :foo/baz
       `keyword? "keyword" :keyword
       `keyword? ":keyword" :keyword
       `keyword? 'symbol :symbol
       `simple-keyword? ":simple-keyword" :simple-keyword
       `qualified-keyword? ":qualified/keyword" :qualified/keyword
       `symbol? "sym" 'sym
       `simple-symbol? "simple-sym" 'simple-sym
       `qualified-symbol? "qualified/sym" 'qualified/sym
       `uuid? "d6e73cc5-95bc-496a-951c-87f11af0d839" #uuid "d6e73cc5-95bc-496a-951c-87f11af0d839"
       `nil? "nil" nil
       `nil? "null" nil
       `false? "false" false
       `true? "true" true
       `zero? "0" 0

       `(spec2/coll-of int?) ["11" "31" "42"] [11 31 42]
       `(spec2/coll-of int?) ["11" "foo" "42"] [11 "foo" 42]

       `(spec2/map-of keyword? int?) {"foo" "42" "bar" "31"} {:foo 42 :bar 31}
       `(spec2/map-of keyword? int?) "foo" "foo"

       `(spec2/or :int int? :double double? :bool boolean?) "42" 42
       `(spec2/or :double double? :bool boolean?) "42.3" 42.3
       `(spec2/or :int int? :double double? :bool boolean?) "true" true

       #?@(:clj [`uri? "http://site.com" (URI. "http://site.com")])
       #?@(:clj [`decimal? "42.42" 42.42M
                 `decimal? "42.42M" 42.42M])))

(def test-gens
  {`inst? (spec2/gen (spec2/inst-in #inst "1980" #inst "9999"))})

#?(:cljs
   (defn ->js [var-name]
         (-> (str var-name)
             (str/replace #"/" ".")
             (str/replace #"-" "_")
             (str/replace #"\?" "_QMARK_")
             (js/eval))))

(defn safe-gen [s sp]
  (try
    (or (test-gens s) (spec2/gen sp))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(deftest test-coerce-generative
  (doseq [s (->> (methods sc2/sym->coercer)
                     (keys)
                     (filter symbol?))
          :let [sp #?(:clj @(resolve s)
                      :cljs (->js s))
                gen        (safe-gen s sp)]
          :when gen]
    (let [res (tc/quick-check 100
                              (prop/for-all [v gen]
                                (spec2/valid? (spec2/resolve-spec s) (sc2/coerce s (-> (pr-str v)
                                                                                           (str/replace #"^#[^\"]+\"|\"]?$"
                                                                                                        ""))))))]
      (if-not (= true (:result res))
        (throw (ex-info (str "Error coercing " s)
                        {:symbol s
                         :result res}))))))

(comment
  (let [s  'clojure.core/inst?
        sp (resolve s)
        ;gen (safe-fen s sp)
        v  (first user/sm)]

    (spec2/valid? (spec2/spec inst?) (sc2/coerce s (-> (pr-str v)
                                                       (str/replace #"^#[^\"]+\"|\"]?$" "")))))
  )

;#?(:clj (deftest test-coerce-inst
;          ;; use .getTime to avoid java.sql.Timestamp/java.util.Date differences
;          ;; we don't check s/valid? here, just that the date/time roundtrips
;          (are [input output] (= (.getTime (sc/coerce `inst? input))
;                                 (.getTime output))
;                              "9/28/2018 22:06" #inst "2018-09-28T22:06"
;                              (str "Fri Sep 28 22:06:52 "
;                                (.getID (java.util.TimeZone/getDefault))
;                                " 2018") #inst "2018-09-28T22:06:52"
;                              "2018-09-28" #inst "2018-09-28"
;                              "9/28/2018" #inst "2018-09-28")))

(deftest test-coerce-inference-test
  (are [keyword input output] (= (sc2/coerce keyword input) output)
       ::infer-int "123" 123
       ::infer-and-spec "42" 42
       ::infer-and-spec-indirect "43" 43
       ::infer-form ["20" "43"] [20 43]
       ::infer-form '("20" "43") '(20 43)
       ::infer-form (map str (range 2)) '(0 1)
       ::second-layer "41" 42
       ::second-layer-and "41" 42

       #?@(:clj [::infer-decimal? "123.4" 123.4M])
       #?@(:clj [::infer-decimal? 123.4 123.4M])
       #?@(:clj [::infer-decimal? 123.4M 123.4M])
       #?@(:clj [::infer-decimal? "" ""])
       #?@(:clj [::infer-decimal? [] []])))

(deftest test-coerce-structure
  (is (= (sc2/coerce-structure {::some-coercion "321"
                                ::not-defined   "bla"
                                :sub            {::infer-int "42"}})
         {::some-coercion 321
          ::not-defined   "bla"
          :sub            {::infer-int 42}}))
  (is (= (sc2/coerce-structure {::some-coercion "321"
                                ::not-defined   "bla"
                                :unqualified    "12"
                                :sub            {::infer-int "42"}}
                               {::sc2/overrides {::not-defined `keyword?
                                                 :unqualified  ::infer-int}})
         {::some-coercion 321
          ::not-defined   :bla
          :unqualified    12
          :sub            {::infer-int 42}}))
  (is (= (sc2/coerce-structure {::or-example "321"}
                               {::sc2/op sc2/conform})
         {::or-example [:int 321]})))

(spec2/def ::bool boolean?)
(spec2/def ::simple-keys (spec2/keys :req [::infer-int]
                                     :opt [::bool]))
(spec2/def ::nested-keys (spec2/keys :req [::infer-form ::simple-keys]
                                     :req-un [::bool]))

(deftest test-coerce-keys
  (is (= {::infer-int 123}
         (sc2/coerce ::simple-keys {::infer-int "123"})))
  (is (= {::infer-form  [1 2 3]
          ::simple-keys {::infer-int 456
                         ::bool      true}
          :bool         true}
         (sc2/coerce ::nested-keys {::infer-form  ["1" "2" "3"]
                                    ::simple-keys {::infer-int "456"
                                                   ::bool      "true"}
                                    :bool         "true"})))
  (is (= "garbage" (sc2/coerce ::simple-keys "garbage"))))

(spec2/def ::simple-schema (spec2/schema [::infer-int ::bool]))
(spec2/def ::nested-schema (spec2/schema [::infer-form ::simple-schema ::bool]))
(spec2/def ::unnamespaced-schema (spec2/schema {:i ::infer-int
                                                :nested (spec2/schema {:b ::bool})
                                                :nested-enum #{:foo :bar}}))

(deftest test-coerce-schema
  (is (= {::infer-int 123}
         (sc2/coerce ::simple-schema {::infer-int "123"})))
  (is (= {::infer-form  [1 2 3]
          ::simple-keys {::infer-int 456
                         ::bool      true}
          :bool         true}
         (sc2/coerce ::nested-keys {::infer-form  ["1" "2" "3"]
                                    ::simple-keys {::infer-int "456"
                                                   ::bool      "true"}
                                    :bool         "true"})))
  (is (= "garbage" (sc2/coerce ::simple-keys "garbage")))
  (is (= {:i 123
          :nested {:b true}
          :nested-enum :bar}
         (sc2/coerce ::unnamespaced-schema {:i "123"
                                            :nested {:b  "true"}
                                            :nested-enum "bar"}))))

(spec2/def ::head double?)
(spec2/def ::body int?)
(spec2/def ::arm int?)
(spec2/def ::leg double?)
(spec2/def ::arms (spec2/coll-of ::arm))
(spec2/def ::legs (spec2/coll-of ::leg))
(spec2/def ::name string?)
(spec2/def ::animal (spec2/keys :req [::head ::body ::arms ::legs]
                                :opt-un [::name ::id]))

(deftest test-coerce-with-registry-overrides
  (testing "it uses overrides when provided"
    (is (= {::head 1 ::body 16 ::arms [4 4] ::legs [7 7] :name :john}
           (binding [sc2/*overrides* {::head (sc2/sym->coercer `int?)
                                      ::leg  (sc2/sym->coercer `int?)
                                      ::name (sc2/sym->coercer `keyword?)}]
             (sc2/coerce ::animal {::head "1"
                                   ::body "16"
                                   ::arms ["4" "4"]
                                   ::legs ["7" "7"]
                                   :name  "john"}))))
    (is (= {::head 1
            ::body 16
            ::arms [4 4]
            ::legs [7 7]
            :name  :john}
           (sc2/coerce ::animal
                       {::head "1"
                        ::body "16"
                        ::arms ["4" "4"]
                        ::legs ["7" "7"]
                        :name  "john"}
                       {::sc2/overrides
                        {::head (sc2/sym->coercer `int?)
                         ::leg  (sc2/sym->coercer `int?)
                         ::name (sc2/sym->coercer `keyword?)}})))
    "Coerce with option form"))

(spec2/def ::foo int?)
(spec2/def ::bar string?)
(spec2/def ::qualified (spec2/keys :req [(or ::foo ::bar)]))
(spec2/def ::unqualified (spec2/keys :req-un [(or ::foo ::bar)]))

(deftest test-or-conditions-in-qualified-keys
  (is (= (sc2/coerce ::qualified {::foo "1" ::bar "hi"})
         {::foo 1 ::bar "hi"})))

(deftest test-or-conditions-in-unqualified-keys
  (is (= (sc2/coerce ::unqualified {:foo "1" :bar "hi"})
         {:foo 1 :bar "hi"})))

(spec2/def ::tuple (spec2/tuple ::foo ::bar int?))

(deftest test-tuple
  (is (= [0 "" 1] (sc2/coerce ::tuple ["0" nil "1"])))
  (is (= "garbage" (sc2/coerce ::tuple "garbage"))))

(deftest test-merge
  (spec2/def ::merge (spec2/merge (spec2/keys :req-un [::foo])
                                  ::unqualified
                                  ;; TODO: add s/multi-spec test
                                  ))
  (is (= {:foo 1 :bar "1" :c {:a 2}}
         (sc2/coerce ::merge {:foo "1" :bar 1 :c {:a 2}}))
      "Coerce new vals appropriately")
  (is (= {:foo 1 :bar "1" :c {:a 2}}
         (sc2/coerce ::merge {:foo 1 :bar "1" :c {:a 2}}))
      "Leave out ok vals")

  (is (= "garbage" (sc2/coerce ::merge "garbage"))
      "garbage is passthrough"))

(def d :kw)
(defmulti multi #'d)
(defmethod multi :default [_] (spec2/keys :req-un [::foo]))
(defmethod multi :kw [_] ::unqualified)
(spec2/def ::multi (spec2/multi-spec multi :hit))

(deftest test-multi-spec
  (is (= {:not "foo"} (sc2/coerce ::multi {:not "foo"})))
  (is (= {:foo 1} (sc2/coerce ::multi {:foo 1})))
  (is (= {:foo 1} (sc2/coerce ::multi {:foo "1"})))
  (is (= {:foo 1 :d :kw} (sc2/coerce ::multi {:d :kw :foo "1"})))
  (is (= "garbage" (sc2/coerce ::multi "garbage"))))

;:   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.emitter.jvm.emit
  (:refer-clojure :exclude [cast])
  (:require [clojure.tools.analyzer.utils :as u]
            [clojure.tools.analyzer.jvm.utils :refer [primitive? numeric? box prim-or-obj] :as j.u]
            [clojure.string :as s]
            [clojure.tools.emitter.jvm.transform :as t]
            [clojure.tools.emitter.jvm.intrinsics :refer [intrinsic intrinsic-predicate]]))

(defmulti -emit (fn [{:keys [op]} _] op))
(defmulti -emit-set! (fn [{:keys [op]} _] op))

(def nil-expr
  {:op :const :type :nil :form nil})

(defn emit-box [tag box]
  (if (and (primitive? tag)
           (not (primitive? box)))
    (cond
     (numeric? tag)
     [[:invoke-static [:clojure.lang.RT/box tag] :java.lang.Number]
      #_[:check-cast box]]
     (= Character box) ;;only char->Character
     [[:invoke-static [:clojure.lang.RT/box :char] :java.lang.Character]]
     (= Boolean box) ;; only bool->Boolean
     [[:invoke-static [:clojure.lang.RT/box :boolean] :java.lang.Object]
      [:check-cast :java.lang.Boolean]])
    (when (primitive? box)
      (let [method (str (.getName ^Class box) "Cast")
            tag (prim-or-obj tag)
            method-sig (str (.getMethod clojure.lang.RT method (into-array Class [tag])))]
        (if-let [ops (intrinsic method-sig)]
          (mapv (fn [op] [:insn op]) ops)
          [[:invoke-static [(keyword "clojure.lang.RT" method) tag] box]])))))

(defn emit-cast [tag cast]
  (if (not (or (primitive? tag)
             (primitive? cast)))
    (when-not (#{Void Void/TYPE} cast)
      [[:check-cast cast]])
    (emit-box tag cast)))

(defn emit
  ([ast]
     (emit ast {}))
  ([{:keys [env ret-tag tag bind-tag] :as ast} frame]
     (let [bytecode (-emit ast frame)
           statement? (= :statement (:context env))
           m (meta bytecode)
           ret-tag (or ret-tag bind-tag)]
       (if statement?
         (if (:const m)
           []
           (into bytecode
                 (when (and (not (:untyped m))
                            (not (:container m))
                            (not= Void/TYPE tag))
                   (if (#{Double/TYPE Long/TYPE} ret-tag)
                     [[:pop2]]
                     [[:pop]]))))
         (into bytecode
               `[~@(when (= :untyped m)
                     [[:insn :ACONST_NULL]])
                 ~@(when (and tag ret-tag
                              (not= tag ret-tag))
                     (emit-cast ret-tag tag))])))))

(defmethod -emit :import
  [{:keys [class]} frame]
  [[:get-static :clojure.lang.RT/CURRENT_NS :clojure.lang.Var]
   [:invoke-virtual [:clojure.lang.Var/deref] :java.lang.Object]
   [:check-cast :clojure.lang.Namespace]
   [:push (.getName ^Class class)]
   [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]
   [:invoke-virtual [:clojure.lang.Namespace/importClass :java.lang.Class] :java.lang.Class]])

(defmethod -emit :throw
  [{:keys [exception]} frame]
  `^:untyped
  [~@(emit (assoc exception :tag :java.lang.Throwable) frame)
   [:throw-exception]])

(defmethod -emit :monitor-enter
  [{:keys [target]} frame]
  `^:untyped
  [~@(emit target frame)
   [:monitor-enter]])

(defmethod -emit :monitor-exit
  [{:keys [target]} frame]
  `^:untyped
  [~@(emit target frame)
   [:monitor-exit]])

(defn cast [to el]
  (if (numeric? to)
    (condp = (box to)
      Integer
      (.intValue ^Number el)

      Long
      (.longValue ^Number el)

      Double
      (.doubleValue ^Number el)

      Float
      (.floatValue ^Number el)

      Short
      (.shortValue ^Number el)

      Byte
      (.byteValue ^Number el))
    (clojure.core/cast to el)))

(defn emit-constant
  [const frame c-tag]
  (let [{:keys [id tag]} (get-in frame [:constants const])]
    ^:const
    [(case const
       (true false)
       [:get-static (if const :java.lang.Boolean/TRUE :java.lang.Boolean/FALSE)
        :java.lang.Boolean]

       nil
       [:insn :ACONST_NULL]

       (if (or (primitive? c-tag)
               (string? const))
         [:push (cast (or (box c-tag) (class const)) const)]
         [:get-static (frame :class) (str "const__" id) tag]))]))

(defmethod -emit :const
  [{:keys [form tag]} frame]
  (emit-constant form frame tag))

(defmethod -emit :quote
  [{:keys [expr]} frame]
  (-emit expr frame))

(defn emit-var [var frame]
  (emit-constant var frame clojure.lang.Var))

(defmethod -emit :var
  [{:keys [var]} frame]
  (conj
   (emit-var var frame)
   [:invoke-virtual [(if (u/dynamic? var)
                       :clojure.lang.Var/get
                       :clojure.lang.Var/getRawRoot)] :java.lang.Object]))

(defmethod -emit-set! :var
  [{:keys [target val]} frame]
  `[~@(emit-var (:var target) frame)
    ~@(emit val frame)
    [:invoke-virtual [:clojure.lang.Var/set :java.lang.Object] :java.lang.Object]])

(defmethod -emit :the-var
  [{:keys [var]} frame]
  (emit-var var frame))

(defmethod -emit :def
  [{:keys [var meta init env]} frame]
  `[~@(emit-var var frame)
    ~@(when (u/dynamic? var) ;; why not when macro?
        [[:push true]
         [:invoke-virtual [:clojure.lang.Var/setDynamic :boolean] :clojure.lang.Var]])
    ~@(when meta
        `[[:dup]
          ~@(emit (assoc meta :tag :clojure.lang.IPersistentMap) frame)
          [:invoke-virtual [:clojure.lang.Var/setMeta :clojure.lang.IPersistentMap] :void]])
    ~@(when init
        `[[:dup]
          ~@(emit init frame)
          [:invoke-virtual [:clojure.lang.Var/bindRoot :java.lang.Object] :void]])])

(defmethod -emit :set!
  [ast frame]
  (-emit-set! ast frame))

(defn emit-as-array [list frame]
  `[[:push ~(int (count list))]
    [:new-array :java.lang.Object]
    ~@(mapcat (fn [i item]
                `[[:dup]
                  [:push ~(int i)]
                  ~@(emit item frame)
                  [:array-store :java.lang.Object]])
              (range) list)])

(defmethod -emit :map
  [{:keys [keys vals]} frame]
  (conj
   (emit-as-array (interleave keys vals) frame)
   [:invoke-static [:clojure.lang.RT/mapUniqueKeys :objects] :clojure.lang.IPersistentMap]))

(defmethod -emit :vector
  [{:keys [items]} frame]
  (conj
   (emit-as-array items frame)
   [:invoke-static [:clojure.lang.RT/vector :objects] :clojure.lang.IPersistentVector]))

(defmethod -emit :set
  [{:keys [items]} frame]
  (conj
   (emit-as-array items frame)
   [:invoke-static [:clojure.lang.RT/set :objects] :clojure.lang.IPersistentSet]))

(defmethod -emit :with-meta
  [{:keys [meta expr]} frame]
  (into
   (emit (assoc expr :tag :clojure.lang.IObj) frame)
   `[~@(emit (assoc meta :tag :clojure.lang.IPersistentMap) frame)
     [:invoke-interface [:clojure.lang.IObj/withMeta :clojure.lang.IPersistentMap]
      :clojure.lang.IObj]]))

(defmethod -emit :do
  [{:keys [statements ret]} frame]
  (with-meta
    (vec (mapcat #(emit % frame) (conj statements ret)))
    {:container true}))

(defn label []
  (keyword (gensym "label__")))

(defn local []
  (keyword (gensym "local__")))

(defmethod -emit :try
  [{:keys [body catches finally env]} frame]
  (let [[start-label end-label ret-label finally-label] (repeatedly label)
        catches (mapv #(assoc %
                         :start-label (label)
                         :end-label (label)) catches)
        context (:context env)
        [ret-local finally-local] (repeatedly local)]

    `^:container
    [[:mark ~start-label]
     ~@(emit body frame)
     ~@(when (not= :statement context)
         [[:astore ret-local]])
     [:mark ~end-label]
     ~@(when finally
         (emit finally frame))
     [:go-to ~ret-label]

     ~@(mapcat
        (fn [{:keys [body start-label end-label local]}]
          `[[:mark ~start-label]
            [:astore ~(:name local)]
            ~@(emit body frame)
            ~@(when (not= :statement context)
                [[:astore ret-local]])
            [:mark ~end-label]
            ~@(when finally
                (emit finally frame))
            [:go-to ~ret-label]])
        catches)
     ~@(when finally
         `[[:mark ~finally-label]
           [:astore ~finally-local]
           ~@(emit finally frame)
           [:aload ~finally-local]
           [:throw-exception]])

     [:mark ~ret-label]
     ~@(when (not= :statement context)
         `[[:aload ~ret-local]])
     [:mark ~(label)]

     ~@(for [{:keys [^Class class] :as c} catches]
         [:try-catch-block start-label end-label (:start-label c) class])

     ~@(when finally
         `[~[:try-catch-block start-label end-label finally-label nil]
           ~@(for [{:keys [start-label end-label] :as c} catches]
               [:try-catch-block start-label end-label finally-label nil])])

     ~@(for [{:keys [local start-label end-label] :as c} catches]
         [:local-variable (:name local) ; or :form?
          :objects nil start-label end-label (:name local)])])) ;; generate idx based on name

(defn emit-line-number
  [{:keys [line]} & [l]]
  (when line
    (let [l (or l (label))]
      [[:mark l]
       [:line-number line l]])))

(defmethod -emit :static-field
  [{:keys [field ret-tag class env]} frame]
  `^:const
  [~@(emit-line-number env)
   ~[:get-static class field ret-tag]])

(defn dup [tag]
  (if (#{Long/TYPE Double/TYPE} tag)
    [:dup2]
    [:dup]))

(defn dup-x1 [tag]
  (if (#{Long/TYPE Double/TYPE} tag)
    [:dup2-x1]
    [:dup-x1]))

(defn dup-x2 [tag]
  (if (#{Long/TYPE Double/TYPE} tag)
    [:dup2-x2]
    [:dup-x2]))

(defmethod -emit-set! :static-field
  [{:keys [target val env]} frame]
  (let [{:keys [ret-tag class field]} target]
   `[~@(emit-line-number env)
     ~@(emit (assoc val :tag ret-tag) frame)
     ~(dup ret-tag)
     ~[:put-static class field ret-tag]]))

(defmethod -emit :instance-field
  [{:keys [instance class field env ret-tag]} frame]
  `^:const
  [~@(emit-line-number env)
   ~@(emit (assoc instance :tag class) frame)
   ~[:get-field class field ret-tag]])

(defmethod -emit-set! :instance-field
  [{:keys [target val env]} frame]
  (let [{:keys [instance class field ret-tag]} target]
   `[~@(emit-line-number env)
     ~@(emit (assoc instance :tag class) frame)
     ~@(emit (assoc val :tag ret-tag) frame)
     ~(dup-x1 ret-tag)
     ~[:put-field class field ret-tag]]))

(defmethod -emit :keyword-invoke
  [{:keys [env fn args] :as ast} frame]
  (let [id (:id fn)
        [end-label fault-label] (repeatedly label)]
    `[~@(emit-line-number env)
      [:get-static ~(name (frame :class)) ~(str "thunk__" id) :clojure.lang.ILookupThunk]
      [:dup]
      ~@(emit fn frame)
      [:dup-x2]
      [:invoke-interface [:clojure.lang.ILookupThunk/get :java.lang.Object] :java.lang.Object]
      [:dup-x2]
      [:jump-insn :IF_ACMPEQ ~fault-label]
      [:pop]
      [:go-to ~end-label]

      [:mark ~fault-label]
      [:swap]
      [:pop]
      [:dup]
      [:get-static ~(name (frame :class)) ~(str "site__" id) :clojure.lang.KeywordLookupSite]
      [:swap]
      [:invoke-interface [:clojure.lang.ILookupThunk/fault :java.lang.Object] :java.lang.Object]
      [:dup]
      [:put-static ~(name (frame :class)) ~(str "thunk__" id) :clojure.lang.ILookupThunk]
      [:swap]
      [:invoke-interface [:clojure.lang.ILookupThunk/get :java.lang.Object] :java.lang.Object]
      [:mark ~end-label]]))

(defmethod -emit :new
  [{:keys [env ^Class class args validated? tag]} frame]
  (if validated?
    `[[:new-instance ~class]
      [:dup]
      ~@(mapcat #(emit % frame) args)
      [:invoke-constructor [~(keyword (.getName class) "<init>")
                            ~@(mapv :tag args)] :void]]
    `[[:push ~(.getName class)]
      [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]
      ~@(emit-as-array args frame)
      [:invoke-static [:clojure.lang.Reflector/invokeCostructor :java.lang.Class :objects] :java.lang.Object]]))

(defn emit-intrinsic [{:keys [args method ^Class class false-label]}]
  (let [args (mapv (fn [{:keys [cast tag]}] (or cast tag)) args)
        m    (str (.getMethod class (name method) (into-array Class args)))]
    (if false-label
      (when-let [ops (intrinsic-predicate m)]
        (conj (mapv (fn [op] [:insn op]) (butlast ops))
              [:jump-insn (last ops) false-label]))
      (when-let [ops (intrinsic m)]
        (mapv (fn [op] [:insn op]) ops)))))

(defmethod -emit :static-call
  [{:keys [env ret-tag validated? args method ^Class class false-label] :as ast} frame]
  (if validated?
    `[~@(emit-line-number env)
      ~@(mapcat #(emit % frame) args)
      ~@(or
         (emit-intrinsic ast)
         `[[:invoke-static [~(keyword (.getName class) (str method))
                            ~@(mapv :tag args)] ~ret-tag]])]
    `[[:push ~(.getName class)]
      [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]
      [:push ~(str method)]
      ~@(emit-as-array args frame)
      [:invoke-static [:clojure.lang.Reflector/invokeStaticMethod
                       :java.lang.Class :java.lang.String :objects]
       :java.lang.Object]]))

(defmethod -emit :instance-call
  [{:keys [env ret-tag validated? args method ^Class class instance]} frame]
  (if validated?
    `[~@(emit-line-number env)
      ~@(emit (assoc instance :tag class) frame)
      ~@(mapcat #(emit % frame) args)
      [~(if (.isInterface class)
          :invoke-interface
          :invoke-virtual)
       [~(keyword (.getName class) (str method)) ~@(mapv :tag args)] ~ret-tag]]
    `[~@(emit instance frame)
      [:push ~(str method)]
      ~@(emit-as-array args frame)
      [:invoke-static [:clojure.lang.Reflector/invokeInstanceMethod
                       :java.lang.Object :java.lang.String :objects]
       :java.lang.Object]]))

(defmethod -emit :host-interop
  [{:keys [m-or-f target env]} frame]
  `[~@(emit target frame)
    [:push ~(str m-or-f)]
    [:invoke-static [:clojure.lang.Reflector/invokeNoArgInstanceMember :java.lang.Object :java.lang.String] :Object]])

(defmethod -emit-set! :host-interop
  [{:keys [target val env]} frame]
  `[~@(emit-line-number env)
    ~@(emit (:target target) frame)
    [:push ~(str (:m-or-f target))]
    ~@(emit val frame)
    [:invoke-static [:clojure.lang.Reflector/setInstanceField :java.lang.Object :java.lang.String :java.lang.Object] :java.lang.Object]])

(defmethod -emit :instance?
  [{:keys [target class]} frame]
  `[~@(emit target frame)
    ~[:instance-of class]])

(defmethod -emit :if
  [{:keys [test then else env]} frame]
  (let [[null-label false-label end-label] (repeatedly label)
        test-expr (emit (assoc test :false-label false-label) frame)]
    `^:container
    [~@(emit-line-number env)
     ~@test-expr
     ~@(when (not (and (= :static-call (:op test))
                     (= :jump-insn (first (last test-expr)))))
         (if (not= (:tag test) Boolean/TYPE)
           [[:dup]
            [:if-null null-label]
            [:get-static :java.lang.Boolean/FALSE :java.lang.Boolean]
            [:jump-insn :IF_ACMPEQ false-label]]
           [[:if-z-cmp :EQ false-label]]))
     ~@(emit then frame)
     [:go-to ~end-label]
     [:mark ~null-label]
     [:pop]
     [:mark ~false-label]
     ~@(emit else frame)
     [:mark ~end-label]]))

(defn emit-args-and-invoke
  ([args frame] (emit-args-and-invoke args frame false))
  ([args frame proto?]
     `[[:check-cast :clojure.lang.IFn]
       ~@(mapcat #(emit % frame) (take 20 args))
       ~@(when-let [args (seq (drop 20 args))]
           (emit-as-array args frame))
       [:invoke-interface [:clojure.lang.IFn/invoke ~@(repeat (min 21 (count args)) :java.lang.Object) ~@(when proto? [:java.lang.Object])] :java.lang.Object]]))

(defmethod -emit :invoke
  [{:keys [fn args env]} frame]
  `[~@(emit fn frame)
    ~@(emit-args-and-invoke args frame)])

(defmethod -emit :protocol-invoke
  [{:keys [fn args env]} frame]
  (let [[on-label call-label end-label] (repeatedly label)
          v (:var fn)
          [target & args] args
          id (:id fn)

          ^Class pinterface (:on-interface @(:protocol (meta v)))]
      `[~@(emit target frame)
        [:dup]
        [:invoke-static [:clojure.lang.Util/classOf :java.lang.Object] :java.lang.Class]

        [:load-this]
        [:get-field ~(name (frame :class)) ~(str "cached__class__" id) :java.lang.Class]
        [:jump-insn :IF_ACMPEQ ~call-label]

        [:dup]
        [:instance-of ~pinterface]
        [:if-z-cmp :NE ~on-label]

        [:dup]
        [:invoke-static [:clojure.lang.Util/classOf :java.lang.Object] :java.lang.Class]
        [:load-this]
        [:swap]
        [:put-field ~(frame :class) ~(str "cached__class__" id) :java.lang.Class]

        [:mark ~call-label]
        ~@(emit-var v frame)
        [:invoke-virtual [:clojure.lang.Var/getRawRoot] :java.lang.Object]
        [:swap]
        ~@(emit-args-and-invoke args frame true)
        [:go-to ~end-label]

        [:mark ~on-label]

        ~@(mapcat #(emit % frame) args)
        [:invoke-interface [~(keyword (.getName pinterface)
                                      (munge (str (:name fn))))
                            ~@(repeat (count args) :java.lang.Object)] :java.lang.Object]

        [:mark ~end-label]]))

(defmethod -emit :prim-invoke
  [{:keys [fn args ^Class prim-interface arg-tags ret-tag]} frame]
  `[~@(emit fn frame)
    [:check-cast ~prim-interface]
    ~@(mapcat #(emit % frame) args)
    [:invoke-interface [~(keyword (.getName prim-interface) "invokePrim")
                        ~@arg-tags] ~ret-tag]])

(defn emit-shift-mask
  [{:keys [shift mask]}]
  (when (not (zero? mask))
    [[:push (int shift)]
     [:insn :ISHR]
     [:push (int mask)]
     [:insn :IAND]]))

(defn emit-test-ints
  [{:keys [test test-type] :as ast} frame default-label]

  (cond
   (nil? (:tag test))
   ;; reflection warning
   `[~@(emit test frame)
     [:instance-of :java.lang.Number]
     [:if-z-cmp :EQ ~default-label]
     ~@(emit (assoc test :tag Integer/TYPE) frame)
     ~@(emit-shift-mask ast)]

   (numeric? (:tag test))
   `[~@(emit (assoc test :tag Integer/TYPE) frame)
     ~@(emit-shift-mask ast)]

   :else
   [[:go-to default-label]]))

(defn emit-test-hashes
  [{:keys [test] :as ast} frame]
  `[~@(emit test frame)
    [:invoke-static [:clojure.lang.Util/hash :java.lang.Object] :int]
    ~@(emit-shift-mask ast)])

(defn emit-then-ints
  [tag comp test then default-label mask frame]
  (cond
   (nil? tag)
   `[~@(emit comp frame)
     ~@(emit test frame)
     [:invoke-static [:clojure.lang.Util/equiv :java.lang.Object :java.lang.Object] :boolean]
     [:if-z-cmp :EQ ~default-label]
     ~@(emit then frame)]

   (= tag Long)
   `[~@(emit (assoc test :tag Long/TYPE) frame)
     ~@(emit (assoc comp :tag Long/TYPE) frame)
     [:if-cmp :long :NE ~default-label]
     ~@(emit then frame)]

   (numeric? tag)
   `[~@(when (not (zero? mask))
         `[~@(emit (assoc test :tag Long/TYPE) frame)
           ~@(emit (assoc comp :tag Long/TYPE) frame)
           [:if-cmp :long :NE ~default-label]])
     ~@(emit then frame)]

   :else
   [[:go-to default-label]]))

(defn emit-then-hashes
  [comp test then test-type default-label frame]
  `[~@(emit comp frame)
    ~@(emit test frame)
    ~@(if (= :hash-identity test-type)
        [[:jump-insn :IF_ACMPNE default-label]]
        [[:invoke-static [:clojure.lang.Util/equiv :java.lang.Object :java.lang.Object] :boolean]
         [:if-z-cmp :EQ default-label]])
    ~@(emit then frame)])

(defmethod -emit :case
  [{:keys [test default tests thens shift mask low high switch-type test-type skip-check? env] :as ast} frame]
  (let [testc (count tests)
        tests (zipmap (mapv :hash tests) (mapv :test tests))
        thens (apply sorted-map (mapcat (juxt :hash :then) thens))
        [default-label end-label] (repeatedly label)
        labels (zipmap (keys tests) (repeatedly label))]
    `^:container
    [~@(emit-line-number env)
     ~@(if (= :int test-type)
         (emit-test-ints ast frame default-label)
         (emit-test-hashes ast frame))
     ~(if (= :sparse switch-type)
        [:lookup-switch-insn default-label (keys tests) (vals labels)] ; to array
        [:table-switch-insn low high default-label
         (mapv (fn [i] (if (contains? labels i) (labels i) default-label)) (range low (inc high)))])
     ~@(mapcat (fn [[i label]]
                 `[[:mark ~label]
                   ~@(cond
                      (= :int test-type)
                      (emit-then-ints (:tag test) test (tests i) (thens i) default-label mask frame)

                      (contains? skip-check? i)
                      [(emit (thens i) frame)]

                      :else
                      (emit-then-hashes test (tests i) (thens i) test-type default-label frame))
                   [:go-to ~end-label]])
               labels)
     [:mark ~default-label]
     ~@(emit default frame)
     [:mark ~end-label]]))

(defn emit-bindings [bindings labels frame]
  (mapcat (fn [{:keys [init tag name] :as binding} label]
            `[~@(emit init frame)
              [:var-insn ~(keyword (if tag (.getName ^Class tag)
                                       "java.lang.Object") "ISTORE")
               ~name]
              ~@(when label
                  [[:mark label]])])
          bindings labels))

(defn emit-let
  [{:keys [op bindings body env]} frame]
  (let [loop? (= :loop op)
        [end-label loop-label & labels] (repeatedly (+ 2 (count bindings)) label)]
    `^:container
    [~@(emit-bindings bindings labels frame)
     [:mark ~loop-label]
     ~@(emit body (merge frame (when loop? {:loop-label loop-label
                                            :loop-locals bindings})))
     [:mark ~end-label]
     ~@(mapv (fn [{:keys [name tag]} label]
               [:local-variable name (or tag :java.lang.Object) nil label end-label name])
             bindings labels)]))

(defmethod -emit :let
  [ast frame]
  (emit-let ast frame))

(defmethod -emit :loop
  [ast frame]
  (emit-let ast frame))

(defn emit-letfn-bindings [bindings  class-names frame]
  (let [binds (set (mapv :name bindings))]
    (mapcat (fn [{:keys [init tag name]} class-name]
              (let [{:keys [closed-overs]} init]
                `[[:var-insn ~(keyword (if tag (.getName ^Class tag)
                                           "java.lang.Object") "ILOAD") ~name]
                  [:check-cast ~class-name]

                  ~@(mapcat (fn [[k c]]
                              (when (binds c)
                                `[~@(emit (assoc c :op :local)
                                          (assoc frame :closed-overs closed-overs))
                                  ~[:put-field class-name k (:tag c)]]))
                            closed-overs)

                  [:pop]]))
            bindings class-names)))


(defn emit-binds [bindings frame]
  (mapv
   (fn [{:keys [init tag name] :as binding}]
     (let [init (emit init frame)
           class-name (.getName ^Class (:class (meta init)))]
       `[~class-name
         [~@init
          [:var-insn ~(keyword (if tag (.getName ^Class tag)
                                   "java.lang.Object") "ISTORE")
           ~name]]]))
   bindings))

(defmethod -emit :letfn
  [{:keys [bindings body env]} frame]
  (let [[loop-label end-label] (repeatedly label)]
    `^:container
    [~@(emit-bindings (mapv #(assoc % :init nil-expr) bindings) (repeat nil) frame)

     ~@(let [binds (emit-binds bindings frame)
             bindings-emit(mapcat second binds)
             class-names (mapv first binds)]
         `[~@bindings-emit
           ~@(emit-letfn-bindings bindings class-names frame)])

     [:mark ~loop-label]
     ~@(emit body frame)
     [:mark ~end-label]
     ~@(mapv (fn [{:keys [name tag]}]
               [:local-variable name (or tag :java.lang.Object) nil loop-label end-label name])
             bindings)]))

(defmethod -emit :recur
  [{:keys [exprs]} {:keys [loop-label loop-locals] :as frame}]
  `[~@(mapcat (fn [{:keys [local name tag] :as arg} binding]
                `[~@(emit arg frame)
                  ~(if (= :arg local)
                     [:store-arg (:arg-id binding)]
                     [:var-insn ~(keyword (if tag (.getName ^Class tag)
                                              "java.lang.Object") "ISTORE")
                      ~name])]) exprs loop-locals)
    [:go-to ~loop-label]])

(defmethod -emit :fn-method
  [{:keys [params tag fixed-arity variadic? body env]}
   {:keys [class] :as frame}]
  (let [arg-tags               (mapv (comp prim-or-obj :tag) params)
        return-type            (prim-or-obj tag)
        tags                   (conj arg-tags return-type)
        prim-interface         (j.u/prim-interface tags)

        primitive?             (some primitive? tags)

        method-name            (cond
                                variadic? :doInvoke
                                primitive? :invokePrim
                                :else
                                :invoke)

        ;; arg-types
        [loop-label end-label] (repeatedly label)

        code
        `[[:start-method]
          [:local-variable :this :clojure.lang.AFunction nil ~loop-label ~end-label :this]
          ~@(mapv (fn [{:keys [name tag]}]
                    [:local-variable name tag nil loop-label end-label name])
                  params)
          [:mark ~loop-label]
          ~@(emit-line-number env loop-label)
          ~@(emit body (assoc frame
                         :loop-label  loop-label
                         :loop-locals params))
          [:mark ~end-label]
          [:return-value]
          [:end-method]]]

    ;; should emit typed only when there's an interface, otherwise it's useless

      `[~{:op     :method
          :attr   #{:public}
          :method [(into [method-name] arg-tags) return-type]
          :code   code}
        ~@(when primitive?
            [{:op        :method
              :attr      #{:public}
              :interface prim-interface
              :method    [(into [:invoke] (repeat (count params) :java.lang.Object))
                          :java.lang.Object]
              :code      `[[:start-method]
                           [:load-this]
                           ~@(mapcat (fn [{:keys [tag]} id]
                                       `[~[:load-arg id]
                                         ~@(emit-cast Object tag)])
                                     params (range))
                           ~[:invoke-virtual (into [(keyword class "invokePrim")] arg-tags) return-type]
                           ~@(emit-cast return-type Object)
                           [:return-value]
                           [:end-method]]}])]))

;; addAnnotations
(defmethod -emit :method
  [{:keys [this  methods args name bridges tag fixed-arity variadic? body env]}
   {:keys [class] :as frame}]

  (let [method-name name
        return-type tag
        arg-types (mapv :tag args)
        [loop-label end-label] (repeatedly label)

        code
        `[[:start-method]
          ~[:local-variable (:name this) class nil loop-label end-label (:name this)]
          ~@(mapv (fn [{:keys [tag name]}]
                    [:local-variable name tag nil loop-label end-label name])
                  args)
          [:mark ~loop-label]
          ~@(emit-line-number env loop-label)
          ~@(emit (assoc body
                    :tag return-type
                    :ret-tag (or (:tag body) Object))
                  (assoc frame
                    :loop-label  loop-label
                    :loop-locals args))
          [:mark ~end-label]
          [:return-value]
          [:end-method]]]

    `[~{:op     :method
        :attr   #{:public}
        :method [(into [method-name] arg-types) return-type]
        :code   code}
      ~@(let [target [(into [(keyword class (str method-name))] arg-types) return-type]]
         (for [{:keys [name parameter-types return-type]} bridges]
           {:op :method
            :attr #{:public :bridge}
            :method [(into [method-name] parameter-types) return-type]
            :code `[[:start-method]
                    [:load-this]
                    [:load-args]
                    [:invoke-virtual ~@target]
                    [:return-value]
                    [:end-method]]}))]))

(defmethod -emit :local
  [{:keys [to-clear? local name tag bind-tag arg-id]}
   {:keys [closed-overs class] :as frame}]
  (let [to-clear? (and to-clear?
                       (not (primitive? bind-tag)))]
    (cond

     (closed-overs name)
     `[[:load-this]
       ~[:get-field class name bind-tag]
       ~@(when to-clear?
           [[:load-this]
            [:insn :ACONST_NULL]
            [:put-field class name bind-tag]])]

     (= :arg local)
     `[[:load-arg ~arg-id]
       ~@(when to-clear?
           [[:insn :ACONST_NULL]
            [:store-arg arg-id]])]

     (= :fn local)
     [[:var-insn :clojure.lang.AFunction/ILOAD 0]]

     (= :this local)
     [[:var-insn :clojure.lang.Object/ILOAD 0]]

     :else
     `[~[:var-insn (keyword (if bind-tag (.getName ^Class bind-tag)
                                "java.lang.Object") "ILOAD") name]
       ~@(when to-clear?
           [[:insn :ACONST_NULL]
            [:var-insn (keyword (if bind-tag (.getName ^Class bind-tag)
                                    "java.lang.Object") "ISTORE") name]])])))

(defmulti emit-value (fn [type value] type))

(defmethod emit-value :nil [_ _]
  [[:insn :ACONST_NULL]])

(defmethod emit-value :string [_ s]
  [[:push s]])

(defmethod emit-value :bool [_ b]
  [[:get-static (if b :java.lang.Boolean/TRUE :java.lang.Boolean/FALSE)
    :java.lang.Boolean]])

(defmethod emit-value :number [_ n]
  [[:push n]
   (cond
    (instance? Long n)
    [:invoke-static [:java.lang.Long/valueOf :long] :java.lang.Long]

    (instance? Integer n)
    [:invoke-static [:java.lang.Integer/valueOf :int] :java.lang.Integer]

    (instance? Double n)
    [:invoke-static [:java.lang.Double/valueOf :double] :java.lang.Double]

    (instance? Float n)
    [:invoke-static [:java.lang.Float/valueOf :float] :java.lang.Float]

    (instance? Byte n)
    [:invoke-static [:java.lang.Byte/valueOf :byte] :java.lang.Byte]

    (instance? Short n)
    [:invoke-static [:java.lang.Short/valueOf :short] :java.lang.Short])])

(defmethod emit-value :class [_ c]
  (if (primitive? c)
    [[:get-static (box c) "TYPE" :java.lang.Class]]
    [[:push (.getName ^Class c)]
     [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]]))

(defmethod emit-value :symbol [_ s]
  [[:push (namespace s)]
   [:push (name s)]
   [:invoke-static [:clojure.lang.Symbol/intern :java.lang.String :java.lang.String]
    :clojure.lang.Symbol]])

(defmethod emit-value :keyword [_ k]
  [[:push (namespace k)]
   [:push (name k)]
   [:invoke-static [:clojure.lang.Keyword/intern :java.lang.String :java.lang.String]
    :clojure.lang.Keyword]])

(defmethod emit-value :var [_ ^clojure.lang.Var v]
  [[:push (str (ns-name (.ns v)))]
   [:push (name (.sym v))]
   [:invoke-static [:clojure.lang.RT/var :java.lang.String :java.lang.String]
    :clojure.lang.Var]])

;; todo record/type, slow path

(defn emit-values-as-array [list]
  `[[:push ~(int (count list))]
    [:new-array :java.lang.Object]
    ~@(mapcat (fn [i item]
                `[[:dup]
                  [:push ~(int i)]
                  ~@(emit-value (u/classify item) item)
                  [:array-store :java.lang.Object]])
              (range) list)])

(defmethod emit-value :map [_ m]
  (let [arr (mapcat identity m)]
    `[~@(emit-values-as-array arr)
      [:invoke-static [:clojure.lang.RT/map :objects] :clojure.lang.IPersistentMap]]))

(defmethod emit-value :vector [_ v]
  `[~@(emit-values-as-array v)
    [:invoke-static [:clojure.lang.RT/vector :objects] :clojure.lang.IPersistentVector]])

(defmethod emit-value :set [_ s]
  `[~@(emit-values-as-array s)
    [:invoke-static [:clojure.lang.RT/set :objects] :clojure.lang.IPersistentSet]])

(defmethod emit-value :seq [_ s]
  `[~@(emit-values-as-array s)
    [:invoke-static [:java.util.Arrays/asList :objects] :java.util.List]
    [:invoke-static [:clojure.lang.PersistentList/create :java.util.List]
     :clojure.lang.IPersistentList]])

(defmethod emit-value :char [_ c]
  [[:push c]
   [:invoke-static [:java.lang.Character/valueOf :char] :java.lang.Character]])

(defmethod emit-value :regex [_ r]
  `[~@(emit-value :string (str r))
    [:invoke-static [:java.util.regex.Pattern/compile :java.lang.String]
     :java.util.regex.Pattern]])

(defn emit-constants [{:keys [class constants]}]
  (mapcat (fn [{:keys [val id tag type]}]
            `[~@(emit-value (or type (u/classify val)) val)
              [:check-cast ~tag]
              ~[:put-static class (str "const__" id) tag]])
          (vals constants)))

(defn emit-keyword-callsites
  [{:keys [keyword-callsites constants class]}]
  (mapcat (fn [k]
            (let [{:keys [id]} (k constants)]
              `[[:new-instance :clojure.lang.KeywordLookupSite]
                [:dup]
                ~@(emit-value :keyword k)
                [:invoke-constructor [:clojure.lang.KeywordLookupSite/<init> :clojure.lang.Keyword] :void]
                [:dup]
                ~[:put-static class (str "site__" id) :clojure.lang.KeywordLookupSite]
                ~[:put-static class (str "thunk__" id) :clojure.lang.ILookupThunk]]))
          keyword-callsites))


;; TODO: generalize this for deftype/reify: needs  mutable field handling + altCtor + annotations
;; add smap

(defn emit-class
  [{:keys [class-name meta methods variadic? constants closed-overs keyword-callsites
           protocol-callsites env annotations super interfaces op fields] :as ast}
   {:keys [debug? class-loader] :as frame}]
  (let [old-frame frame

        constants (into {}
                        (remove #(let [{:keys [tag type]} (val %)]
                                   (or (primitive? tag)
                                       (#{:string :bool} type)))
                                constants))

        frame (merge frame
                     {:class              class-name
                      :constants          constants
                      :closed-overs       closed-overs
                      :keyword-callsites  keyword-callsites
                      :protocol-callsites protocol-callsites})

        consts (mapv (fn [{:keys [id tag]}]
                       {:op   :field
                        :attr #{:public :final :static}
                        :name (str "const__" id)
                        :tag  tag})
                     (vals constants))

        meta-field (when meta
                     [{:op   :field
                       :attr #{:public :final}
                       :name "__meta"
                       :tag  :clojure.lang.IPersistentMap}])

        keyword-callsites (mapcat (fn [k]
                                    (let [{:keys [id]} (k constants)]
                                      [{:op   :field
                                        :attr #{:public :final :static}
                                        :name (str "site__" id)
                                        :tag  :clojure.lang.KeywordLookupSite}
                                       {:op   :field
                                        :attr #{:public :final :static}
                                        :name (str "thunk__" id)
                                        :tag  :clojure.lang.ILookupThunk}]))
                                  keyword-callsites)

        protocol-callsites  (mapcat (fn [p]
                                      (let [{:keys [id]} (constants p)]
                                        [{:op   :field
                                          :attr #{:private}
                                          :name (str "cached__class__" id)
                                          :tag  :java.lang.Class}
                                         {:op   :field
                                          :attr #{:private}
                                          :name (str "cached__proto__fn__" id)
                                          :tag  :clojure.lang.AFunction}
                                         {:op   :field
                                          :attr #{:private}
                                          :name (str "cached__proto__impl__" id)
                                          :tag  :clojure.lang.IFn}]))
                                    protocol-callsites)

        deftype? (= op :deftype)
        defrecord? (contains? closed-overs '__meta)

        closed-overs (mapv (fn [{:keys [name bind-tag mutable] :as local}]
                             (merge local
                                    {:op   :field
                                     :attr (when deftype?
                                             (if mutable
                                               #{mutable}
                                               #{:public :final}))
                                     :tag  (or bind-tag Object)}))
                           (if deftype?
                             fields
                             (vals closed-overs)))

        ctor-types (into (if meta [:clojure.lang.IPersistentMap] [])
                         (repeat (count closed-overs) :java.lang.Object))

        class-ctors [{:op     :method
                      :attr   #{:public :static}
                      :method [[:<clinit>] :void]
                      :code   `[[:start-method]
                                ~@(emit-line-number env)
                                ~@(when (seq constants)
                                    (emit-constants frame))
                                ~@(when (seq keyword-callsites)
                                    (emit-keyword-callsites frame))
                                [:return-value]
                                [:end-method]]}
                     (let [[start-label end-label] (repeatedly label)]
                       {:op     :method
                        :attr   #{:public}
                        :method `[[:<init> ~@ctor-types] :void]
                        :code   `[[:start-method]
                                  ~@(emit-line-number env)
                                  [:label ~start-label]
                                  [:load-this]
                                  [:invoke-constructor [~(keyword (name super) "<init>")] :void]
                                  ~@(when meta
                                      [[:load-this]
                                       [:load-arg 0]
                                       [:put-field class-name :__meta :clojure.lang.IPersistentMap]])
                                  ~@(mapcat
                                     (fn [{:keys [name bind-tag]} id]
                                       `[[:load-this]
                                         ~[:load-arg id]
                                         ~@(emit-cast Object bind-tag)
                                         ;; [:check-cast tag]
                                         ~[:put-field class-name name bind-tag]])
                                     closed-overs (if meta (rest (range)) (range)))

                                  [:label ~end-label]
                                  [:return-value]
                                  [:end-method]]})]

        defrecord-ctor (when defrecord?
                         [{:op     :method
                            :attr   #{:public}
                            :method `[[:<init> ~@(drop-last 2 ctor-types)] :void]
                            :code   `[[:start-method]
                                      [:load-this]
                                      [:load-args]
                                      [:insn :ACONST_NULL]
                                      [:insn :ACONST_NULL]
                                      [:invoke-constructor [~(keyword class-name "<init>") ~@ctor-types] :void]
                                      [:return-value]
                                      [:end-method]]}])

        kw-callsite-method (when-let [kw-cs (seq (frame :keyword-callsites))]
                             (let [cs-count (count kw-cs)
                                   [end-label & labels] (repeatedly (inc cs-count) label)]
                               [{:op     :method
                                 :attr   #{:public}
                                 :method [[:swapThunk :int :clojure.lang.ILookupThunk] :void]
                                 :code   `[[:start-method]
                                           [:load-arg 0]
                                           ~[:table-switch-insn 0 (dec cs-count) end-label labels]
                                           ~@(mapcat (fn [i l]
                                                       [[:mark l]
                                                        [:load-arg 1]
                                                        [:put-static class-name (str "thunk__" i) :clojure.lang.ILookupThunk]
                                                        [:go-to end-label]])
                                                     (range) labels)
                                           [:mark ~end-label]
                                           [:return-value]
                                           [:end-method]]}]))

        variadic-method (when variadic?
                          (let [required-arity (->> methods (filter :variadic?) first :fixed-arity)]
                            [{:op     :method
                              :attr   #{:public}
                              :method [[:getRequiredArity] :int]
                              :code   `[[:start-method]
                                        [:push ~(int required-arity)]
                                        [:return-value]
                                        [:end-method]]}]))

        meta-methods (when meta
                       [{:op     :method
                         :attr   #{:public}
                         :method `[[:<init> ~@(butlast ctor-types)] :void]
                         :code   `[[:start-method]
                                   [:load-this]
                                   [:insn :ACONST_NULL]
                                   [:load-args]
                                   [:invoke-constructor [~(keyword class-name "<init>")
                                                         ~@ctor-types] :void]
                                   [:return-value]
                                   [:end-method]]}
                        {:op     :method
                         :attr   #{:public}
                         :method`[[:meta] :clojure.lang.IPersistentMap]
                         :code   [[:start-method]
                                  [:load-this]
                                  [:get-field class-name :__meta :clojure.lang.IPersistentMap]
                                  [:return-value]
                                  [:end-method]]}
                        {:op     :method
                         :attr   #{:public}
                         :method`[[:withMeta :clojure.lang.IPersistentMap] :clojure.lang.IObj]
                         :code   `[[:start-method]
                                   [:new-instance ~class-name]
                                   [:dup]
                                   [:load-arg 0]
                                   ~@(mapcat
                                      (fn [{:keys [name tag]}]
                                        [[:load-this]
                                         [:get-field class-name name tag]])
                                      closed-overs)
                                   [:invoke-constructor [~(keyword class-name "<init>")
                                                         ~@ctor-types] :void]
                                   [:return-value]
                                   [:end-method]]}])

        deftype-fields (vec (remove '#{__meta __extmap} (mapv :name closed-overs)))

        deftype-methods (when deftype?
                          `[~{:op     :method
                              :attr   #{:public :static}
                              :method [[:getBasis] :clojure.lang.IPersistentVector]
                              :code   `[[:start-method]
                                        ~@(emit-value :vector deftype-fields)
                                        [:return-value]
                                        [:end-method]]}
                            ~@(when defrecord?
                                [{:op     :method
                                  :attr   #{:public :static}
                                  :method [[:create :clojure.lang.IPersistentMap] class-name]
                                  :code   `[[:start-method]
                                            ~@(mapcat
                                               (fn [field id]
                                                 `[[:load-arg 0]
                                                   ~@(emit-value :keyword field)
                                                   [:insn :ACONST_NULL]
                                                   [:invoke-interface [:clojure.lang.IPersistentMap/valAt :java.lang.Object :java.lang.Object] :java.lang.Object]
                                                   [:astore ~id]
                                                   [:load-arg 0]
                                                   ~@(emit-value :keyword field)
                                                   [:invoke-interface [:clojure.lang.IPersistentMap/without :java.lang.Object] :clojure.lang.IPersistentMap]
                                                   [:store-arg 0]])
                                               deftype-fields (rest (range)))
                                            [:new-instance ~class-name]
                                            [:dup]
                                            ~@(for [i (rest (range (inc (count deftype-fields))))]
                                                [:var-insn :java.lang.Object/ILOAD i])
                                            [:insn :ACONST_NULL]
                                            [:load-arg 0]
                                            [:invoke-static [:clojure.lang.RT/seqOrElse :java.lang.Object] :java.lang.Object]
                                            [:invoke-constructor [~(keyword class-name "<init>")
                                                                  ~@ctor-types] :void]
                                            [:return-value]
                                            [:end-method]]}])])

        jvm-ast
        {:op          :class
         :debug?      debug?
         :attr        #{:public :super :final}
         :annotations annotations
         :name        (s/replace class-name \. \/)
         :super       (s/replace (name super) \. \/)
         :interfaces  interfaces
         :fields      `[~@consts ~@ keyword-callsites
                        ~@meta-field ~@closed-overs ~@protocol-callsites]
         :methods     `[~@class-ctors ~@defrecord-ctor ~@deftype-methods
                        ~@kw-callsite-method ~@variadic-method
                        ~@meta-methods ~@(mapcat #(-emit % frame) methods)]}

        bc
        (t/-compile jvm-ast)

        class (.defineClass ^clojure.lang.DynamicClassLoader class-loader class-name bc nil)]
    (if deftype?
      [[:insn :ACONST_NULL]]
      (with-meta
        `[[:new-instance ~class-name]
          [:dup]
          ~@(when meta
              [[:insn :ACONST_NULL]])
          ~@(mapcat #(emit (assoc % :op :local :tag Object) old-frame)
                    closed-overs) ;; need to clear?
          [:invoke-constructor [~(keyword class-name "<init>")
                                ~@ctor-types] :void]]
        {:class class}))))

(defmethod -emit :reify
  [{:keys [class-name] :as ast}
   frame]
  (let [class-name (.getName ^Class class-name)
        ast (assoc ast
              :class-name class-name
              :super :java.lang.Object
              :meta {})]
    (emit-class ast frame)))

(defmethod -emit :deftype
  [{:keys [class-name] :as ast}
   frame]
  (let [class-name (.getName ^Class class-name)
        ast (assoc ast
              :class-name class-name
              :super :java.lang.Object)]
    (with-meta
      (emit-class ast frame)
      {:untyped true})))

(defmethod -emit :fn
  [{:keys [local variadic?] :as ast}
   {:keys [class] :as frame}]
  (let [class-name (str (or class (munge (ns-name *ns*)))
                        "$"
                        (gensym (str (or (and (:form local)
                                              (s/replace (:form local) "." "_DOT_"))
                                         "fn") "__")))
        super (if variadic? :clojure.lang.RestFn :clojure.lang.AFunction)
        ast (assoc ast
              :class-name class-name
              :super super)]
    (emit-class ast frame)))

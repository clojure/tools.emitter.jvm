;:   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.emitter.jvm.transform
  (:refer-clojure :exclude [type])
  (:require [clojure.string :as s]
            [clojure.tools.analyzer.jvm.utils :refer [maybe-class]]
            [clojure.tools.analyzer.utils :refer [update!]])
  (:import (org.objectweb.asm Type Label Opcodes ClassWriter ClassReader)
           (org.objectweb.asm.commons GeneratorAdapter Method)
           (org.objectweb.asm.util CheckClassAdapter TraceClassVisitor)))

(defn type-str [x]
  (cond

   (= :objects x)
   "java.lang.Object[]"

   (class? x)
   (let [class ^Class x
         n (.getName class)]
     (if (.endsWith n ";")
       (str (subs n 2 (dec (count n))) "[]")
       n)
     #_(.getCanonicalName ^Class x))

   :else
   (name x)))

(defn method-desc [ret method args]
  (Method/getMethod (str (type-str ret) " "
                         (name method)
                         \( (s/join ", " (map type-str args)) \))))

(def ^:dynamic *labels* {})
(def ^:dynamic *locals* {})

(defmulti -compile :op)
(defmulti -exec (fn [op _ _] op))

(defn transform [gen bc]
  (binding [*locals* *locals*
            *labels* *labels*]
    (doseq [[inst & args] bc]
      (-exec inst args gen))))

(def ^:const objects (Class/forName "[Ljava.lang.Object;"))

(defn special [c]
  (case (name c)
    "int" Integer/TYPE
    "float" Float/TYPE
    "void" Void/TYPE
    "long" Long/TYPE
    "byte" Byte/TYPE
    "char" Character/TYPE
    "double" Double/TYPE
    "boolean" Boolean/TYPE

    "objects" objects

    nil))

(defn ^Class get-class [type-desc]
  (cond
   (nil? type-desc)
   Object

   (class? type-desc)
   type-desc

   (special type-desc)
   (special type-desc)

   :else
   (Class/forName (name type-desc))))

(defn ^Type type [type-desc]
  (try
    (Type/getType (get-class type-desc))
    (catch ClassNotFoundException e
      (Type/getObjectType (s/replace type-desc \. \/)))))

(defmethod -exec :invoke-static
  [_ [[method & args] ret] ^GeneratorAdapter gen]
  (let [[class method-name]
        [(namespace method) (name method)]]
    (.invokeStatic gen (type class) (method-desc ret method-name args))))

(defmethod -exec :invoke-virtual
  [_ [[method & args] ret] ^GeneratorAdapter gen]
  (let [[class method-name]
        [(namespace method) (name method)]]
    (.invokeVirtual gen (type class) (method-desc ret method-name args))))

(defmethod -exec :invoke-interface
  [_ [[method & args] ret] ^GeneratorAdapter gen]
  (let [[class method-name]
        [(namespace method) (name method)]]
    (.invokeInterface gen (type class) (method-desc ret method-name args))))

(defmethod -exec :invoke-constructor
  [_ [[method & args] ret] ^GeneratorAdapter gen]
  (let [[class method-name]
        [(namespace method) (name method)]]
    (.invokeConstructor gen (type class) (method-desc ret method-name args))))

(defmethod -exec :check-cast
  [_ [class] ^GeneratorAdapter gen]
  (.checkCast gen (type class)))

(defmethod -exec :new-array
  [_ [class] ^GeneratorAdapter gen]
  (.newArray gen (type class)))

(defmethod -exec :array-store
  [_ [class] ^GeneratorAdapter gen]
  (.arrayStore gen (type class)))

(defmethod -exec :new-instance
  [_ [class] ^GeneratorAdapter gen]
  (.newInstance gen (type class)))

(defmethod -exec :instance-of
  [_ [class] ^GeneratorAdapter gen]
  (.instanceOf gen (type class)))

(defmethod -exec :get-static
  [_ args ^GeneratorAdapter gen]
  (let [[class field tag]
        (if (= 3 (count args))
          args
          [(namespace (first args)) (name (first args)) (second args)])]
    (.getStatic gen (type class) (name field) (type tag))))

(defmethod -exec :put-static
  [_ args ^GeneratorAdapter gen]
  (let [[class field tag]
        (if (= 3 (count args))
          args
          [(namespace (first args)) (name (first args)) (second args)])]
    (.putStatic gen (type class) (name field) (type tag))))

(defmethod -exec :get-field
  [_ args ^GeneratorAdapter gen]
  (let [[class field tag]
        (if (= 3 (count args))
          args
          [(namespace (first args)) (name (first args)) (second args)])]
    (.getField gen (type class) (name field) (type tag))))

(defmethod -exec :put-field
  [_ args ^GeneratorAdapter gen]
  (let [[class field tag]
        (if (= 3 (count args))
          args
          [(namespace (first args)) (name (first args)) (second args)])]
    (.putField gen (type class) (name field) (type tag))))

(defn get-label [^GeneratorAdapter gen label]
  (or (*labels* label)
      (let [l (.newLabel gen)]
        (update! *labels* assoc label l)
        l)))

(defmethod -exec :mark
  [_ [label] ^GeneratorAdapter gen]
  (.mark gen (get-label gen label)))

(defmethod -exec :label
  [_ [label] ^GeneratorAdapter gen]
  (.visitLabel gen (get-label gen label)))

(defmethod -exec :go-to
  [_ [label] ^GeneratorAdapter gen]
  (.goTo gen (get-label gen label)))

(defmethod -exec :start-method
  [_ _ ^GeneratorAdapter gen]
  (.visitCode gen))

(defmethod -exec :end-method
  [_ _ ^GeneratorAdapter gen]
  (.endMethod gen))

(defmethod -exec :return-value
  [_ _ ^GeneratorAdapter gen]
  (.returnValue gen))

(defmethod -exec :load-this
  [_ _ ^GeneratorAdapter gen]
  (.loadThis gen))

(defmethod -exec :load-args
  [_ _ ^GeneratorAdapter gen]
  (.loadArgs gen))

(defmethod -exec :swap
  [_ _ ^GeneratorAdapter gen]
  (.swap gen))

(defmethod -exec :dup
  [_ _ ^GeneratorAdapter gen]
  (.dup gen))

(defmethod -exec :dup-x1
  [_ _ ^GeneratorAdapter gen]
  (.dupX1 gen))

(defmethod -exec :dup-x2
  [_ _ ^GeneratorAdapter gen]
  (.dupX2 gen))

(defmethod -exec :dup2
  [_ _ ^GeneratorAdapter gen]
  (.dup2 gen))

(defmethod -exec :dup2-x1
  [_ _ ^GeneratorAdapter gen]
  (.dup2X1 gen))

(defmethod -exec :dup2-x2
  [_ _ ^GeneratorAdapter gen]
  (.dup2X2 gen))

(defmethod -exec :pop
  [_ _ ^GeneratorAdapter gen]
  (.pop gen))

(defmethod -exec :pop2
  [_ _ ^GeneratorAdapter gen]
  (.pop2 gen))

(defmethod -exec :throw-exception
  [_ _ ^GeneratorAdapter gen]
  (.throwException gen))

(defmethod -exec :monitor-enter
  [_ _ ^GeneratorAdapter gen]
  (.monitorEnter gen))

(defmethod -exec :monitor-exit
  [_ _ ^GeneratorAdapter gen]
  (.monitorExit gen))

(defn opcode [op]
  (cond
   (integer? op)
   op

   (nil? op)
   0

   :else
   (case (name op)
     "ISTORE"               Opcodes/ISTORE
     "ILOAD"                Opcodes/ILOAD
     "ACONST_NULL"          Opcodes/ACONST_NULL
     "IF_ACMPEQ"            Opcodes/IF_ACMPEQ
     "IF_ACMPNE"            Opcodes/IF_ACMPNE
     "ISHR"                 Opcodes/ISHR
     "IAND"                 Opcodes/IAND
     "public"               Opcodes/ACC_PUBLIC
     "bridge"               Opcodes/ACC_BRIDGE
     "super"                Opcodes/ACC_SUPER
     "final"                Opcodes/ACC_FINAL
     "static"               Opcodes/ACC_STATIC
     "private"              Opcodes/ACC_PRIVATE
     "volatile-mutable"     Opcodes/ACC_VOLATILE
     "synchronized-mutable" 0
     "EQ"                   GeneratorAdapter/EQ
     "NE"                   GeneratorAdapter/NE)))

(defmethod -exec :insn
  [_ [insn] ^GeneratorAdapter gen]
  (.visitInsn gen (opcode insn)))

(defmethod -exec :jump-insn
  [_ [insn label] ^GeneratorAdapter gen]
  (.visitJumpInsn gen (opcode insn) (get-label gen label)))

(defmethod -exec :if-null
  [_ [label] ^GeneratorAdapter gen]
  (.ifNull gen (get-label gen label)))

(defmethod -exec :if-z-cmp
  [_ [insn label] ^GeneratorAdapter gen]
  (.ifZCmp gen (opcode insn) (get-label gen label)))

(defmethod -exec :if-cmp
  [_ [t insn label] ^GeneratorAdapter gen]
  (.ifCmp gen (type t) (opcode insn) (get-label gen label)))

(defn get-local [local]
  (if (integer? local)
    local
    (or (*locals* local)
        (let [l (count *locals*)]
          (update! *locals* assoc local l)
          l))))

(defmethod -exec :load-arg
  [_ [arg] ^GeneratorAdapter gen]
  (.loadArg gen (int arg)))

(defmethod -exec :store-arg
  [_ [arg] ^GeneratorAdapter gen]
  (.storeArg gen (int arg)))


(defmethod -exec :var-insn
  [_ [insn local] ^GeneratorAdapter gen]
  (.visitVarInsn gen (.getOpcode (type (namespace insn))
                                 (opcode (name insn)))
                 (get-local local)))

(defmethod -exec :aload
  [_ [local] ^GeneratorAdapter gen]
  (.visitVarInsn gen Opcodes/ALOAD (get-local local)))

(defmethod -exec :astore
  [_ [local] ^GeneratorAdapter gen]
  (.visitVarInsn gen Opcodes/ASTORE (get-local local)))

(defn descriptor [tag]
  (.getDescriptor (type tag)))

(defmethod -exec :try-catch-block
  [_ [l1 l2 l3 t] ^GeneratorAdapter gen]
  (.visitTryCatchBlock gen (get-label gen l1) (get-label gen l2) (get-label gen l3)
                       (when t (apply str (butlast (rest (descriptor t)))))))

(defmethod -exec :local-variable
  [_ [desc tag _ l1 l2 local] ^GeneratorAdapter gen]
  (.visitLocalVariable gen (name desc) (descriptor tag) nil (get-label gen l1)
                       (get-label gen l2) (get-local local)))

(defmethod -exec :line-number
  [_ [line label] ^GeneratorAdapter gen]
  (.visitLineNumber gen (int line) (get-label gen label)))

(defmethod -exec :table-switch-insn
  [_ [l h default-label labels] ^GeneratorAdapter gen]
  (.visitTableSwitchInsn gen (int (get-local l)) (int (get-local h))
                         (get-label gen default-label)
                         (into-array Label (mapv #(get-label gen %) labels))))

(defmethod -exec :lookup-switch-insn
  [_ [l t labels] ^GeneratorAdapter gen]
  (.visitLookupSwitchInsn gen (get-label gen l) (int-array (map get-local t))
                          (into-array Label (mapv #(get-label gen %) labels))))

;; todo: smarter
(defmethod -exec :push
  [_ [x] ^GeneratorAdapter gen]
  (cond

   (or (nil? x) (string? x))
   (.push gen ^String x)

   (instance? Integer x)
   (.push gen (int x))

   (instance? Long x)
   (.push gen (long x))

   (instance? Float x)
   (.push gen (float x))

   (instance? Double x)
   (.push gen (double x))

   (or (instance? Character x)
       (instance? Short x))
   (.visitIntInsn gen Opcodes/SIPUSH (int x))

   (instance? Byte x)
   (.visitIntInsn gen Opcodes/BIPUSH (int x))))

(defn compute-attr [attr]
  (reduce (fn [r x] (+ r (opcode x))) 0 attr))

(defmethod -compile :method
  [{:keys [attr method code cv]}]
  (let [[[method-name & args] ret] method
        m (method-desc ret method-name args)
        gen (GeneratorAdapter. (compute-attr attr) m nil nil cv)]

    (transform gen code)))

(defmethod -compile :field
  [{:keys [attr tag cv] :as f}]
  (let [tag (if (keyword? tag) (Class/forName (name tag)) tag)]
    (.visitField ^ClassWriter cv (compute-attr attr) (name (:name f))
                 (descriptor tag) nil nil)))

(defmethod -compile :class
  [{:keys [attr super fields methods debug? interfaces] :as c}]
  (let [cv (ClassWriter. ClassWriter/COMPUTE_MAXS)
        interfaces (into interfaces (keep :interface methods))]

    (.visit cv Opcodes/V1_5 (compute-attr attr) (:name c) nil (name super)
            (into-array String (mapv (fn [i] (s/replace (type-str i) \. \/)) interfaces)))

    (.visitSource cv (:name c) nil)

    (doseq [f fields]
      (-compile (assoc f :cv cv)))

    (doseq [m methods]
      (-compile (assoc m :cv cv)))

    (.visitEnd cv)
    (let [bc (.toByteArray cv)]
      (when debug?
        (let [cr (ClassReader. bc)
              w (java.io.PrintWriter. *out*)
              v (TraceClassVisitor. w)
              v (CheckClassAdapter. v)]
          (.accept cr v 0)))
      bc)))

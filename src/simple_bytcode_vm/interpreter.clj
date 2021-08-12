(ns simple-bytcode-vm.interpreter
  (:refer-clojure :exclude [eval])
  (:require [simple-bytcode-vm.env :as env]
            [simple-bytcode-vm.util :as u]))

(declare eval)

(defmulti eval-instruction
  (fn [[opcode & args] state]
    opcode))

(defn apply-fn
  [{:keys [params body env]} args]
  (let [args-env (zipmap params args)
        fn-env   (env/extend env args-env)]
    (eval body fn-env)))

(defn make-fn
  [fn-internals]
  (fn [& args]
    (apply-fn fn-internals args)))


(defmethod eval-instruction :load-const
  [[_ const] {:keys [pc stack env]}]
  {:pc (inc' pc)
   :stack (cons const stack)
   :env env})

(defmethod eval-instruction :store-name
  [[_ name] {:keys [pc stack env]}]
  {:pc (inc' pc)
   :stack (rest stack)
   :env (env/assoc! env name (first stack))})

(defmethod eval-instruction :load-name
  [[_ name] {:keys [pc stack env]}]
  {:pc (inc' pc)
   :stack (cons (env/lookup env name) stack)
   :env env})

(defmethod eval-instruction :call-function
  [[_ nargs] {:keys [pc stack env]}]
  (let [args (reverse (take nargs stack))
        f    (nth stack nargs)
        stack (drop (inc nargs) stack)]
    {:pc (inc' pc)
     :stack (cons (apply f args) stack)
     :env env}))

(defmethod eval-instruction :relative-jump
  [[_ offset] state]
  (update state :pc +' offset))

(defmethod eval-instruction :relative-jump-if-true
  [[_ offset] {:keys [pc stack env]}]
  (let [[cond-val & stack] stack]
    {:pc (+' pc (if cond-val offset 1))
     :stack stack
     :env env}))

(defmethod eval-instruction :make-function
  [_ {:keys [pc stack env]}]
  (let [[body params & stack] stack
        the-fn (make-fn {:params params
                         :body body
                         :env env})]
    {:pc (+' pc 1)
     :stack (cons the-fn stack)
     :env env}))

(defmethod eval-instruction :default
  [instruction _]
  (u/throw+ "Error: " #'eval-instruction " not defined for:\n\t" instruction))


(defn eval
  ([instructions]
   (eval instructions (env/base-env)))
  ([instructions env]
   (let [instructions (vec instructions)
         total-instructions (count instructions)]
     (loop [{:keys [pc stack] :as state}
            {:pc 0
             :stack ()
             :env env}]
       (if (< pc total-instructions)
         (let [ins (instructions pc)]
           (recur (eval-instruction ins state)))
         (first stack))))))


(comment
  "Helper fns"
  (remove-all-methods eval-instruction)
  )

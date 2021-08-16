(ns simple-bytcode-vm.experiments
  (:require [clojure.core.match :refer [match]]
            [simple-bytcode-vm.env :as env]
            [simple-bytcode-vm.util :as u]))

;; Naive AST Walker
;; ================
(defn walk
  [expr env]
  (match expr
   [:lit e]
   e

   [:var idx]
   (env/lookup env idx)

   [:set idx e]
   (let [v (walk e env)]
     (env/assoc! env idx v)
     v)

   [:bin op e1 e2]
   (let [f  (resolve op)
         v1 (walk e1 env)
         v2 (walk e2 env)]
     (f v1 v2))

   [:do & forms]
   (reduce (fn [_ form]
             (walk form env))
           nil
           forms)

   [:while e-condition e-body]
   (while (walk e-condition env)
     (walk e-body env))))

(defn naive-ast-walk
  [expr]
  (let [env (env/base-env)]
    #(walk expr env)))


;; Compile to closure
;; ==================
(defn ->closure
  [expr]
  (match expr
   [:lit e]
   (fn [_] e)

   [:var idx]
   #(env/lookup % idx)

   [:set idx e]
   (let [f (->closure e)]
     (fn [env]
       (let [v (f env)]
         (env/assoc! env idx v)
         v)))

   [:bin op e1 e2]
   (let [f  (resolve op)
         f1 (->closure e1)
         f2 (->closure e2)]
     (fn [env]
       (let [v1 (f1 env)
             v2 (f2 env)]
         (f v1 v2))))

   [:do & forms]
   (let [compiled-forms (map ->closure forms)]
     (fn [env]
       (reduce (fn [_ f]
                 (f env))
               nil
               compiled-forms)))

   [:while e-condition e-body]
   (let [f-condition (->closure e-condition)
         f-body (->closure e-body)]
     (fn [env]
       (while (f-condition env)
         (f-body env))))))

(defn compiled-to-closure
  [expr]
  (let [cc (->closure expr)
        env (env/base-env)]
    #(cc env)))


;; Naive bytecode compiler + interpreter
;; =====================================
(defn ->bytecode
  [expr]
  (match expr
   [:lit e]
   [[:load-const e]]

   [:var idx]
   [[:load-name idx]]

   [:set idx e]
   (u/vconcat (->bytecode e)
              [[:store-name idx]])

   [:bin op e1 e2]
   (let [b1 (->bytecode e1)
         b2 (->bytecode e2)]
     (u/vconcat b1 b2 [[:call-bin op]]))

   [:do & forms]
   (u/vmapcat ->bytecode forms)

   [:while e-condition e-body]
   (let [cond-bc (->bytecode e-condition)
         body-bc (->bytecode e-body)
         end-offset (+ 2 (count body-bc))
         top-offset (- (+ (count cond-bc)
                          (count body-bc)
                          1))]
     (u/vconcat cond-bc
                [[:relative-jump-if-false end-offset]]
                body-bc
                [[:relative-jump top-offset]]))))

(defn naive-interpret-bytecode
  [tape env]
  (let [tape-length (count tape)]
    (loop [pc 0
           stack ()]
      (if (< pc tape-length)
        (match (tape pc)
         [:load-const e]
         (recur (inc pc)
                (cons e stack))

         [:load-name idx]
         (recur (inc pc)
                (cons (env/lookup env idx) stack))

         [:store-name idx]
         (let [[x & stack] stack]
           (env/assoc! env idx x)
           (recur (inc pc)
                  stack))

         [:call-bin op]
         (let [[e2 e1 & stack] stack
               f (resolve op)
               res (f e1 e2)]
           (recur (inc pc)
                  (cons res stack)))

         [:relative-jump offset]
         (recur (+ pc offset)
                stack)

         [:relative-jump-if-false offset]
         (let [[dont-jump? & stack] stack]
           (recur (+ pc (if dont-jump? 1 offset))
                  stack)))
        (first stack)))))

(defn compiled-to-bytecode
  [expr]
  (let [bc (->bytecode expr)
        env (env/base-env)] 
    #(naive-interpret-bytecode bc env)))


(comment
  (do
    ;; Benchmarking util
    ;; =================
    (defn bench
      [msg f]
      (let [x (volatile! nil)]
        (time
         (dotimes [_ 5000]
           (vreset! x (f))))
        (println msg @x)))


    ;; Code to be interpreted
    ;; ======================
    (def code
      '[:do
        [:set n [:lit 10]]
        [:set f [:lit 1]]

        [:while [:bin > [:var n] [:lit 1]]
         [:do
          [:set f [:bin *' [:var n] [:var f]]]
          [:set n [:bin -  [:var n] [:lit 1]]]]]

        [:var f]])
    

    ;; Clojure code without interpretation overhead
    ;; ============================================
    (defn clj-code-with-interpreter-env
      [env]
      (env/assoc! env 'n 10)
      (env/assoc! env 'f 1)

      (while (> (env/lookup env 'n) 1)
        (env/assoc! env 'f (*' (env/lookup env 'n)
                               (env/lookup env 'f)))
        (env/assoc! env 'n (- (env/lookup env 'n)
                              1)))

      (env/lookup env 'f))

    (defn init-clj-code
      [code]
      (let [env (env/base-env)]
        #(code env)))


    ;; Clojure code without interpreter env
    ;; ====================================
    (defn clj-code-with-own-atom-env []
      (let [n (atom 10)
            f (atom 1)]
        (while (> @n 1)
          (swap! f *' @n)
          (swap! n - 1))
        @f))


    (defn clj-code-with-own-volatile-env []
      (let [n (volatile! 10)
            f (volatile! 1)]
        (while (> @n 1)
          (vswap! f *' @n)
          (vswap! n - 1))
        @f))


    ;; Idiomatic Clojure
    ;; =================
    (defn idiomatic-clj []
      (reduce *' (range 1 (inc 10))))


    ;; Benchmarks
    ;; ==========
    (defn bench-all
      [msg-f-pairs]
      (doseq [[msg f] msg-f-pairs]
        (bench msg f)
        (println)))

    (bench-all
     [[:naive-ast-walk
       (naive-ast-walk code)]

      [:compliled-to-closure
       (compiled-to-closure code)]

      [:compiled-to-bytecode
       (compiled-to-bytecode code)]

      [:clj-with-interpreter-env
       (init-clj-code clj-code-with-interpreter-env)]

      [:clj-with-own-atoms
       clj-code-with-own-atom-env]

      [:clj-with-own-volatiles
       clj-code-with-own-volatile-env]

      [:idiomatic-clj
       idiomatic-clj]]))
  )

(ns simple-bytcode-vm.core-test
  (:require [clojure.test :refer :all]
            [simple-bytcode-vm.repl :as repl]))

(deftest basic-functional-test
  (is (== 120
          (repl/eval
           '(do
              (def fact
                (fn [n]
                  (if (< n 2)
                    1
                    (* n (fact (- n 1))))))

              (fact 5))))))

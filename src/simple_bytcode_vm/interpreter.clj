(ns simple-bytcode-vm.interpreter
  (:refer-clojure :exclude [eval]))

(defn eval
  [instructions]
  (loop [pc 0
         stack ()
         instructions (vec instructions)]
    (if (< pc (count instructions))
      (let [ins (first instructions)
            [op arg] ins]
        (case op
          :load-const
          (recur (inc' pc)
                 (cons arg stack)
                 instructions)))
      (first stack))))

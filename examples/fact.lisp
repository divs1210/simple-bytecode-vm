(do

 (def fact
      (fn [n]
          (if (< n 2)
              1
            (* n (fact (- n 1))))))

 (def args
      (command-line-args))

 (def n
      (read-string (args 1)))

 (println "fact(" n ") => " (fact n)))

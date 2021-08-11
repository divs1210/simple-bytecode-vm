(ns simple-bytcode-vm.util
  (:require [clojure.string :as str]))

(defn throw+ [& msgs]
  (throw (Exception. (str/join msgs))))

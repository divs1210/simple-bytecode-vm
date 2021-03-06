(ns simple-bytcode-vm.util
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]))

(defn throw+
  [& msgs]
  (throw (Exception. (str/join msgs))))

(defn pretty-spit
  [filename data]
  (spit filename
        (with-out-str
          (pprint/pprint data))))

(defn vconcat
  [& seqs]
  (vec (apply concat seqs)))

(defn vmapcat
  [f coll]
  (vec (mapcat f coll)))

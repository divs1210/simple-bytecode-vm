(defproject simple-bytcode-vm "0.1.0-SNAPSHOT"
  :description "Simple bytecode compiler + VM for Clojure-like language"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.0-alpha1"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {"-compile" ["trampoline" "run" "-m" "simple-bytcode-vm.compiler"]
            "-run"     ["trampoline" "run" "-m" "simple-bytcode-vm.interpreter"]
            "-repl"    ["trampoline" "run" "-m" "simple-bytcode-vm.repl"]})

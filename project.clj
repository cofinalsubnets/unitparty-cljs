(defproject unitparty "0.1.0"

  :description "In-browser algebraic unit converter."

  :url "https://github.com/walpurgisriot/unitparty-cljs"

  :license {:name "GNU GPL"
            :url "https://www.gnu.org/licenses/gpl.html"}

  :min-lein-version "2.2.0"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2069"]]

  :plugins [[lein-cljsbuild "1.0.0-alpha1"]
            [com.cemerick/clojurescript.test "0.2.1"]]

  :hooks [leiningen.cljsbuild]

  :cljsbuild {

    :test-commands {"tests"
                    ["phantomjs" :runner ;;"phantom/test.js" "resources/private/test.html"]}
                     "window.literal_js_was_evaluated=true"
                     "target/cljs/unitparty-test.js"]}
    :builds {
 
      :dev {
        :source-paths ["src/cljs"]
        :compiler {
          :output-to "target/dev/unitparty-debug.js"
          :optimizations :whitespace
          :pretty-print  true }}
 
      :prod {
        :source-paths ["src/cljs"]
        :compiler {
          :output-to "target/dist/js/unitparty.js"
          :optimizations :advanced
          :pretty-print  false}}
 
     :test {
       :source-paths  ["src/cljs" "test"]
       :compiler {
         :output-to "target/cljs/unitparty-test.js"
         :optimizations :whitespace
         :pretty-print  true}}}})


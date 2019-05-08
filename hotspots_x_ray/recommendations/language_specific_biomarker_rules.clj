(ns hotspots-x-ray.recommendations.language-specific-biomarker-rules
  (:require [evolutionary-metrics.complexity.loco :as loco]))

;; We need language dependent rules since it's a huge difference betweeen 500 LoC Clojure vs Java.
;;
(def ^:const default-penalty-point-thresholds ; used in case there isn't a language-specific override
  {:minimum-loc-for-cohesion-calculation      500

   ; Some languages (e.g. Erlang) may have modules based on templates.
   ; That is, some functions will always be implemented. Here's our
   ; opportunity to filter out such functions from the cohesion metrics.
   :functions-to-exclude-from-cohesion       #{}

   :god-function-cc                           10
   :god-function-loc                          70

   :dear-god-function-cc                      100
   :dear-god-function-loc                     500

   ; The delta after known functions (i.e. parsed by X-Ray) have been substracted and
   ; comments and blanks removed.
   :large-data-declarations-delta-loc         1500
   :enable-large-data-declarations            true

   :median-fn-loc                             50.0

   :clone-ratio                               0.1
   :n-clones                                  4
   :small-file-loc                            200
   :many-code-clones                          10

   :cc-mean                                   4

   :main-body-cc                              10 ; complexity in global scope (e.g. PHP)

   ; detect the delta from previous month and warn if the growth exceeds this value:
   :complexity-delta-in-cc-for-trend-warning 10

   ; warn if single developer or former contributors is/are responsible for complex code
   :ownership-for-one-developer-warning       0.95
   :knowledge-loss-for-former-contributors-warning 0.8

   ; any nested complexity deeper than 4 is considered high
   :nested-complexity-of-interest             4
   :fns-for-much-nested-complexity            10

   :few-functions                             7
   :many-functions                            75

   :mean-fn-args                              4
   :many-primitives                           10
   :primive-fn-args-ratio                     0.3
   :string-fn-args-ratio                      0.39
   :max-fn-args                               5

   :max-ctor-args                             5

   :large-file-loc                            1000
   :really-large-file-loc                     5000
   :gigantic-file-loc                         10000

   :many-authors                              4
   :fractal-value-for-congestion              0.7

   :lcom4-suspected-low-cohesion              3
   :lcom4-low-cohesion                        3

   ;; Thresholds for test smells
   ;;

   ;; Assertion smells
   :many-large-assertion-blocks               4})



; The abstraction levels are lower in C so be more generous
(def ^:private c-specific-thresholds
  (merge default-penalty-point-thresholds
         {:many-primitives       20
          :primive-fn-args-ratio 0.6
          :many-functions       110}))

; The same goes for Go -- it's not exactly a wonder of abstractions, so relax the limits
(def ^:private go-specific-thresholds
  (merge default-penalty-point-thresholds
         {:many-primitives       15
          :primive-fn-args-ratio 0.4
          ; Go's error handling leads to lots of conditionals -> relax the rules
          :god-function-loc     80
          :dear-god-function-cc  150
          :cc-mean               5}))


; For Scala we expect less code in a unit, so punish harder
(def ^:private scala-specific-thresholds
  (merge default-penalty-point-thresholds
         {:large-file-loc        600
          :really-large-file-loc 3000
          :gigantic-file-loc     6000}))

; Python is supposed to be expressive, so punish harder
(def ^:private python-specific-thresholds
  (merge default-penalty-point-thresholds
         {:large-file-loc        600
          :really-large-file-loc 3000
          :gigantic-file-loc     6000

          ;; Python allows some meta programming tricks that might bias our LCOM 4 calculation.
          ;; Hence, we relax it just a little, little bit.
          :lcom4-suspected-low-cohesion 4
          :lcom4-low-cohesion           4}))

; For PHP we expect plenty of code in global scope, so let's punish that harder since it would be
;; better expressed using named elements (functions).
(def ^:private php-specific-thresholds
  (merge default-penalty-point-thresholds
         {:large-data-declarations-delta-loc 400
          ; A dynamic language like PHP might have us miss some variable/function reference, so relax the
          ; cohesion limits just a little bit:
          :lcom4-suspected-low-cohesion 4
          :lcom4-low-cohesion           4}))

(def ^:private perl-specific-thresholds
  (merge default-penalty-point-thresholds
       {:large-data-declarations-delta-loc 800
        :many-primitives       15
        :god-function-loc      80
        :dear-god-function-cc  150
        :cc-mean               5

        ; A dynamic language like PHP might have us miss some variable/function reference, so relax the
        ; cohesion limits just a little bit:
        :lcom4-suspected-low-cohesion 5
        :lcom4-low-cohesion           5}))

; Ruby is quite expressive so punish harder
(def ^:private ruby-specific-thresholds
  (merge default-penalty-point-thresholds
         {:large-data-declarations-delta-loc 800

          :large-file-loc        600
          :really-large-file-loc 3000
          :gigantic-file-loc     5500

          ; A dynamic language like Ruby might have us miss some variable/function reference, so relax the
          ; cohesion limits just a little bit:
          :lcom4-suspected-low-cohesion 7
          :lcom4-low-cohesion           7}))


; RSA-RTE files are XMI documents with lots of meta data. The active lines of code are
; real, so we only tweak the parameters that relate to non-code constructs:
(def ^:private rsa-rte-specific-thresholds
  (merge default-penalty-point-thresholds
         {:enable-large-data-declarations false

          ; Also, release the cohesion limits just a little bit in case we miss some part of the model:
          :lcom4-suspected-low-cohesion 4
          :lcom4-low-cohesion           4}))


; For Kotlin we expect less code in a unit, so punish harder
(def ^:private kotlin-specific-thresholds
  (merge default-penalty-point-thresholds
         {:large-file-loc        600
          :really-large-file-loc 3000
          :gigantic-file-loc     6000}))

; Render methods tend to be pretty long. If we can catch that, we use a
; higher threshold:
(def ^:private react-specific-thresholds
  (merge default-penalty-point-thresholds
         {:god-function-cc                           10
          :god-function-loc                          120}))

(def ^:private erlang-specific-thresholds
  (merge default-penalty-point-thresholds
         {; A dynamic language like Erlang might have us miss some variable/function reference, so relax the
          ; cohesion limits just a little bit:
          :lcom4-suspected-low-cohesion 4
          :lcom4-low-cohesion           4

          ; We also exclude the OTP function from cohesion calulcations:
          :functions-to-exclude-from-cohesion #{
                                                ; gen_server
                                                "handle_call"
                                                "handle_cast"
                                                "handle_info"
                                                "code_change"

                                                ; gen_statem
                                                "state_functions"
                                                "handle_event_function"

                                                ; gen_event
                                                "handle_event"

                                                ; common across behaviors
                                                "all"
                                                "start_link"
                                                "init"
                                                "stop"
                                                "terminate"

                                                ; common test
                                                "groups"
                                                "suite"
                                                "init_per_suite"
                                                "end_per_suite"
                                                "init_per_group"
                                                "end_per_group"
                                                "init_per_testcase"
                                                "end_per_testcase"
                                                }
          }))

;; Add custom rules to the map below (e.g. ".cs" csharp-rules).
(def ^:private extension->rules
  {".c"     c-specific-thresholds
   ".go"    go-specific-thresholds
   ".scala" scala-specific-thresholds
   ".kt"    kotlin-specific-thresholds
   ".py"    python-specific-thresholds
   ".php"   php-specific-thresholds
   ".rb"    ruby-specific-thresholds

   ".pl"    perl-specific-thresholds
   ".pm"    perl-specific-thresholds

   ".jsx"   react-specific-thresholds
   ".tsx"   react-specific-thresholds

   ".efx"   rsa-rte-specific-thresholds
   ".emx"   rsa-rte-specific-thresholds

   ".erl"   erlang-specific-thresholds})

(defn rules-for
  [file-name]
  (let [ext (loco/extension-of file-name)]
    (get extension->rules ext default-penalty-point-thresholds)))
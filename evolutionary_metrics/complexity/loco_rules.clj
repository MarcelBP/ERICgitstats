;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.complexity.loco-rules
  (:require [clojure.string :as s]))

(def ^:private extensions-to-ignore
  #{".bin" ".jar" ".class" ".exe" ".obj" ".o" ".png" ".jpg" ".gif" ".ico" ".so"
    ".gz" ".zip" ".tar" ".svg" ".war"
    ".db"
    ".eot" ".ttf" ".woff2" ".woff"
    ".gpg" ".sha1"
    ".ogg" ".wav" ".psd" ".swf" ".pdf"
    ".dll"})

(defn ignore-extension?
  [extension]
  (some->> extension clojure.string/lower-case extensions-to-ignore))

(defn- comment?
  [expr line]
  (re-find expr line))

(def clj-comment? (partial comment? #"^\s*;"))

(def xml-comment? (partial comment? #"<!\-\-.*?\-\->"))
(def xml-start-comment? (partial comment? #"^\s*<!\-\-"))
(def xml-end-comment? (partial comment? #"\-\->"))

(def elm-comment? (partial comment? #"^\s*\-\-\s"))
(def elm-start-comment? (partial comment? #"^\s*\{\-"))
(def elm-end-comment? (partial comment? #"\-\}"))

(def cstyle-comment? (partial comment? #"^\s*//"))
(def cstyle-single-line-comment (partial comment? #"//"))
(def cstyle-start-comment? (partial comment? #"^\s*/\*"))
(def cstyle-end-comment? (partial comment? #"\*/"))

(def dash-comment? (partial comment? #"^\s*#"))

(def dash-comment-after-code? (partial comment? #"#"))

(def python-comment? dash-comment?)
(def ruby-comment? dash-comment?)
(def perl-comment? dash-comment?)
(def ruby-start-comment? (partial comment? #"=begin"))
(def ruby-end-comment? (partial comment? #"=end"))

(defn pure-text
  [name]
  {:language name :blank? s/blank? :comment? (fn [_] false)})

(def perl-lang
  {:language "Perl" :blank? s/blank? :comment? perl-comment?
   :multi-start? (partial comment? #"=(?!cut)\w+")
   :multi-end? (partial comment? #"=cut")})

(defn- cstyle-lang
  [name]
  {:language name :blank? s/blank? :comment? cstyle-comment?
   :commented-line? cstyle-single-line-comment
   :multi-start? cstyle-start-comment?
   :multi-end? cstyle-end-comment?})

(def fortran-lang
  {:language "Fortran" :blank? s/blank? :comment? (partial comment? #"^\s*!\.*")})

(def preprocessed-fortran-lang
  {:language "Fortran"
   :blank? s/blank?
   :comment? (partial comment? #"^\s*(!|//)\.*")
   :multi-start? cstyle-start-comment?
   :multi-end? cstyle-end-comment?})

(def gams-lang
  {:language "GAMS" :blank? s/blank? :comment? (partial comment? #"^\s*\*")
   :multi-start? (partial comment? #"^\s*\$ontext")
   :multi-end? (partial comment? #"^\s*\$offtext")})

(def pascal-lang
  {:language "Pascal"
   :blank? s/blank?
   :comment? cstyle-comment?
   :multi-start? (partial comment? #"(\(\*|\{)")
   :multi-end? (partial comment? #"(\*\)|\})")})

(def php-style
  {:language "PHP" :blank? s/blank? :comment? (partial comment? #"^\s*((//)|#)")
   :commented-line? (partial comment? #"((//)|#)")
   :multi-start? cstyle-start-comment?
   :multi-end? cstyle-end-comment?})

(defn- ruby-style
  [name]
  {:language name :blank? s/blank? :comment? ruby-comment?
   :commented-line? dash-comment-after-code?
   :multi-start? ruby-start-comment? :multi-end? ruby-end-comment?})

(def racket-lang
  {:language "Racket" :blank? s/blank? :comment? clj-comment?
   :multi-start? (partial comment? #"^\s* #\|")
   :multi-end? (partial comment? #"\|#")})

(defn- ml-lang
  [name]
  {:language name :blank? s/blank? :comment? cstyle-comment?
   :multi-start? (partial comment? #"^\s*\(\*")
   :multi-end? (partial comment? #"\*\)")})

(def dos-batch
  {:language "DOS Batch" :blank? s/blank? :comment? (partial comment? #"^\s*(REM)|(::)")})

(defn- lang-with-%%-comments
  [name]
  {:language name :blank? s/blank? :comment? (partial comment? #"^\s*%%")})

(def sql-lang
  {:language "SQL" :blank? s/blank? :comment? (partial comment? #"^\s*\-\-")
   :multi-start? cstyle-start-comment?
   :multi-end? cstyle-end-comment?})

(defn-  specific-sql-lang
  [lang]
  {:language lang :blank? s/blank? :comment? (partial comment? #"^\s*\-\-")
   :multi-start? cstyle-start-comment?
   :multi-end? cstyle-end-comment?})

(defn- haskell-style-lang
  [name]
  {:language name
   :blank? s/blank?
   :comment? (partial comment? #"^\s*\-\-")
   :multi-start? (partial comment? #"\{-")
   :multi-end? (partial comment? #"-\}")})

(defn- literate-haskell-style-lang
  [name]
  {:language name
   :blank? s/blank?
   :comment? (partial comment? #"^[^>]")})

(defn- lua-style-lang
  [name]
  {:language name
   :blank? s/blank?
   :comment? (partial comment? #"^\s*\-\-")
   :multi-start? (partial comment? #"\-\-\[=*\[")
   :multi-end? (partial comment? #"\]=*\]")})

(defn- xml-lang
  [name]
  {:language name :blank? s/blank? :comment? xml-comment?
   :multi-start? xml-start-comment?
   :multi-end? xml-end-comment?})

(defn- elm-lang
  [name]
  {:language name :blank? s/blank? :comment? elm-comment?
   :multi-start? elm-start-comment?
   :multi-end? elm-end-comment?})

(def assembly-lang
  {:language "Assembly" :blank? s/blank?
   :comment? (partial comment? #"^\s*;")
   :multi-start? cstyle-start-comment?
   :multi-end? cstyle-end-comment?})

(def asp-lang
  {:language "ASP.NET" :blank? s/blank?
   :comment? (partial comment? #"^\s*<%.*%>")
   :multi-start? (partial comment? #"<%")
   :multi-end? (partial comment? #"%>")})

(defn- dash-lang
  [name]
  {:language name :blank? s/blank?
   :comment? dash-comment?})

(defn- lisp-lang
  [name]
  {:language name :blank? s/blank? :comment? clj-comment?})

(def rule-map
  {
   "README" (pure-text "Text")
   "LICENSE" (pure-text "Text")
   "VERSION" (pure-text "Text")
   "Version" (pure-text "Text")

   ".agda"  (haskell-style-lang "Agda")
   ".lagda"  (literate-haskell-style-lang "Literate Agda")

   ".aidl" (cstyle-lang "Android AIDL")

   ".cls"      (cstyle-lang "Apex")
   ".tgr"      (cstyle-lang "Apex")
   ".trigger"  (cstyle-lang "Apex")
   ".component" (xml-lang "Apex")
   ".labels"    (xml-lang "Apex")
   ".layout"    (xml-lang "Apex")
   ".object"    (xml-lang "Apex")
   ".page"      (xml-lang "Apex")

   ".as"   (cstyle-lang "ActionScript")

   ".asm"   assembly-lang
   ".s"     assembly-lang
   ".S"     assembly-lang

   ".asa"   asp-lang
   ".asp"   asp-lang
   ".asax"  asp-lang
   ".ascx"  asp-lang
   ".asmx"  asp-lang
   ".config" asp-lang
   ".master" asp-lang
   "sitemap" asp-lang
   "webinfo" asp-lang

   ".bat"   dos-batch
   ".BAT"   dos-batch
   ".btm"   dos-batch
   ".BTM"   dos-batch
   ".cmd"   dos-batch
   ".CMD"   dos-batch

   ".build" (xml-lang "NAnt Script")

   ".c"     (cstyle-lang "C")
   ".h"     (cstyle-lang "C")

   ".css"   (cstyle-lang "CSS")

   ".cpp"   (cstyle-lang "C++")
   ".cxx"   (cstyle-lang "C++")
   ".C"     (cstyle-lang "C++")
   ".cc"    (cstyle-lang "C++")
   ".pcc"   (cstyle-lang "C++")
   ".c++"   (cstyle-lang "C++")
   ".hpp"   (cstyle-lang "C++")
   ".H"     (cstyle-lang "C++")

   ".clj"   (lisp-lang "Clojure")
   ".cljs"  (lisp-lang "ClojureScript")
   ".cljc"  (lisp-lang "Clojure")
   ".cljx"  (lisp-lang "Clojure")

   ".cmake" (pure-text "CMake")

   ".coffee" (dash-lang "CoffeeScript")

   ".cfm"   (xml-lang "ColdFusion")
   ".cfc"   (xml-lang "ColdFusion")

   ".cs"    (cstyle-lang "C#")
   ".cshtml" (xml-lang "Razor C#")
   ".csproj" (xml-lang ".Net Project File")
   ".vbproj" (xml-lang ".Net Project File")
   ".vcproj" (xml-lang ".Net Project File")
   ".xproj" (xml-lang ".Net Project File")
   ".kproj" (xml-lang ".Net Project File")
   ".wdproj" (xml-lang ".Net Project File")
   ".wixproj" (xml-lang "Wix Project File")
   ".sln"    (pure-text ".Net Solution File")

   ".resx" (xml-lang ".Net Resource File")

   ".stylecop" (xml-lang "StyleCop Settings")

   ".sh"    (dash-lang "Shell Script")
   ".csh"   (dash-lang "C Shell Script")
   ".tcsh"  (dash-lang "C Shell Script")

   ".dart"  (cstyle-lang "Dart")

   ".dtd"   (xml-lang "DTD")

   ".ex"    (dash-lang "Elixir")
   ".exs"   (dash-lang "Elixir")
   ".erl"   (lang-with-%%-comments "Erlang")
   ".hrl"   (lang-with-%%-comments "Erlang")

   ".elm"   (elm-lang "Elm")

   ".enc"   (pure-text "Encoding")
   ".exp"   (dash-lang "Expect")

   ; files for Rational Software Architect models
   ".efx"   (xml-lang "RSA-RTE Model")
   ".emx"   (xml-lang "RSA-RTE Model")

   ".fs"    (ml-lang "F#")
   ".fsi"   (ml-lang "F#")

   ".g4"    (cstyle-lang "ANTLR")

   ".gms"   gams-lang
   ".gpr"   gams-lang

   ".go"    (cstyle-lang "Go")
   ".gsp"   (pure-text "Grails")

   ".gant"  (cstyle-lang "Groovy")
   ".gradle" (cstyle-lang "Groovy")
   ".groovy" (cstyle-lang "Groovy")

   ".hs"    (haskell-style-lang "Haskell")
   ".lhs"   (literate-haskell-style-lang "Literate Haskell")

   ".idr"  (haskell-style-lang "Idris")
   ".lidr"  (literate-haskell-style-lang "Literate Idris")

   ".html"  (xml-lang "HTML")
   ".htm"   (xml-lang "HTML")
   ".tmpl"  (xml-lang "TMPL")
   ".haml"  (xml-lang "HAML")

   ".java"  (cstyle-lang "Java")
   ".fxml"  (xml-lang "JavaFX FXML")

   ".jd" (xml-lang "JavaDoc")

   ".jsp" (xml-lang "JavaServer Pages")
   ".jspf" (xml-lang "JavaServer Pages")
   ".jsf"   (xml-lang "JavaServer Faces")
   ".xhtml" (xml-lang "JavaServer Faces")

   ".less" (cstyle-lang "Less")

   ".f"   fortran-lang
   ".for" fortran-lang
   ".f90" fortran-lang
   ".f95" fortran-lang
   ".f03" fortran-lang
   ".f15" fortran-lang

   ".F"   preprocessed-fortran-lang
   ".FOR" preprocessed-fortran-lang
   ".F90" preprocessed-fortran-lang
   ".F95" preprocessed-fortran-lang
   ".F03" preprocessed-fortran-lang
   ".F15" preprocessed-fortran-lang

   ".js"    (cstyle-lang "JavaScript")
   ".jsx"   (cstyle-lang "JavaScript")
   ".tsx"   (cstyle-lang "TypeScript")
   ".vue"   (cstyle-lang "Vue")
   ".json"  (pure-text "JSON")

   ".kt"    (cstyle-lang "Kotlin")

   ".el"    (lisp-lang "Emacs Lisp")
   ".lisp"  (lisp-lang "Lisp")
   ".lsp"   (lisp-lang "Lisp")
   ".sc"    (lisp-lang "Lisp")

   ".lua"   (lua-style-lang "Lua")

   ".m"     (cstyle-lang "Objective C")
   ".mm"    (cstyle-lang "Objective C++")

   "Makefile" (pure-text "Makefile")
   ".mk"   (pure-text "Makefile")

   ".ml"    (ml-lang "OCaml")
   ".mli"   (ml-lang "OCaml")
   ".mll"   (ml-lang "OCaml")
   ".mly"   (ml-lang "OCaml")

   ".oden"  (cstyle-lang "Oden")

   ".pl"    perl-lang
   ".pm"    perl-lang

   ".pp"    pascal-lang
   ".pas"   pascal-lang

   ".pde"   (cstyle-lang "Processing")

   ".php"  php-style

   ".pom" (xml-lang "Maven POM")

   ".purs" (haskell-style-lang "PureScript")

   ".py"    {:language "Python" :blank? s/blank? :comment? python-comment?}

   ".rb"   (ruby-style "Ruby")
   ".erb"  (xml-lang "Embedded Ruby")
   "Gemfile" (ruby-style "Gemfile")
   "Gemfile.lock" (ruby-style "Gemfile")
   ".tt"   (pure-text "Thor Template")
   ".rake" (ruby-style "Rake")
   ".rdoc" (ruby-style "Rake")

   ".spec" (ruby-style "Spec")
   ".rules" (ruby-style "Spec rules")

   ".rkt"  racket-lang

   ".r"    (dash-lang "R")

   ".rs" (cstyle-lang "Rust")

   ".scala"  (cstyle-lang "Scala")
   ".check"  (pure-text "Text")
   ".targets" (pure-text "Text")
   ".expected" (pure-text "Text")

   ".scss" (cstyle-lang "Sass")
   ".styl" (cstyle-lang "Stylus")

   ".sql"    sql-lang
   ".psql"   sql-lang
   ".plsql"  sql-lang

   ".prc"    sql-lang
   ".fnc"    sql-lang
   ".tab"    sql-lang
   ".trg"    sql-lang
   ".seq"    (specific-sql-lang "SQL seq")
   ".bdy"    (specific-sql-lang "SQL bdy")

    ".swift"    (cstyle-lang "Swift")

   ".ps1" (dash-lang "PowerShell")

   ".tcl"    (dash-lang "TCL")
   ".itk"    (dash-lang "TCL")
   ".tk"     (dash-lang "TCL")

   ".ts"    (cstyle-lang "TypeScript")

   ".txt"    (pure-text "Text")
   ".md"     (pure-text "Markdown")
   ".markdown" (pure-text "Markdown")
   ".csv"    (pure-text "CSV")
   ".tsv"    (pure-text "TSV")
   ".properties" (pure-text "Properties")

   ".vb"    {:language "VB" :blank? s/blank? :comment? cstyle-comment? :commented-line? cstyle-single-line-comment}

   ".xml"   (xml-lang "XML")
   ".xsd"   (xml-lang "XSD")
   ".xslt"  (xml-lang "XSLT")
   ".xsl"   (xml-lang "XSL")

    ".yml" (dash-lang "YAML")
    ".yaml" (dash-lang "YAML")
   })

(defn- lookup-with-fallback
  "Some extensions are case insensitive (*.h and *.H) so we
   attempt a lookup by case first. If we fail, we make a second
   case insensitive match. A failure there is propagated as a
   non-match to the caller."
  [extension]
  (if-let [r (rule-map extension)]
    r
    (some-> extension
            clojure.string/lower-case
            rule-map)))

(defn rule-for
  [extension]
  (lookup-with-fallback extension))

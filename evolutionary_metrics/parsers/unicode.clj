(ns evolutionary-metrics.parsers.unicode
  (:require [clojure.string :as string]
            [taoensso.timbre :as log]))

(defn unescape-octals
  "Takes a string with octal values prepended by an escaped backslash, ie \"\\303\\251\",
  and turns that into proper octal escapes, ie \"\303\251\"."
  [s]
  (letfn [(str-of-digits->char [group-matches]
            ;; only one group is used, which will be the second element in the vector
            ;; (see re-groups)
            (Character/toString (char (Integer/valueOf (second group-matches) 8))))]
    (string/replace s #"\\(\d{3})" str-of-digits->char)))

(defn quotes-present?
  "When filenames contain non-ASCII characters, git puts quotes around
  them in the log. This function returns true when they are present,
  either in a single path name or in a rename pattern.

  There are four cases where quoted paths are present:
  - \"a single quoted path\"
  - \"a rename from a quoted path\" => to an unquoted path
  - a rename from an unquoted path => \"to a quoted path\"
  - \"a rename from a quoted path\" => \"to a quoted path\"

  Finding a quote as the first or the last character of the path name
  handles all these cases."
  [s]
  (or (string/starts-with? s "\"")                          ;leading quote
      (string/ends-with? s "\""))                           ;trailing quote
  )

(defn trim-quotes-simple [s]
  (-> s
      (string/replace #"^\"" "")
      (string/replace #"\"$" "")))

(defn trim-quotes
  "When file names contain non-ASCII characters, Git puts quotes around
  them in the log. This function removes them when they are present in
  each of these four cases:

  - \"path\"
  - \"path\" => \"path\"
  - \"path\" => path
  - path => \"path\"

  It leaves the input unchanged if any of these cases:

  - path => path
  - path
   "
  [s]
  (let [renaming-both-quoted (re-pattern "^\"(.+)\"( => )\"(.+)\"$")
        renaming-first-quoted (re-pattern "^\"(.+)\"( => .*)$")
        renaming-last-quoted (re-pattern "^(.* => )\"(.+)\"$")
        renaming-none-quoted (re-pattern "^.+ => .+$")
        single (re-pattern "^\"(.+)\"$")
        dbg (fn [x s] (log/tracef "%s: <%s>" x s) s)]
    (cond
      ;; first check all renaming patterns

      (re-matches renaming-both-quoted s)                   ;quotes around first and second path
      (dbg "both" (string/replace s renaming-both-quoted "$1$2$3"))

      (re-matches renaming-first-quoted s)                  ;quotes around first path
      (dbg "first" (string/replace s renaming-first-quoted "$1$2"))

      (re-matches renaming-last-quoted s)                   ;quotes around second path
      (dbg "last" (string/replace s renaming-last-quoted "$1$2"))

      (re-matches renaming-none-quoted s)                   ;no quotes around any path
      (dbg "none" s)

      ;; finally check if plain path, ie no renaming pattern
      (re-matches single s)                                 ;single path, leading and trailing quote
      (dbg "single" (string/replace s single "$1"))

      :else                                                 ;otherwise leave unchanged
      (dbg "else" s)
      )))

(defn compose
  "This function takes a string possibly containing 3-digit octal UTF-8 values prepended by an escaped backslash,
  returning a string where those octal values have been composed into their Unicode character.
  Characters with higher codepoints, requiring surrogate pairs, will be handled correctly.
  See https://en.wikipedia.org/wiki/UTF-16 for information on surrogate pairs.

  For example, \"r\\\\303\\\\251l\\\\303\\\\262\\\\360\\\\237\\\\221\\\\215y\" is turned into \"rélò\uD83D\uDC4Dy\".

  Note that any non-ASCII characters in the string MUST be properly encoded with octal values in
  order to return a correct result. An input string \"rélòy\" will NOT be correctly decoded into
  \"rélòy\". Only the input string \"r\\\\303\\\\251l\\\\303\\\\262y\" will be.

  Use case
  The git-log documentation (rather briefly) states:

    Path names are encoded in UTF-8 normalization form C.
    https://git-scm.com/docs/git-log#_discussion

  For reference, the various Unicode normalization forms are described here:
  https://unicode.org/reports/tr15/

  A path that looks like this in the repository:

    packages/a-néw-dìrectòry/hello

  will be transformed into this in the git log output:

    \"packages/a-n\\303\\251w-d\\303\\254rect\\303\\262ry/hello\"

  This function performs the following steps:

  1. Unescape octals
  2. Coerce each character into an int
  3. Turn the sequence of ints into an array of bytes
  4. Create a string from the byte array

  Unescape octals
  When the path has been read into a Java string, it will contain sequences of 3-digit octal values
  prepended by an escaped backslash, for example: \"\\\\303\\\\251\". As a pre-processing step,
  each of these 3-digit sequences are replaced by the actual octal representation, for example
  \"\\303\\251\". This makes it possible to treat all characters in the string equally in the next
  step.

  The way this is done is by using the fact that Clojure's string/replace can take a function
  (rather than a string) as its replace value. The given function is called with the match of the
  regular expression (the result of calling re-groups, actually). In our case, the regular
  expression looks for 3-digit sequences prepended by a backslash, where the digit sequence is the
  only group. The function gets the single matching group by calling second on its argument (the
  first is the whole match), thus having access to a 3-digit string. This is first turned into an
  octal-based integer, which is then turned into a character, replacing the original sequence with
  the actual escaped value.

  Coerce each character into an int
  This is just a mapping of the int function over the characters in the string.

  Turn the sequence of ints into an array of bytes
  This is just a call to the byte-array function.

  Create a String from the byte array
  This step just calls the String constructor with the byte array from the previous step."
  [s]
  (->> s
       unescape-octals
       (map int)
       byte-array
       (String.)))

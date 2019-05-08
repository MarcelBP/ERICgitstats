(ns evolutionary-metrics.app.glob-patterns)

;;; based on code from here: https://github.com/Raynes/fs/blob/master/src/me/raynes/fs.clj
(defn glob->regex
  "Takes a glob-format string and returns a regex string."
  [s]
  (loop [stream s
         re ""
         curly-depth 0]
    (let [[c j] stream]
        (cond
         (nil? c) (re-pattern
                    ; We add ^ and $ since we check only for file names
                    (str "^" (if (= \. (first s)) "" "(?=[^\\.])") re "$"))

         (= c \\) (recur (nnext stream) (str re c c) curly-depth)
         (= c \/) (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                         curly-depth)
         (and (= c \*) (= j \*)) (recur (nnext stream) (str re ".*") curly-depth)
         (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
         (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
         (= c \{) (recur (next stream) (str re \() (inc curly-depth))
         (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
         (and (= c \,) (< 0 curly-depth)) (recur (next stream) (str re \|)
                                                 curly-depth)
         (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c)
                                                  curly-depth)
         :else (recur (next stream) (str re c) curly-depth)))))

(defn glob-match [pattern path]
  (let [compiled-pattern (re-pattern (glob->regex pattern))]
    (re-matches compiled-pattern path)))

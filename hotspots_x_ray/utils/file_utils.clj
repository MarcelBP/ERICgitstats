(ns hotspots-x-ray.utils.file-utils
  "Handy utility functions for dealing with files input.
  Important part is using proper encoding and BOM char handling for reading data from files.
  This is especially useful for ANTLR parsers."
  (:require [clojure.java.io :as io])
  (:import (java.io FileReader
                    FileNotFoundException
                    InputStreamReader)
           (java.nio.charset Charset)
           (org.mozilla.universalchardet UniversalDetector)
           (net.pempek.unicode UnicodeBOMInputStream)))

(def default-encoding (or
                       (some-> (Charset/defaultCharset) .name)
                       "UTF-8"))

(defn autodetect-file-encoding
  "Tries to auto-detect file's encoding returning default java system's charset or UTF-8
  if not possible."
  [file-path]
  (try 
    (some-> file-path
            io/as-file
            UniversalDetector/detectCharset)
    (catch java.nio.file.NoSuchFileException e
      (throw (java.io.FileNotFoundException. (str "File not found: " file-path))))))

(defn reader-with-autodetected-encoding
  "Returns file reader with auto-detected encoding.
  See `autodetect-file-encoding`.

  Note: uses `UnicodeBOMInputStream` internally to make sure that files with leading Byte Order Mark
  are handled properly.
  There are cases when ANTLR isn't able to deal with BOM chars therefore we need to wrap input stream."
  [file-path]
  (let [encoding (or (autodetect-file-encoding file-path)
                     default-encoding)
        bom-is (-> file-path
                   io/input-stream
                   UnicodeBOMInputStream.)]
    (.skipBOM bom-is) ; skip Byte-order mark to not confuse ANTLR parser with funny BOM character
    (io/reader bom-is :encoding encoding)))


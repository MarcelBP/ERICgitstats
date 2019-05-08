;; Copyright (c) 2014-2016 Andrey Antukh <niwi@niwi.nz>
;;
;; Licensed under the Apache License, Version 2.0 (the "License")
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Links to rfcs:
;; - https://tools.ietf.org/html/rfc1951

(ns buddy.util.deflate
  "Interface to DEFLATE compression algorithm."
  (:import java.io.ByteArrayInputStream
           java.io.ByteArrayOutputStream
           java.util.zip.Deflater
           java.util.zip.DeflaterOutputStream
           java.util.zip.InflaterInputStream
           java.util.zip.Inflater
           java.util.zip.ZipException))

(defn compress
  "Given a plain byte array, compress it and
  return an other byte array."
  ([^bytes input]
   (compress input nil))
  ([^bytes input {:keys [nowrap] :or {nowrap true}}]
   (let [os (ByteArrayOutputStream.)
         defl (Deflater. Deflater/DEFLATED nowrap)]
     (with-open [dos (DeflaterOutputStream. os defl)]
       (.write dos input))
     (.toByteArray os))))

(defn uncompress
  "Given a compressed data as byte-array,
  uncompress it and return as an other
  byte array."
  ([^bytes input]
    (uncompress input nil))
  ([^bytes input {:keys [nowrap buffer-size]
                  :or {nowrap true buffer-size 2048}
                  :as opts}]
   (let [buf  (byte-array (int buffer-size))
         os   (ByteArrayOutputStream.)
         inf  (Inflater. ^Boolean nowrap)]
     (try
       (with-open [is  (ByteArrayInputStream. input)
                   iis (InflaterInputStream. is inf)]
         (loop []
           (let [readed (.read iis buf)]
             (when (pos? readed)
               (.write os buf 0 readed)
               (recur)))))
       (.toByteArray os)
       (catch ZipException e
         (if nowrap
           (uncompress input (assoc opts :nowrap false))
           (throw e)))))))

(ns matasano-clj.challenge-one
  (:import java.util.Base64))

;--------- Challenge 1 ---------------
; example data ;;;;;;;;;;;;;
(def hexvals "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(def expected "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
(def ascii-ex "I'm killing your brain like a poisonous mushroom")

; functions ;;;;;;;;;;
(defn hex->ascii [hex]
  (apply str
         (map
           (fn [[x y]] (char (Integer/parseInt (str x y) 16)))
           (partition 2 hex))))

(defn ascii->hex [s]
  (apply str
         (map #(format "%02x" (int %)) s)))

(defn encodeb64 [to-encode]
  (.encodeToString (Base64/getEncoder) (.getBytes to-encode)))

(defn decodeb64 [to-decode]
  (String. (.decode (Base64/getDecoder) to-decode)))

(defn hex->b64 [hexvals]
  (-> hexvals
      hex->ascii
      encodeb64))

(defn b64->hex [b64]
  (-> b64
      decodeb64
      ascii->hex))
;--------------- end -----------------


;---------- Challenge 2 ---------------

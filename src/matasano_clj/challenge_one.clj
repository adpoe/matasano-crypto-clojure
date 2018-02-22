(ns matasano-clj.challenge-one
  (:import java.util.Base64))

;--------- Challenge 1 ---------------
; example data
(def hexvals "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(def expected "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
(def ascii-ex "I'm killing your brain like a poisonous mushroom")

; functions
(defn hex->ascii [hex]
  "Transform a string of all hex values into a string of all ascii values."
  (apply str
         (map
           (fn [[x y]] (char (Integer/parseInt (str x y) 16)))
           (partition 2 hex))))

(defn ascii->hex [s]
  "Transform a string of all ascii values into string of all hex values."
  (apply str
         (map #(format "%02x" (int %)) s)))

(defn encodeb64 [to-encode]
  "Encode an ascii string into Base64."
  (.encodeToString (Base64/getEncoder) (.getBytes to-encode)))

(defn decodeb64 [to-decode]
  "Decode a Base64 string into ascii."
  (String. (.decode (Base64/getDecoder) to-decode)))

(defn hex->b64 [hexvals]
  "Transform a hex string to Base64."
  (-> hexvals
      hex->ascii
      encodeb64))

(defn b64->hex [b64]
  "Transform a Base64 string to hex."
  (-> b64
      decodeb64
      ascii->hex))
;--------------- end -----------------


;---------- Challenge 2 ---------------

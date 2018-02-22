(ns matasano-clj.challenge-one
  (:import java.util.Base64))

;--------- Challenge 1 ---------------
(def hexvals "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(def expected "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
(def ascii-ex "I'm killing your brain like a poisonous mushroom")

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
(def xor-a "1c0111001f010100061a024b53535009181c")
(def xor-b "686974207468652062756c6c277320657965")
(def xor-c "746865206b696420646f6e277420706c6179")
(def ch2-expected-ascii "the kid don't play")

(defn zip [& colls]
  "Mirrors the `zip` function in Haskell."
  (partition (count colls) (apply interleave colls)))

(defn xor-strs [s1 s2]
  "XOR 2 hex strings, and return their ascii value."
  (let [a1 (map int (hex->ascii s1))
        a2 (map int (hex->ascii s2))
        tups (zip a1  a2)]
    (apply str
           (map
             (fn [[x y]] (char (bit-xor x y)))
             tups))))

(defn xor-strs' [s1 s2]
  "XOR 2 hex strings and return their hex value."
  (ascii->hex (xor-strs s1 s2)))

(xor-strs xor-a xor-b)
(xor-strs' xor-a xor-b)
;--------------- end -------------


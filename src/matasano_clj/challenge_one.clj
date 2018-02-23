(ns matasano-clj.challenge-one
  (:import java.util.Base64)
  (require [clojure.string :as s]))

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
        tups (zip a1 a2)]
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


;--------- Challenge 3 ---------
(def ch3-cipher "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
(def ascii-chars (into [] (map char (range 32 127))))

(def letter-freqs
  "The most commonly used letters in English language, ordered."
  (s/lower-case "ETAOIN SHRDLU"))

(def letter-scores
  "A mapping from most commonly used letters to a score."
  (zipmap
    letter-freqs
    (range (count letter-freqs))))

(defn score-text [text]
  "Assign a numerical value to text indicating likelihood it represents written English."
  (let [norm-txt (s/lower-case text)]
    (apply +
           (remove nil?   ; drop resulting nil from any failed lookups
                   (map
                      (partial get letter-scores)
                      norm-txt)))))

(defn match-length [text c]
  "Given a character and some text, create a string == to the same length
  of the text, containing *only* the target char."
  (apply str (repeat (count text) c)))

(defn get-single-byte-ciphers [ciphertext]
  "Get all possible ascii single-byte ciphers of same length as target ciphertext."
  (map (partial match-length ciphertext)
        ascii-chars))

(defn get-xored-candidates [ciphertext candidates]
  "XOR all candidate single-byte ciphers with target ciphertext."
  (map (partial xor-strs' ciphertext)
       (map ascii->hex candidates)))

(defn get-score-map [char-list xored-candidates]
  "Index each possible single-byte cipher character with its score."
  (zipmap char-list
        (map score-text
             (map hex->ascii
                  xored-candidates))))

(defn single-byte-xor [text]
  "Given a text encoded with a single-byte xor cipher,
  find the most likely character encodings and decoded text.

  Return the most likely candidates, in descending order of likelihood."
  (let [candidates (get-single-byte-ciphers text)
        xored-candidates (get-xored-candidates text candidates)
        score-map (get-score-map ascii-chars xored-candidates)]
    (sort-by val > score-map)))

(defn get-n-most-likely-candidates [n ciphertext]
  "Get the N most likely ascii chars encoding a single-byte XOR cipher."
  (take n (single-byte-xor ciphertext)))

(defn decode-single-byte-xor [ciphertext ch]
  "Given a single character and ciphertext, decode the ciphertext as a single byte XOR cipher."
  (hex->ascii (xor-strs' ciphertext
                         (ascii->hex (match-length ciphertext ch)))))

(defn get-n-most-likely-decodings [n ciphertext]
  "Return plaintextof the N most likely single-byte XOR decodings
  of a given ciphertext."
  (let [n-most-likely (map first (take n (single-byte-xor ch3-cipher)))]
      (map (partial decode-single-byte-xor ciphertext)
         n-most-likely)))

(get-n-most-likely-candidates 5 ch3-cipher)
(get-n-most-likely-decodings 5 ch3-cipher)

(defn single-byte-xor' [n ciphertext]
  "Get a mapping of the N most likely ascii chars encoding
  a single byte XOR on the target ciphertext,
  along with the decoding."
  (zip (get-n-most-likely-candidates n ch3-cipher)
       (get-n-most-likely-decodings n ch3-cipher)))

(single-byte-xor' 5 ch3-cipher)

;'(([\_ 132] "Dhhlni`'JD t'knlb'f'whric'ha'efdhi")
;   ([\X 118] "Cooking MC's like a pound of bacon")  <= the winner.
;   ([\D 104] "_sswur{<Q_;o<puwy<}<lsirx<sz<~}sr")
;   ([\d 104] "SSWUR[qOPUWY]LSIRXSZ^]_SR")
;   ([\[ 98] "@llhjmd#N@$p#ojhf#b#slvmg#le#ab`lm"))
;-------------- end ------------------

(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(def letters
  "Alphabet used later on for decoding/encoding of the message"
  (map char (range (int \a) (inc (int \z)))))

(defn full-cipher
  "Return full cipher - cipher repeated till message length is met."
  [cipher message]
  (let [cipher-len (count cipher)
        msg-len (count message)
        to-take (Math/ceil (/ msg-len cipher-len))
        taken (take to-take (repeat cipher))]
    (apply str (take msg-len (apply concat taken)))))

(defn- encode-pair
  "Encode single-letter (m)essage given (k)ey."
  [k m]
  (let [offset 97 ;; letter 'a' offset
        index (- (int k) offset)
        split-pos (- (int m) offset)]
    (nth (apply concat (reverse (split-at split-pos letters))) index)))

(defn encode
  "Encode a message with given keyword."
  [keyword message]
  (let [cipher (full-cipher keyword message)
        msg (str/lower-case message)]
    (apply str (map encode-pair cipher msg))))

(defn decode
  "Decode encoded message with given keyword."
  [keyword message]
  (let [cipher (full-cipher keyword message)]
    (apply str (map decode-pair cipher message))))

(defn- decode-pair
  "Decode pair takes a different approach than encode-pair: mod is
  used instead of cipher generation."
  [k m]
  (let [offset 97
        kval (- (int k) offset)
        mval (- (int m) offset)
        res (mod (- mval kval) (count letters))]
    (char (+ offset res))))

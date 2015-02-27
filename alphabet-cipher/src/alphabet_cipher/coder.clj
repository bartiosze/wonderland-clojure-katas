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
        to-take (Math/ceil (/ msg-len cipher-len))]
    (take msg-len (apply concat (take to-take (repeat cipher))))))

(defn- encode-letters [k m]
  (let [offset 97 ;; letter 'a' offset
        index (- (int k) offset)
        split-pos (- (int m) offset)]
    (nth (apply concat (reverse (split-at split-pos letters))) index)
    ;;[k index]
    ))

(defn encode [keyword message]
  (let [cipher (apply str (full-cipher keyword message))
        msg (str/lower-case message)]
    (apply str (map encode-letters cipher msg))))

(defn decode [keyword message]
  "decodeme")


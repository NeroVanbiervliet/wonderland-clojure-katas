(ns alphabet-cipher.coder)

(def alphabet (map char (range 97 123)))

(defn char-to-index [char]
 (-> (int char) (- 97)))

(defn rot-seq [coll rot]
  (if (= rot 0)
    coll
    (recur (-> (rest coll)
                         vec
                         (conj (first coll)))
           (dec rot))))

(defn encode-char [key-char msg-char]
  (-> alphabet
      (rot-seq (char-to-index key-char))
      (nth (char-to-index msg-char))))

(defn pad-key [key length]
  (->> key
       repeat ; produces lazy seq
       (apply concat) ; apply str would not work, as it does not return a lazy seq but tries to consume all inputs
       (take length)))

(defn encode [keyword message]
  (let [padded-key (pad-key keyword (count message))]
    (->> (map encode-char padded-key message)
        (apply str))))

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")


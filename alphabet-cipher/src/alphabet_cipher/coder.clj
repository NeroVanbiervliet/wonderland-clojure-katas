(ns alphabet-cipher.coder)

; NEED idea: whole rotation of seqs is not necessary
; you can do all computations in ints and modulo
; then convert to a character when needed
; e.g int(enc) = int(msg) + int(key) % 26

(def alphabet (map char (range 97 123)))

(defn to-int [char_]
 (-> (int char_) (- 97)))

(defn to-char [int_]
  (-> int_ (+ 97) char))

; only positive rotations
(defn rot-seq [coll rot]
  (if (= rot 0)
    coll
    (recur (-> (rest coll)
                         vec
                         (conj (first coll)))
           (dec rot))))

(defn encode-char [key-char msg-char]
  (-> alphabet
      (rot-seq (to-int key-char))
      (nth (to-int msg-char))))

(defn decode-char [key-char msg-char]
  (-> alphabet
      (rot-seq (to-int key-char))
      (.indexOf msg-char)
      (to-char)))

(defn pad-key [key length]
  (->> key
       repeat ; produces lazy seq
       (apply concat) ; apply str would not work, as it does not return a lazy seq but tries to consume all inputs
       (take length)))

(defn unpad-key [padded-key]
  (->> (count padded-key)
       (range 1) ; all possible key lengths
       (map #(->> (partition % padded-key)
                  doall
                  (map (fn [c] (apply str c)))))
       (filter #(= (first %) (second %)))
       last ; last = problem for scones (sconesscones), first is problem for abcabcx (abc)
       first))

(defn encode [keyword message]
  (let [padded-key (pad-key keyword (count message))]
    (->> (map encode-char padded-key message)
         (apply str))))

(defn decode [keyword message]
  (let [padded-key (pad-key keyword (count message))]
    (->> (seq message)
         (map decode-char padded-key)
         (apply str))))

(defn decipher [cipher message]
  (->
   (decode message cipher) ; e.g. sconessconesscon
   unpad-key))

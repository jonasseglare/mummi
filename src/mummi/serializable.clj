(ns mummi.serializable
  (:require [mummi.common :refer [make-symbol map-with-keys? defprotocol2]]))


;; A special protocol for serialization.

(defprotocol2 Serializable
  "Should serialize an object"
  (serialize [this]))

(defprotocol2 Deserializable
  "Should deserialize an object"
  (deserialize [this]))


(defmacro extend-id-ser [& typenames]
  (let [x (gensym)]
    `(do
       ~@(map
          (fn [typename]
            `(extend-type ~typename
               Serializable
               (serialize [~x] ~x)
               Deserializable
               (deserialize [~x] ~x)))
          typenames))))

(defmacro exclude-ser [& typenames]
  (let [x (gensym)]
    `(do
       ~@(map
          (fn [typename]
            `(extend-type ~typename
               Serializable
               (serialize [~x]
                 (throw (Error. (str "Unable to serialize " ~typename))))
               Deserializable
               (deserialize [~x]
                 (throw (Error. (str "Unable to deserialize " ~typename))))))
          typenames))))

  

(extend-id-ser java.lang.Object)
(extend-id-ser nil)

(exclude-ser clojure.lang.Atom
             clojure.lang.Fn)


 

        
      

(extend-type clojure.lang.IPersistentMap
  Serializable
  (serialize [this]
    (merge
     this
     (zipmap
      (map serialize (keys this))
      (map serialize (vals this)))))
  Deserializable
  (deserialize [this]
    (merge
     this
     (zipmap
      (map deserialize (keys this))
      (map deserialize (vals this))))))

(extend-type clojure.lang.IPersistentVector
  Serializable
  (serialize [this]
    (vec (map serialize this)))
  Deserializable
  (deserialize [this]
    (vec (map deserialize this))))

(extend-type clojure.lang.ISeq
  Serializable
  (serialize [this]
    (seq (map serialize this)))
  Deserializable
  (deserialize [this]
    (seq (map deserialize this))))

(extend-type clojure.lang.IPersistentSet
  Serializable
  (serialize [this]
    (set (map serialize this)))
  Deserializable
  (deserialize [this]
    (set (map deserialize this))))

(defn serialized [x]
  (assert (symbol? x))
  (make-symbol "Serialized" x))

(defn serialized-constructor [x]
  (make-symbol (serialized x) "."))

(defn is-defser-fun [x sym]
  (if (seq? x)
    (let [funname (first x)]
      (if (symbol? funname)
        (if (= (name sym) (name funname))
          (let [args (second x)]
            (if (vector? args)
              (if (and (symbol? (first args))
                       (= 1 (count args)))
                {:args args
                 :body (rest (rest x))}))))))))
                

; Should return a function definition
; for a function that takes a map and returns
; the serialized type with the fields from the
; map
(defn make-constructor [fun-name typename fields]
  (let [src (gensym)]
    `(defn ~fun-name [~src]
       (assert (map-with-keys? ~src ~@(map keyword fields)))
       (~(serialized-constructor typename)
        ~@(map (fn [fsym]
                 `(~(keyword fsym) ~src))
               fields)))))

(defn make-serializer [constructor serializer typename serialized-fields]
  (let [parsed (is-defser-fun serializer 'serialize)]
    (assert
     parsed
     "The serializer should be on the form (serialize [obj-to-serialize] ...) and
return a map with the serialized keys")
    `(extend-type
         ~typename Serializable
         (serialize ~(:args parsed)
           (~constructor (do ~@(:body parsed)))))))

(defn make-deserializer [deserializer typename]
  (let [result (gensym)
        parsed (is-defser-fun deserializer 'deserialize)]
    (assert parsed "The deserializer should be on the form (deserialize [serialized-obj] ...) and return the deserialized object")
    `(extend-type ~(serialized typename)
       Deserializable
       (deserialize ~(:args parsed)
         (let [~result (do ~@(:body parsed))]
           (assert (instance? ~typename ~result))
           ~result)))))
          

(defmacro defser [typename

                  ; A vector of symbols
                  serialized-fields

                  ; Should return a map
                  ; of the serialized fields, given
                  ; the objects
                  serializer

                  ; Should return the object
                  ; built from a map of serialized fields.
                  deserializer]
  (let [src (gensym)
        constructor (gensym)
        destructor (gensym)]
    
    `(do
       (defrecord ~(serialized typename)
           ~serialized-fields)

       ~(make-constructor constructor typename serialized-fields)

       ~(make-serializer constructor serializer typename serialized-fields)
       ~(make-deserializer deserializer typename)
       )))

           



(defrecord Test [name age])

(defser Test [ser-name ser-age]
  (serialize [x] {:ser-name (:name x) :ser-age (:age x)})
  (deserialize [x] (Test. (:ser-name x)
                          (:ser-age x))))
  

(comment
  
  (do



    (def x (Test. "Mummi" 28))
    (def y (serialize x))
    (assert (instance? SerializedTest y))
    (def z (deserialize y))
    (assert (= x z))


    (def mx {:a x})
    (assert (= (deserialize (serialize mx))
               mx))

    (def sx (list x))
    (assert (= (deserialize (serialize sx))
               sx))

    (def vx [x])
    (assert (= (deserialize (serialize vx))
               vx))

    (def setx #{x})
    (assert (= (deserialize (serialize setx))
               setx))
    
    )


  
)

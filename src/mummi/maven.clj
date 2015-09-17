(ns mummi.maven)

;; HUR MAN GÖR EN LOKAL JAR
;; 1. Kör detta istället
;; https://gist.github.com/stuartsierra/3062743
;;   MED RÄTT VERSIONSNUMMER!!!
;; mvn deploy:deploy-file -Dfile=selenium-java-2.44.0.jar -DartifactId=selenium-java -Dversion=2.44.0 -DgroupId=selenium-java -Dpackaging=jar -Durl=file:repo

;; 2. Sedan, i projektfilen:
;; 	:repositories {"local" "file:repo"}
;; 	:dependencies [[org.clojure/clojure "1.6.0"]
;; 					 [selenium-java "2.44.0"]])
;;
;; Se make-lein-deps och make-maven-calls för hur man
;; genererar anrop



;; "/home/jonas/programmering/clojure/myfbot/seleniumlibs"
; make-maven-calls
; make-deps

;; (def files (file-seq (clojure.java.io/file "/home/jonas/programmering/clojure/myfbot/seleniumlibs")))
;; #'user/files
;; user> (count files)

(defn to-files-if-not-already [x]
  (if (string? x)
    (file-seq (clojure.java.io/file x))
    x))

(defn split-filename [filename]
  (if (not (.endsWith filename ".jar"))
    (throw (Exception. (str filename " does not end with .jar")))
    (let [dash-index (.lastIndexOf filename "-")
          len (.length filename)]
      (if (= dash-index -1)
        (do (println "PLEASE RENAME YOUR JAR FILES PROPERLY") (assert false))
        ;[(.substring filename 0 (- len 4))
        ; nil]
        [(.substring filename 0 dash-index)
         (.substring filename (+ 1 dash-index) (- len 4))]
        ))))

(defn make-valid-version [version]
  (if (not version) "0.0.1" version))

(defn get-name-and-version [filename]
  (let [[name version] (split-filename filename)]
    [name (make-valid-version version)]))

(defn make-maven-call [full-file]
  (assert (instance? java.io.File full-file))
  (let [full-path (.getAbsolutePath full-file)
        filename (.getName full-file)
        [name version] (get-name-and-version filename)]
    (str "mvn deploy:deploy-file -Dfile="
         full-path
         " -DartifactId=" name
         " -Dversion=" version
         " -DgroupId=" name
         " -Dpackaging=jar -Durl=file:repo")))

(defn make-lein-dep [file]
  (assert (instance? java.io.File file))
  (let [filename (.getName file)
        [name-str version] (get-name-and-version filename)]
    (println filename)
    [(symbol name-str) version]))
    

(defn files-to-string [all-files file-to-string]
  (loop [files all-files
         result ""]
    (if (empty? files)
      result
      (recur
       (next files)
       (let [file (first files)]
         (if (.isFile file)
           (str result (file-to-string file) "\n")
           result))))))

;; Example:
;;
;; Call (println (make-maven-calls "/home/jonas/programmering/clojure/myfbot/seleniumlibs"))
(defn make-maven-calls [all-files]
  (files-to-string (to-files-if-not-already all-files) make-maven-call))


;; (make-lein-deps "/home/jonas/programmering/clojure/myfbot/seleniumlibs")
;;   no prinln here. Just copy/paste the data structure.
(defn make-lein-deps [all-files]
  (loop [files (to-files-if-not-already all-files)
         result []]
    (if (empty? files)
      result
      (recur
       (next files)
       (let [file (first files)]
         (if (.isFile file)
           (conj result (make-lein-dep file))
           result))))))
          

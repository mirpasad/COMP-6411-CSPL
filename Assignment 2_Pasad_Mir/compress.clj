(ns compress)

;; Compress File
(defn compress-file []
  (print "Enter file name: ")
  (flush)
  (let [filename (read-line)
        base-words (->> (clojure.string/split (slurp "frequency.txt") #"\s+"))]
    (with-open [reader (clojure.java.io/reader filename)]
      (let [compressed-data (atom [])
            punctuation-pattern #"[,.!?()]@_-$&"
            number-pattern #"\b(\d+)\b"]
        (doseq [line (line-seq reader)]
          (let [tokens (clojure.string/split line #"\b|\s+|\b(?=\p{Punct})|\b(?<=\p{Punct})")]
            (doseq [token tokens]
              (if (re-matches punctuation-pattern token)
                (swap! compressed-data conj token)
                (if (re-matches number-pattern token)
                  (swap! compressed-data conj (str "@" token "@"))

                  (let [lowercase-token (clojure.string/lower-case token)
                        index (.indexOf (map clojure.string/lower-case base-words) lowercase-token)]
                    (if (contains? (set base-words) lowercase-token)
                      (swap! compressed-data conj index)
                      (swap! compressed-data conj token))))))))
        (spit (str filename ".ct") (clojure.string/join " " @compressed-data))
        (println "File compressed successfully.")))))


;; Decompress File
(defn decompress-file []
  (print "Enter compressed file name: ")
  (flush)
  (let [filename (read-line)]
    (let [base-words (->> (clojure.string/split (slurp "frequency.txt") #"\s+"))]
      (with-open [reader (clojure.java.io/reader filename)]
        (let [compressed-data (->> (clojure.string/split (slurp filename) #"\s+")
                                   (map #(try (Integer/parseInt %)
                                               (catch java.lang.NumberFormatException _ %))))
              decompressed-words (atom [])]
          (doseq [data compressed-data]
            (if (instance? java.lang.Integer data)
              (let [word (get base-words data)]
                (swap! decompressed-words conj word))
              (swap! decompressed-words conj data)))
          (println "Decompressed text:")
;          (println @decompressed-words)
          (let [decompressed-str (clojure.string/join " " @decompressed-words)
                decompressed-str (clojure.string/replace decompressed-str #"\s+([,.\?!])" "$1")
                decompressed-str (clojure.string/replace decompressed-str #"([\[\(])\s+" "$1")
                decompressed-str (clojure.string/replace decompressed-str #"\s+([\]\)])" "$1")
                decompressed-str (clojure.string/replace decompressed-str #"\s+-\s+" " - ")
                decompressed-str (clojure.string/replace decompressed-str #"\@(\d+)\@" "$1")
                decompressed-str (clojure.string/replace decompressed-str #"\s+([@$])\s+" " $1")
                decompressed-str (clojure.string/replace decompressed-str #"(?<=\.|^)\s*\p{L}" #(clojure.string/upper-case %))
                ]
            (print decompressed-str))
          (println))))))

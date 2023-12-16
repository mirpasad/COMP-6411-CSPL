(ns menu
  (:require [compress :refer [compress-file decompress-file]]))


;; Display the list of files
(defn display-files []
  (let [files (file-seq (java.io.File. "."))]
    (doseq [file files]
      (println (.getName file)))))

;; Display the contents of a file
(defn display-file-contents []
  (print "Enter file name: ")
  (flush)
  (let [filename (read-line)]
    (when-let [content (try (slurp filename)
                           (catch Exception e
                             (println "Invalid file.")))]
      (println content))))


;; Display the menu and handle user input
(defn menu []
  (loop []
    (println)
    (println "Menu:")
    (println "1. Display list of files")
    (println "2. Display file contents")
    (println "3. Compress a file")
    (println "4. Decompress a file")
    (println "5. Exit")
    (print "Enter your choice: ")
    (flush)
    (let [choice (read-line)]
      (case choice
        "1" (do (display-files) (recur))
        "2" (do (display-file-contents) (recur))
        "3" (do (compress-file) (recur))
        "4" (do (decompress-file) (recur))
        "5" (println "Exiting...")
        (do (println "Invalid choice.") (recur))))))

;; Menu function to start the program
(menu)

(ns cloud-descriptor.file-io
  (:require [clojure.java.io :refer :all]
            [clojure.string :as string]))

(defn- get-file
  [path]
  (let [in-file (file path)]
    (when-not (.exists in-file)
      (throw (ex-info (str "File '" path "' doesn't exist")
                      {:path path})))
    in-file))

(defn get-file-contents
  "Call given function with contents of given file"
  [path]
  (-> path
      get-file
      slurp))

(defn delete-dir-recursive!
  "Recursively delete a directory."
  [^java.io.File file]
  (when (.exists file)
    (when (.isDirectory file)
      (run! delete-dir-recursive! (.listFiles file)))
    (delete-file file)))

(defn get-out-dir!
  "Get path of output directory"
  [in-path]
  (let [in-file (get-file in-path)
        filename (string/replace (.getName in-file) #"(\..+)" "")
        path (.getParent (.getAbsoluteFile in-file))
        output-path (str path "/" filename "/")]
    (delete-dir-recursive! (file output-path))
    output-path))

(defn write-to-main-in-dir!
  "Write output to main.tf file in given directory"
  [out-dir output]
  (let [out-file (str out-dir "/main.tf")]
    (make-parents out-file)
    (spit out-file output)))

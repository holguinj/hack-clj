(ns hack-clj.test-utils
  (:require [hack-clj.parse :as parse]))

(def ex-cmp
  (-> "dev-resources/vm/MemoryAccess/BasicTest/BasicTest.cmp"
    parse/file->lines))


(defn parse-cmp
  [cmp-lines]
  {:pre [(zero? (mod (count cmp-lines) 2))]}
  (loop [[head cmp & rest] cmp-lines
         acc {}]
    (let [locs (->> head
                    (re-seq #"RAM\[(\d+)[^\d]")
                    (map second)
                    (map #(Integer/parseInt %)))
          vals (->> cmp
                    (re-seq #" (\d+) ")
                    (map second)
                    (map #(Integer/parseInt %)))
          new-acc (merge acc (zipmap locs vals))]
      (if (empty? rest)
        new-acc
        (recur rest
               new-acc)))))

(defn read-cmp
  [path]
  (-> path
    parse/file->lines
    parse-cmp))

(def ex-tst
  (->> "dev-resources/vm/MemoryAccess/BasicTest/BasicTest.tst"
    parse/file->lines
    parse/strip-comments
    (filter #(.startsWith % "set "))))

(defn parse-tst-line
  [line]
  (let [address (->> line
                  (parse/extract #"RAM\[(\d+)\]")
                  Integer/parseInt)
        val (->> line
              (parse/extract #"(\d+),")
              Integer/parseInt)]
    {address val}))

(defn parse-tst
  [tst-lines]
  (->> tst-lines
    parse/strip-comments
    (filter #(.startsWith % "set "))
    (map parse-tst-line)
    (reduce merge {})))

(defn read-tst
  [path]
  (-> path
    parse/file->lines
    parse-tst))

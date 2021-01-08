(ns spike-for-parsing-xbrl.core
  (:require [clojure.xml :as xml])
  (:gen-class))


(defn xml-seq-of
  [source]
  (->> source
       xml/parse
       xml-seq))

(defn string-to-stream
  [s]
  (->> s
       .getBytes
       java.io.ByteArrayInputStream.))

(defn enclose-with-dummy-tag
  [string]
  (str "<dummy>" string "</dummy>"))

(defn only-elements-tagged-with
  [tags m]
  (filter (comp tags :tag) m))

(defn content-tagged-with
  [tag xml-seq]
  (->> (only-elements-tagged-with #{ tag } xml-seq)
       first
       :content
       first))

(defn contents-tagged-with
  [tag xml-seq]
  (->> (only-elements-tagged-with #{ tag } xml-seq)
       (map :content)))

(defn cleansing-string
  [value]
  (-> (clojure.string/replace value #"^[ －―]+" "")
      (clojure.string/trim)))

(defn content-string [table-data-contents]
  (->> (mapcat (fn [table-data-content]
                 (->> (tree-seq associative? :content table-data-content)
                      (filter string?)
                      (map cleansing-string)))
               table-data-contents)
       (interpose " ")
       (apply str)))

(defn affiliated-company-record-from
  [table-record]
  (->> (contents-tagged-with :td table-record)
       (map content-string)
       (take 5))) ;; 名称、 住所、 資本金、 主な事業内容、 議決権の所有割合 の 5 つ

(defn valid-name?
  [name]
  (and (not (= name "名称"))
       (not (= name "主要な損益情報等"))
       (< 1 (count name))))

(defn valid-capital?
  [capital]
  (or (try
        (->> (re-seq #"\d" capital)
             (apply str)
             Float/parseFloat
             float?)
        (catch Exception e false))
      (re-seq #"[１-９]" capital)))

(defn valid-record?
  [record]
  (let [threshold 4]
    (and (<= threshold (count (filter (complement empty?) record)))
         (let [name (first record)
               capital (nth record 2)]
           (and (valid-name? name)
                (valid-capital? capital))))))

(defn construct-affiliated-company-records
  [edinet-code fiscal-year-end-date records]
  (->> (filter valid-record? records)
       (map #(cons fiscal-year-end-date %))
       (map #(cons edinet-code %))))

(defn as-tsv
  [records]
  (->> (map #(apply str (interpose "\t" %)) records)
       (interpose "\n")
       (apply str)))

(defn affiliated-companies-tsv
  [xbrl-file-name]
  (let [xbrl-seq (xml-seq-of (java.io.File. xbrl-file-name))
        edinet-code (content-tagged-with :jpdei_cor:EDINETCodeDEI xbrl-seq)
        fiscal-year-end-date (content-tagged-with :jpdei_cor:CurrentFiscalYearEndDateDEI xbrl-seq)
        html-seq (->> (content-tagged-with :jpcrp_cor:OverviewOfAffiliatedEntitiesTextBlock xbrl-seq)
                      enclose-with-dummy-tag
                      string-to-stream
                      xml-seq-of)]
    (->> (contents-tagged-with :tr html-seq)
         (map affiliated-company-record-from)
         (construct-affiliated-company-records edinet-code fiscal-year-end-date)
         as-tsv)))

(defn -main
  [& args]
  (let [file-name (first args)]
    (print (affiliated-companies-tsv file-name))))

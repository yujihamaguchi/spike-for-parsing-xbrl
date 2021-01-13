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

(defn first-content-tagged-with
  [tag xml-seq]
  (->> xml-seq
       (only-elements-tagged-with #{ tag })
       first
       :content
       first))

(defn contents-tagged-with
  [tag xml-seq]
  (->> xml-seq
       (only-elements-tagged-with #{ tag })
       (map :content)))

(defn cleansing-string
  [value]
  (-> value
      (clojure.string/replace #"^[ －―]+" "")
      (clojure.string/trim)))

(defn concat-string-of [table-data-contents]
  (->> table-data-contents
       (mapcat (fn [table-data-content]
                 (->> (tree-seq associative? :content table-data-content)
                      (filter string?)
                      (map cleansing-string))))
       (interpose " ")
       (apply str)
       cleansing-string))

(defn table-data-contents
  [table-record]
  (->> table-record
       (contents-tagged-with :td)
       (map concat-string-of))) 

(def ng-words-for-name-address
  ["名称"
   "会社名"
   "売上"
   "損失"
   "資本"
   "損益"
   "資産合計"
   "経常利益"
   "経常損失"
   "当期純利益"
   "当期純損失"
   "純資産額"
   "総資産額"
   "営業収益"])

(defn amount?
  [value]
  (= 0 (count (clojure.string/replace value #"[\d,[１-９]]" ""))))

(defn valid-string-value?
  [value]
  (and (not-any? #(re-seq (re-pattern %) value) ng-words-for-name-address)
       (not (amount? value))))

(defn valid-capital?
  [capital]
  (or (empty? capital)
      (try
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
               address (second record)
               capital (nth record 2)
               description (nth record 3)]
           (and (valid-string-value? name)
                (or (empty? address)
                    (valid-string-value? address))
                (or (empty? description)
                    (valid-string-value? description))
                (valid-capital? capital))))))

(defn construct-affiliated-company-records
  [edinet-code fiscal-year-end-date records]
  (->> records
       (filter valid-record?)
       (map #(cons fiscal-year-end-date %))
       (map #(cons edinet-code %))))

(defn as-tsv
  [records]
  (->> records
       (map #(apply str (interpose "\t" %)))
       (interpose "\n")
       (apply str)))

(defn column-position-named-with
  [company-name-header-string table-data-contents]
  (let [name-haeder-string company-name-header-string
        name-header-pattern (re-pattern (str "^\\s*" company-name-header-string ".*")) ]
    (letfn [(header? [content]
              (some #(re-seq name-header-pattern %) content))]
      (let [header (first (filter header? table-data-contents))
            indexed-header-contents (zipmap (map #(clojure.string/replace % name-header-pattern name-haeder-string)
                                                 header)
                                            (iterate inc 0))]
        (if-not (seq header)
          ##Inf
          (val (find indexed-header-contents name-haeder-string)))))))

(defn affiliated-companies-tsv
  [xbrl-file-name]
  (let [xbrl-seq (xml-seq-of (java.io.File. xbrl-file-name))
        edinet-code (first-content-tagged-with :jpdei_cor:EDINETCodeDEI xbrl-seq)
        fiscal-year-end-date (first-content-tagged-with :jpdei_cor:CurrentFiscalYearEndDateDEI xbrl-seq)
        html-seq (->> (first-content-tagged-with :jpcrp_cor:OverviewOfAffiliatedEntitiesTextBlock xbrl-seq)
                      enclose-with-dummy-tag
                      string-to-stream
                      xml-seq-of)
        table-records (contents-tagged-with :tr html-seq)
        table-data-contents (map table-data-contents table-records)]
    (->> table-data-contents
         (map #(drop (apply min (map (fn [name] (column-position-named-with name table-data-contents)) ["名称" "会社名" "名　　称"])) %))
         (map #(take 5 %)) ;; 名称、 住所、 資本金、 主な事業内容、 議決権の所有割合 の 5 つ
         (construct-affiliated-company-records edinet-code fiscal-year-end-date)
         as-tsv)))

(defn -main
  [& args]
  (let [file-name (first args)]
    (print (affiliated-companies-tsv file-name))))

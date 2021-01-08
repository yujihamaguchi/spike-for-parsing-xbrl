(ns spike-for-parsing-xbrl.core
  (:require [clojure.xml :as xml])
  (:gen-class))

(declare cleansing valid-value? valid-record? construct-affiliated-entity-record construct-affiliated-entity-records)

(defn construct-value
  [values]
  (->> (map cleansing values)
       (interpose " ")
       (apply str)))

(defn only-tags
  [tags m]
  (filter (comp tags :tag) m))

(defn contain-tags?
  [tags m]
  ((comp tags :tag) m))

(defn string-to-stream
  [s]
  (->> s
       .getBytes
       java.io.ByteArrayInputStream.))

(defn contents-tagged-by
  [tag xml-seq]
  (->> xml-seq
       (only-tags #{ tag })
       (map :content)))

(defn as-tsv
  [entities]
  (->> entities
       (map #(apply str (interpose "\t" %)))
       (interpose "\n")
       vec
       ((fn [xs] (conj xs "\n")))
       (apply str)))

(defn xml-sequence-of
  [source]
  (->> source
       xml/parse
       xml-seq))

(defn enclose-with-dummy-tag
  [string]
  (str "<dummy>" string "</dummy>"))

(defn content-tagged-by
  [tag xml-seq]
  (->> xml-seq
       (only-tags #{ tag })
       first
       :content
       first))

(defn affiliated-entities-tsv
  [xbrl-file-name]
  (let [xbrl-seq (xml-sequence-of (java.io.File. xbrl-file-name))
        edinet-code (content-tagged-by :jpdei_cor:EDINETCodeDEI xbrl-seq)
        fiscal-year-end-date (content-tagged-by :jpdei_cor:CurrentFiscalYearEndDateDEI xbrl-seq)
        affiliated-entitiy-seq (->> (content-tagged-by :jpcrp_cor:OverviewOfAffiliatedEntitiesTextBlock xbrl-seq)
                                    enclose-with-dummy-tag
                                    string-to-stream
                                    xml-sequence-of)
        raw-records (->> (contents-tagged-by :table affiliated-entitiy-seq)
                                        (apply concat)
                                        ((fn [table-contents]
                                           (if (some (partial contain-tags? #{:tbody}) table-contents)
                                             (->> (contents-tagged-by :tbody table-contents)
                                                  (apply concat)
                                                  (contents-tagged-by :tr))                                            
                                             (contents-tagged-by :tr table-contents)))))]
    (->> raw-records
         (map construct-affiliated-entity-record)
         (construct-affiliated-entity-records edinet-code fiscal-year-end-date)
         as-tsv)))

(defn -main
  [& args]
  (let [file-name (first args)]
    (print (affiliated-entities-tsv file-name))))

;; 調整するところ
(defn cleansing
  [value]
  (-> (clojure.string/replace value #"^[ －―]+" "")
      (clojure.string/trim)))

(defn valid-value?
  [value]
  (<= 1 (count value)))

(defn valid-record?
  [record]
  (let [threshold 4]
    (and (<= threshold (count (filter valid-value? record)))
         (let [name (first record)
               capital (nth record 2)]
           (and (not (= name "名称"))
                (not (= name "主要な損益情報等"))
                (< 1 (count name))
                (or (try
                      (->> (re-seq #"\d" capital)
                           (apply str)
                           Float/parseFloat
                           float?)
                      (catch Exception e false))
                    (re-seq #"[１-９]" capital)))))))

(defn normalize-value [value]
  (->> (only-tags #{ :p } value)
       (mapcat (fn [paragraph] (filter string? (tree-seq associative? :content paragraph))))))

(defn construct-affiliated-entity-record
  [record]
  (->> (contents-tagged-by :td record)
       (map normalize-value)
       (take 5) ;; 名称、 住所、 資本金、 主な事業内容、 議決権の所有割合 の 5 つ
       (map (fn [values] (filter string? values)))
       (map construct-value)))

(defn construct-affiliated-entity-records
  [edinet-code fiscal-year-end-date records]
  (->> records
       (filter valid-record?)
       (map (fn [r] (cons fiscal-year-end-date r)))
       (map (fn [r] (cons edinet-code r)))))

#_(affiliated-entities-tsv "ng/r.xbrl")


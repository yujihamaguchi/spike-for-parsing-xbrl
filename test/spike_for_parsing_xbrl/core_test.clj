(ns spike-for-parsing-xbrl.core-test
  (:require [clojure.test :refer :all]
            [spike-for-parsing-xbrl.core :refer :all]))

(deftest xbrl-file-to-tsv-test
  (testing (is (= (affiliated-companies-tsv "resources/art-nature.xbrl") (slurp "resources/art-nature.tsv")))
    (is (= (affiliated-companies-tsv "resources/rkb.xbrl") (slurp "resources/rkb.tsv")))
    (is (= (affiliated-companies-tsv "resources/aresti.xbrl") (slurp "resources/aresti.tsv")))
    (is (= (affiliated-companies-tsv "resources/menikon.xbrl") (slurp "resources/menikon.tsv")))
    (is (= (affiliated-companies-tsv "resources/aichi-seiko.xbrl") (slurp "resources/aichi-seiko.tsv")))
    (is (= (affiliated-companies-tsv "resources/meidensha.xbrl") (slurp "resources/meidensha.tsv")))
    (is (= (affiliated-companies-tsv "resources/ryukyu-ginko.xbrl") (slurp "resources/ryukyu-ginko.tsv")))
    (is (= (affiliated-companies-tsv "resources/watami.xbrl") (slurp "resources/watami.tsv")))
    (is (= (affiliated-companies-tsv "resources/r-biban.xbrl") (slurp "resources/r-biban.tsv")))
    (is (= (affiliated-companies-tsv "resources/rc-core.xbrl") (slurp "resources/rc-core.tsv")))
    (is (= (affiliated-companies-tsv "resources/yukijirushi.xbrl") (slurp "resources/yukijirushi.tsv")))))

(deftest only-elements-tagged-with-test
  (testing (is (= [{:tag :x}] (only-elements-tagged-with  #{:x} [{:tag :x} {:tag :y}])))
    (is (= [{:tag :x} {:tag :y}] (only-elements-tagged-with  #{:x :y} [{:tag :x} {:tag :y}])))
    (is (= [] (only-elements-tagged-with #{:x} [] ) ))))

(deftest content-tagged-with-test
  (testing (is (= "foo" (content-tagged-with :foo [{:tag :foo :content ["foo"]}])))
    (is (= nil (content-tagged-with :foo [{:tag :bar :content ["bar"]}])))))

(deftest contents-tagged-with-test
  (testing (is (= [["foo"]] (contents-tagged-with :x [{:tag :x :content ["foo"]}])))
    (is (= [["foo"] ["bar"]] (contents-tagged-with :x [{:tag :x :content ["foo"]} {:tag :x :content ["bar"]}])))))

(deftest content-string-test
  (testing (is (= "foo" (content-string [{:tag :p :content ["foo"]}])))
    (is (= "foo bar" (content-string [{:tag :p :content ["foo"]} {:tag :p :content ["bar"]}])))
    (is (= "foo bar baz" (content-string [{:tag :p :content ["foo"]} {:tag :span :content ["bar"]} {:tag :p :content ["baz"]}])))))

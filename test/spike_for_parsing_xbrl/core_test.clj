(ns spike-for-parsing-xbrl.core-test
  (:require [clojure.test :refer :all]
            [spike-for-parsing-xbrl.core :refer :all]))

(deftest xbrl-file-to-tsv-test
  (testing (is (slurp "resources/art-nature.tsv") (= (affiliated-companies-tsv "resources/art-nature.xbrl")))
    (is (= (slurp "resources/rkb.tsv") (affiliated-companies-tsv "resources/rkb.xbrl")))
    (is (= (slurp "resources/aresti.tsv") (affiliated-companies-tsv "resources/aresti.xbrl")))
    (is (= (slurp "resources/menikon.tsv") (affiliated-companies-tsv "resources/menikon.xbrl")))
    (is (= (slurp "resources/aichi-seiko.tsv") (affiliated-companies-tsv "resources/aichi-seiko.xbrl")))
    (is (= (slurp "resources/meidensha.tsv") (affiliated-companies-tsv "resources/meidensha.xbrl")))
    (is (= (slurp "resources/ryukyu-ginko.tsv") (affiliated-companies-tsv "resources/ryukyu-ginko.xbrl")))
    (is (= (slurp "resources/watami.tsv") (affiliated-companies-tsv "resources/watami.xbrl")))
    (is (= (slurp "resources/r-biban.tsv") (affiliated-companies-tsv "resources/r-biban.xbrl")))
    (is (= (slurp "resources/rc-core.tsv") (affiliated-companies-tsv "resources/rc-core.xbrl")))
    (is (= (slurp "resources/yukijirushi.tsv") (affiliated-companies-tsv "resources/yukijirushi.xbrl")))
    (is (= (slurp "resources/yokokawa-denki.tsv") (affiliated-companies-tsv "resources/yokokawa-denki.xbrl")))
    (is (= (slurp "resources/moriya-shokai.tsv") (affiliated-companies-tsv "resources/moriya-shokai.xbrl")))
    (is (= (slurp "resources/ckd.tsv") (affiliated-companies-tsv "resources/ckd.xbrl")))
    (is (= (slurp "resources/shigaginko.tsv") (affiliated-companies-tsv "resources/shigaginko.xbrl")))
    (is (= (slurp "resources/nichirei.tsv") (affiliated-companies-tsv "resources/nichirei.xbrl")))))

(deftest only-elements-tagged-with-test
  (testing (is (= [{:tag :x}] (only-elements-tagged-with  #{:x} [{:tag :x} {:tag :y}])))
    (is (= [{:tag :x} {:tag :y}] (only-elements-tagged-with  #{:x :y} [{:tag :x} {:tag :y}])))
    (is (= [] (only-elements-tagged-with #{:x} [] ) ))))

(deftest first-content-tagged-with-test
  (testing (is (= "foo" (first-content-tagged-with :foo [{:tag :foo :content ["foo"]}])))
    (is (= nil (first-content-tagged-with :foo [{:tag :bar :content ["bar"]}])))
    (is (= "foo" (first-content-tagged-with :foo [{:tag :foo :content ["foo"]} {:tag :foo :content ["bar"]}])))
    (is (= "bar" (first-content-tagged-with :bar [{:tag :foo :content ["foo"]} {:tag :bar :content ["bar"]}])))))

(deftest contents-tagged-with-test
  (testing (is (= [["foo"]] (contents-tagged-with :x [{:tag :x :content ["foo"]}])))
    (is (= [["foo"] ["bar"]] (contents-tagged-with :x [{:tag :x :content ["foo"]} {:tag :x :content ["bar"]}])))))

(deftest concat-string-of-test
  (testing (is (= "foo" (concat-string-of [{:tag :p :content ["foo"]}])))
    (is (= "foo bar" (concat-string-of [{:tag :p :content ["foo"]} {:tag :p :content ["bar"]}])))
    (is (= "foo bar baz" (concat-string-of [{:tag :p :content ["foo"]} {:tag :span :content ["bar"]} {:tag :p :content ["baz"]}])))))

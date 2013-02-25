(ns testing.testing)
(require 'suffix.suffix)
(use 'clojure.test 'clojure.inspector)

;****************************************************
;****************************************************
; SINGLE-PATTERN QUERIES
;****************************************************
;****************************************************

(println "\n now we are going to treat SINGLE-PATTERN QUERIES: \n")
(def text "There is no dark side in the moon really, as a matter of fact it's all dark.")
(def pat1 "dark side")
(def pat2 "bright side")

(println "This is the text we generate suffix tree for: \n" text)
(println "This is the first pattern we are making queries for: " pat1)
(println "This is the second pattern we are making queries for: " pat2)
(def tree_root (suffix.core/build_tree text))

(println "This is the tree (shown using the 'inspect-tree' tool:")
(inspect-tree tree_root)

;****************************************************
; Query #1: do the patterns appear in the text ? 
;****************************************************

(println "Query #1: do the patterns appear in the text?") 

(def desired_1 [true,false])
(println "The desired result is: \n " desired_1)

(deftest test_1 (are [tree_root pat1 tree_root pat2 desired_1]
                       (=  [ (suffix.core/is_sub tree_root pat1) (suffix.core/is_sub tree_root pat2)] desired_1) 
                      tree_root pat1 tree_root pat2  desired_1 )
)

;****************************************************
; Query #2: where do the patterns appear in the text ? 
;****************************************************

(println "Query #2: where do the patterns appear in the text ?") 
(def desired_2 [#{12},nil])
(println "The desired result is: \n " desired_2)

(deftest test_2 (are [tree_root pat1 tree_root pat2 desired_2]
                       (=  [ (suffix.core/which_ind tree_root pat1) (suffix.core/which_ind tree_root pat2)] desired_2) 
                      tree_root pat1 tree_root pat2  desired_2 )
)


;****************************************************
; Query #3: How many times patterns apear in text ?
;****************************************************

(println "Query #3: How many times patterns apear in text ?") 

(def desired_3 [1,0])
(println "The desired result is: \n " desired_3)


(deftest test_3 (are [tree_root pat1 tree_root pat2 desired_3]
                       (=  [ (suffix.core/how_many tree_root pat1) (suffix.core/how_many tree_root pat2)] desired_3) 
                      tree_root pat1 tree_root pat2  desired_3 )
  )


;****************************************************
; Query #4: What is the longest most repeating substring?
;****************************************************

(println "Query #4: What is the longest most repeating substring?")
(println "If there's more than one substring that appears
        in the same amount of times in the string,\n returns
        the longest one among them.")
(println "\n Convention: if there is no repeating substring, returns the string itself")

(def desired_4 " ")
(println "The desired result is: \n " desired_4 "(the space character)") 


(deftest test_4 (are [tree_root  desired_4]
                      (=  (suffix.core/longest_repeating_substring tree_root) desired_4) 
                  tree_root  desired_4
                  )
)




;****************************************************
;****************************************************
; MULTI-PATTERNS QUERIES
;****************************************************
;****************************************************

(println "\n now we are going to treat MULTI-PATTERNS QUERIES,")
(println "and therefore calculations will be executing at the lazy way of calculation:  \n")
(def word "Mississippii")
(def pattern_sequence '("is", "ip","Mip","Miss", "miss") ; showing case-sesitivity 
 )

(println "This is the word we generate suffix tree for:" word)
(println "These are the patterns we are making queries for: " pattern_sequence)

;****************************************************
; Query #1: do the patterns appear ? lazely response:
;****************************************************

(println "Query #1: do the patterns appear in the word?") 

(def desired_res_1 [true,true,false,true,false])
(println "The desired result is: \n " desired_res_1)

(deftest test_res_1 (are [word pattern_sequene desired_res_1]
                       (= (suffix.core/sub_lazy word pattern_sequence) desired_res_1) 
                      word pattern_sequene desired_res_1
                     )
)


;****************************************************
; Query #2: do the patterns appear ? if they do, where?:
;****************************************************

(println "Query #2: do the patterns appear ? if they do, where?:?") 

(def desired_res_2 [#{1,4},#{7},nil,#{0},nil])
(println "The desired result is: \n " desired_res_2)
(deftest test_res_2 (are [word pattern_sequene desired_res_2]
                       (= (suffix.core/idx_lazy word pattern_sequence) desired_res_2)word pattern_sequene desired_res_2
                  )
)


;****************************************************
; Query #3: How many times patterns apear in text ?
;****************************************************

(println "Query #3: How many times patterns apear in text ?") 

(def desired_res_3 [2,1,0,1,0])
(println "The desired result is: \n " desired_res_3)


(deftest test_res_3 (are [word pattern_sequene desired_res_3]
                       (= ( suffix.core/how_many_lazy word pattern_sequence) desired_res_3) 
                     word pattern_sequene desired_res_3
                   )
)

;****************************************************
; Query #4: What is the longest most repeating substring?
;****************************************************

(println "Query #4: What is the longest most repeating substring?")
(println "That is, The longest repeated substring for each string in the coll'\n.
        If there's more than one substring that appears in the same amount of times in the string, returns
        the longest one among them.")
(println "Convention: if there is no repeating substring, returns the string itself")

(def desired_res_4 ["is", "ip","Mip", "s", "s"])
(println "The desired result is: \n " desired_res_4)
(def dum "dummy") 

(deftest test_res_4 (are [dum pattern_sequene desired_res_4]
                      (= (suffix.core/longest_repeating_substring_lazy dum pattern_sequence) desired_res_4) 
                    dum pattern_sequene desired_res_4
                  )
)




(run-tests)



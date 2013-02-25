(ns suffix.core
  (:gen-class
   :methods [#^{:static true} [is_sub_java [String String] Boolean]]
   :require clojure.inspector)
  )


; ======================================
;           Tree definition
; ======================================

"Node definition
 ---------------
 Node in the tree contains:
 1) next - hash-map of:
        key = <current char>
        value = rest of word, <node object>
 2) idx_start - set of indices where the word
                read so far starts

"
(defrecord node [next idx_start cnt]) 


; ======================================
;      Tree construction functions
; ======================================

(defn build_node
  "Creates an empty node"
  []
  (let [next (hash-map)
        idx_start (vector)
        cnt 0]
    (node. next idx_start cnt)))


(defn build_suffix
  "Generates the sub-tree for the given suffix

   Input:
        word - the suffix word to generate suffix tree for
               (required)
        idx - index which indicates at which indices
              does the current path correspond to
              (required)
        node - node to which the tree will be rooted at.
               If not given - creates an empty node and roots
               the new tree under this node.
   Returns:
        tree for the given suffix          
   "
  ([word idx] (build_suffix word (build_node) idx))
  ([word node idx]
    (if (= (count word) 0)
      (if (empty? (:next node))
        ; empty node
        (update-in (assoc node :idx_start [idx])
                   [:cnt] inc)
        ; node already exists?
        (update-in 
          (update-in node [:idx_start] conj idx)
          [:cnt] inc))

      ; node already exists
      (if-let [next_node (get-in node [:next (first word)])]
        (let [new_child_node (build_suffix (subs word 1) next_node idx)
              cur_char (first word)
              idx_start (conj (:idx_start node) idx)
              cnt (+ (:cnt node) 1)]
          (assoc
              (assoc-in node [:next cur_char] new_child_node)
              :idx_start idx_start
              :cnt cnt))
      
      ; node doesn't exist
      (let [child_node (build_suffix (subs word 1) idx)
            cur_char (first word)
            idx_start (conj (:idx_start node) idx)
            cnt (+ (:cnt node) 1)]
        (assoc (assoc-in node [:next cur_char] child_node)
               :idx_start idx_start
               :cnt cnt)
        )))))

(defn build_tree
  "Generates the suffix tree for the given word

   Input:
        word - the word to generate suffix tree for.
               word must be a string. (required)
   Returns:
        suffix tree for the given word          
   "
  [word]
  (if (not= (string? word))
    (throw (Throwable. "Input must be a string!"))
  (loop [idx 0
         root (build_suffix word idx)
         rest_word (subs word 1)]
    (if (empty? rest_word)
      root
    (recur (inc idx)
           (build_suffix rest_word root (+ idx 1))
           (subs rest_word 1))))))




; ======================================
;       Tree query & utility functions
; ======================================

(defn pred_mismatch
  "A predicate for validating the params to suffix_query

   Input:
        word - the word to be searched in (string). Required.
        col - collection of patterns (strings), Required.
   Method:
        validates that word is a string,
        and col is a non-empty collection of strings
   Returns:
        true if either of the params type mismatch
        and false otherwise
   "
  [word col]
  (or (or (not (string? word))
          (and (not= (list? col))
               (not= (vector? col))))
           (or (empty? col)
               (not-every? string? col))))

(defn lazy_query_real
  [func col root]
  (if (empty? col)
    nil
  (cons (func root (first col))
        (lazy-seq (lazy_query_real func (rest col) root))))  
  )

(defn lazy_query
  [func word col]
  (if (pred_mismatch word col)
    (throw (Throwable. (str
      "\nInput must be:\n"
      "col - a non-empty collection of string patterns\n"
      "word - a string!\n")))
    (lazy_query_real func col (build_tree word)))
  )

(defn how_much
  "Returns how many times a substring appears inside word 

   Input:
        root - the suffix tree (required).
        sub - sub-word to be searched for (required).
   "
  [root sub]
  (if (= (count sub) 0)
    (:cnt root)
    (if-let [next_node (get-in root [:next (first sub)])]
      (how_much next_node (subs sub 1))
      0)))


(defn which_ind
  "Retrieves the set of indices where sub is located 
   (and nil otherwise).

   Input:
        root - the suffix tree (required).
        sub - sub-word to be searched for
   Returns:
        the set of indices where sub is located,
        and nil otherwise
   "
  [root sub]
  (if (= (count sub) 0)
    (get-in root [:idx_start])
    (if-let [next_node (get-in root [:next (first sub)])]
      (which_ind next_node (subs sub 1))
      nil)))


(defn is_sub
  "Checks whether sub is a substring of a word 

   Input:
        root - the suffix tree (required).
        sub - sub-word to be searched for
   Returns:
        true if sub is a substring of root
        false otherwise
   "
  [root sub]
  (not= (which_ind root sub) nil))


; ======================================
;        Queries with Lazy sequences
; ======================================
(defn sub_lazy
  "Checks whether a collection of inputs are substrings of a word

   Input:
        word - the word to be searched in (required)
        col - collection of patterns (strings)
   Returns:
        a lazy sequence, for each pattern (in the corresponding index)
        returns true if it is a substring of word
        false otherwise
   "
  [word col]
  (lazy_query is_sub word col))

(defn idx_lazy
  "Checks whether a collection of inputs are substrings of a word

   Input:
        word - the word to be searched in (required)
        col - collection of patterns (strings)
   Returns:
        a lazy sequence, for each pattern (in the corresponding index)
        returns true if it is a substring of word
        false otherwise
   "
  [word col]
  (lazy_query which_ind word col))


; ======================================
;        AOT demonstration
;    Call Clojure functions from Java
; ======================================
(defn -is_sub_java
  "Checks whether sub is a substring of a word 

   Input:
        root - the suffix tree (required).
        sub - sub-word to be searched for
   Returns:
        true if sub is a substring of root
        false otherwise
   "
  [string_ sub]
  (is_sub (build_tree string_) sub))


; ======================================
;              Testings
; ======================================

(defn do_testing
  []
  (let [pat (list "ku" "ka" "h" "kuku")
        root (build_tree "kuka")
        sol_vec (sub_lazy "kuku" pat)
        sol_vec2 (idx_lazy "kuku" pat)]
    (println sol_vec)
    (println sol_vec2)))

(defn show_tree
  []
  (use 'clojure.inspector)
  (inspect-tree (build_tree "hi")))

; ======================================
;              Main
; ======================================

(defn -main[]
  (do_testing)
  )
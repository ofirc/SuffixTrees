(ns test.test)

"Node definition
 ---------------
 Node in the tree contains:
 1) next - hash-map of:
        key = <current char>
        value = rest of word, <node object>
 2) idx_start - set of indices where the word
                read so far starts

"
(defrecord node [next idx_start]) 

(defn build_node
  "Creates an empty node"
  []
  (let [next (hash-map)
        idx_start (vector)]
    (node. next idx_start)))


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
        (assoc node :idx_start [idx])
        ; node already exists?
        (update-in node [:idx_start] conj idx))
        
      
      ; node already exists
      (if-let [next_node (get-in node [:next (first word)])]
        (let [new_child_node (build_suffix (subs word 1) next_node idx)
              cur_char (first word)
              idx_start (conj (:idx_start node) idx)]
          (assoc
              (assoc-in node [:next cur_char] new_child_node)
              :idx_start idx_start))
      
      ; node doesn't exist
      (let [child_node (build_suffix (subs word 1) idx)
            cur_char (first word)
            idx_start (conj (:idx_start node) idx)]
        (assoc (assoc-in node [:next cur_char] child_node)
               :idx_start idx_start)
        )
))))

(defn build_tree
  "Generates the suffix tree for the given word

   Input:
        word - the word to generate suffix tree for.
               word must be a string. (required)
   Returns:
        suffix tree for the given word          
   "
  [word]
  (if (not= (type word) java.lang.String)
    (throw (Throwable. "Input must be a string!"))
  (loop [cur_idx 0
         root (build_suffix word cur_idx)
         rest_word (subs word 1)]
    (if (empty? rest_word)
      root
    (recur (inc cur_idx)
           (build_suffix rest_word root (+ cur_idx 1))
           (subs rest_word 1))))))

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


; Laziness is a virtue
; --------------------
(defn is_sub_lazy2
  "The actual is_sub_lazy function.
   The is_sub_lazy function is a wrapper on top of
   is_sub_lazy2 in order to save the cost of performing
   the input validity on each entrance of is_sub_lazy.
   So it is performed once and then sent to is_sub_lazy2."
  [col root]
  (if (empty? col)
    nil
  (cons (is_sub root (first col))
        (lazy-seq (is_sub_lazy2 (rest col) root)))))

(defn is_sub_lazy
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
  (if (or (or (not= (type word) java.lang.String)
          (and (not= (type col) clojure.lang.PersistentList)
               (not= (type col) clojure.lang.PersistentVector)))
          (some #(not= (type %) java.lang.String) col))
    (throw (Throwable. "Input must be:
                        col - a collection of string patterns
                        word - a string!"))
    (is_sub_lazy2 col (build_tree word))))


; pattern
(def pat (list "ku" "ka" "h" "kuku"))

; tree
(def root (build_tree "kuka"))

; test patterns
(def sol_vec (is_sub_lazy "kuku" pat))


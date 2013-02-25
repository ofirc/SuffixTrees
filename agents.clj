(ns suffix.agents)

(require 'suffix.suffix)

(defn is_sub_agt 
  [x root] 
  (println (str x ":            " (suffix.core/is_sub root x))))

(defn agent_example
  []
  (let [text "hiha"
        root (suffix.core/build_tree text)
        pat '("hi","ih","hj","hiha")
        agts (map agent pat)]
    ; start agent
	  (println "Demonstrating agents in action"
	           "on the following pattern:\n" text 
            "\n=====================================\n"
            "Pattern        Matched")
    (dotimes [i (count pat)] 
      (send (nth agts i) is_sub_agt root))))


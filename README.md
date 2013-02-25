SuffixTrees
===========

Suffix Tree impelmentation in Clojure

In order to run the project functions do the following:
1) Eclipse > Import existing project into workspace > eclipse_project.zip
2) To run the tests, open 'src/testing.clj' and load it in the REPL
(Alt+Ctrl+S)
3) To run the suffix functions, open 'src/suffix/suffix.clj' and load it
in the REPL (Alt+Ctrl+S), and then switch to the namespace (Alt+Ctrl+N).
From here you can call any of the project functions, for example:
(println (is_sub (build_tree "banana") "na"))

Note: 
1) We've used Counterclockwise plugin for Eclipse that integrates
Clojure support into the IDE, such as REPL and editing features.
For more information please refer to:
https://code.google.com/p/counterclockwise/

2) There is a more detailed introduction to the project written in Hebrew
in "Project full report.pdf"
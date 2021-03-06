;;;; Utils package definitions

(in-package :cl-user)

(defpackage :glu
  (:use :cl :local-time)
  (:export :!
           :*!equivs*
           :*english-list*
           :*log-format-time*
           :->
           :=>
           :after?
           :append1
           :before?
           :best
           :conc1
           :def!
           :dlambda
           :dup?
           :empty?
           :explode
           :failed?
           :fif
           :filter
           :find2
           :finter
           :funion
           :group
           :labeled-time
           :last1
           :levels
           :logm
           :loose-parse-int
           :longer?
           :lrec
           :map->
           :map0-n
           :map1-n
           :mapa-b
           :mapcars
           :memoize
           :mklist
           :mkstr
           :most
           :mostn
           :new-r
           :non-empty?
           :prune
           :r
           :r-data
           :r-level
           :r-message
           :reread
           :rmapcar
           :sf
           :sharp-double-quote-reader
           :sharp-greater-than-reader
           :single?
           :split-if
           :succeeded?
           :symb
           :to-string
           :trec
           :ttrav))

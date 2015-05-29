;;;; Utils package definitions

(defpackage :glu
  (:use :cl)
  (:export :*english-list*
           :last1
           :single?
           :append1
           :conc1
           :mklist
           :longer?
           :filter
           :group
           :flatten
           :prune
           :find2
           :before?
           :after?
           :dup?
           :split-if
           :best
           :most
           :mostn
           :map->
           :mapa-b
           :map0-n
           :map1-n
           :mappend
           :mapcars
           :rmapcar
           :mkstr
           :symb
           :reread
           :explode
           :*!equivs*
           :!
           :def!
           :memoize
           :compose
           :fif
           :finter
           :funion
           :lrec
           :ttrav
           :trec
           :labeled-time
           :with-gensyms
           :sharp-double-quote-reader
           :sharp-greater-than-reader))

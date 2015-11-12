;;;; Utils package definitions

(defpackage :glu
  (:use :cl)
  (:export :!
           :*!equivs*
           :*english-list*
           :after?
           :append1
           :before?
           :best
           :compose
           :conc1
           :def!
           :dlambda
           :dup?
           :explode
           :fif
           :filter
           :find2
           :finter
           :flatten
           :fmt
           :funion
           :group
           :labeled-time
           :last1
           :longer?
           :lrec
           :map->
           :map0-n
           :map1-n
           :mapa-b
           :mapcars
           :mappend
           :memoize
           :mklist
           :mkstr
           :most
           :mostn
           :prune
           :reread
           :rmapcar
           :sharp-double-quote-reader
           :sharp-greater-than-reader
           :single?
           :split-if
           :symb
           :trec
           :ttrav
           :with-gensyms))

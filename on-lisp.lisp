;;;; The following functions are taken directly from Paul Graham's On Lisp, or
;;;; are minor modifications there of.

(in-package :glu)

;;; List-based:

(defun last1 (lst)
  "Get the last item in lst. If lst is not a list it simply returns it."
  (if (listp lst)
    (car (last lst))
    lst))

(defun single? (lst)
  "Determine whether lst is a list with exactly one element."
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  "Append obj to the end of the lst."
  (append lst (list obj)))

(defun conc1 (lst obj)
  "Destructively append obj to the end of lst."
  (nconc lst (list obj)))

(defun mklist (obj)
  "Ensure obj is a list."
  (if (listp obj) obj (list obj)))

(defun longer? (x y)
  "Determine if x is longer than y.
   x and y can be any values for which #'length can be applied."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  "Applies fn to each item in lst, returning a list of the non-nil values."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  "Groups items in source into groups of size n."
  (if (<= n 0) (error "n (group size) must be positive"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun prune (test tree)
  "Removes items in tree (traversing nested lists), where test returns true for
   the item."
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

;;; Searching:

(defun find2 (fn lst)
  "Find the first object in lst that satisfies fn."
  (if (null lst)
    nil
    (let ((val (funcall fn (car lst))))
      (if val
        (values (car lst) val)
        (find2 fn (cdr lst))))))

(defun before? (x y lst &key (test #'eql))
  "Determine if x occurs before y in lst.
   If x occurs in lst but y doesn't, this is considered a match.
   This function returns the cdr of lst starting with x if the match is made;
   otherwise nil."
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before? x y (cdr lst) :test test))))))

(defun after? (x y lst &key (test #'eql))
  "Determine if x occurs after y in lst.
   This function returns the cdr of lst starting with x if the match is made;
   otherwise nil."
  (let ((rest (before? y x lst :test test)))
    (and rest (member x rest :test test))))

(defun dup? (obj lst &key (test #'eql))
  "Determine if obj occurs in lst more than once.
   This function returns the cdr of lst starting with the second instance of
   obj if found; otherwise nil."
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  "Returns two lists that are split after fn returns true for the first match
   in lst."
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun best (fn lst)
  "Determine the item considered the 'best' among all items, according to fn.
   fn should take two args and return non-nil if the first arg is 'better' than
   the second.
   This function returns the 'best' item."
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun most (fn lst)
  "Determine the item with the highest score in lst by passing each item in lst
   through fn. fn should return a numeric value - the score.
   This function returns the item with the highest score, and the score itself.
   In case of a tie the first item wins."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun mostn (fn lst)
  "Determine the items tied for the highest score in lst by passing each item
   in lst through fn. fn should return a numeric value - the score.
   This function returns a list of the items with the highest score, and the
   score itself."
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;; Mapping

(defun map-> (fn start end-fn succ-fn)
  "A flexible method of applying a function over a sequence.
   fn: function to apply over the sequence.
   start: value specifying the start of the sequence.
   end-fn: function that determines the end of the sequence.
   succ-fn: function that generates the next item in the sequence.
   This function returns a list after having applied fn to each item in the
   specified sequence."
  (do ((i start (funcall succ-fn i))
       (result nil))
    ((funcall end-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapa-b (fn a b &optional (step 1))
  "Maps over a sequence from a to b, applying fn to each item.
   fn: function to apply to each item in the sequence.
   a: value specifying the start of the sequence.
   b: value specifying the end of the sequence (inclusive).
   step: value specifying how much to step each item in the sequence by.
   This function returns a list after having applied fn to each item in the
   specified sequence."
  (map-> fn
         a
         (λ (x) (> x b))
         (λ (x) (+ x step))))

(defun map0-n (fn n)
  "Maps over a sequence from 0 to n, applying fn to each item.
   fn: function to apply to each item in the sequence.
   n: value specifying the end of the sequence (inclusive).
   "
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  "Maps over a sequence from 1 to n, applying fn to each item.
   fn: function to apply to each item in the sequence.
   n: value specifying the end of the sequence (inclusive).
   This function returns a list after having applied fn to each item in the
   specified sequence."
  (mapa-b fn 1 n))

(defun mapcars (fn &rest lsts)
  "Behaves like mapcar but works with multiple list arguments."
  (let ((result nil))
    (dolist (lst lsts)
      (dolist  (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  "Works like mapcar but supports nested lists."
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             (λ (&rest args)
                (apply #'rmapcar fn args))
             args)))

;; Strings and symbols:

(defun mkstr (&rest args)
  "Builds a string by concatenating the printed versions of the given args."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Returns a symbol whose printed name is the concatenation of the given args.
   If the symbol doesn't exist, it is created."
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  "Takes a series of objects, prints and re-reads them.
   NOTE: I don't understand what this would be used for..."
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  "Takes a symbol and returns a list of symbols made from the characters in
   its name."
  (map 'list
       (λ (c) (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

;;; Function manipulation:

(defvar *!equivs* (make-hash-table)
  "A hash table containing functions with their destructive counterparts.")

(defun ! (fn)
  "Get the destructive version of fn. If there is no destructive version of fn,
   fn is returned."
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  "Register a function (fn) with its destructive counterpart (fn!)."
  (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn)
  "Caches and returns the result of calls to fn.
   Unique calls to fn are stored in a hash table and returned when found.
   Lookups are done using #'equal.
   Note that multiple return values are not handled."
  (let ((cache (make-hash-table :test #'equal)))
    (λ (&rest args)
       (multiple-value-bind (val win) (gethash args cache)
         (if win
             val
             (setf (gethash args cache) (apply fn args)))))))

(defun fif (if then &optional else)
  "fif stands for 'function if'. It encapsulates the pattern:
   'if x then y else z'."
  (λ (x)
     (if (funcall if x)
         (funcall then x)
         (if else (funcall else x)))))

(defun finter (fn &rest fns)
  "finter stands for 'function intersection'. It builds a function that
   essentially computes the intersection of some value across a set of
   functions."
  (if (null fns)
      fn
      (let ((chain (apply #'finter fns)))
        (λ (x) (and (funcall fn x) (funcall chain x))))))

(defun funion (fn &rest fns)
  "funion stands for 'function union'. It builds a function that essentially
   computes the union of some value across a set of functions."
  (if (null fns)
    fn
    (let ((chain (apply #'funion fns)))
      (λ (x) (or (funcall fn x) (funcall chain x))))))

(defun lrec (rec &optional base)
  "lrec stands for 'list recurser'.
   It encapsulates the pattern of recursing down the cdrs of a list.
   rec: a function that takes two arguments. The first arg is the current car
   of the list. The second arg is a function which can be called to continue
   the recursion.
   base: a function or value. If a function it returns the base value. If a
   a value, it is the base value itself.
   Note: Graham warns that this will not yield a fast implementation as it
   won't result in tail-recursion. Use this for prototyping or where speed is
   not critical.
   Some example uses:
   * copy list:
     * (lrec (λ (x f) (cons x (funcall f))))
   * remove duplicates:
     * (lref (λ (x f) (adjoin x (funcall f))))
   * find-if for some function fn
     * (lref (λ (x f) (if (fn x) x (funcall f))))
   * some, for some function fn
     * (lref (λ (x f) (or (fn x) (funcall f))))"
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                              (λ () (self (cdr lst)))))))
    #'self))

(defun ttrav (rec &optional (base #'identity))
  "ttrav stands for 'tree traverser'.
   It encapsulates the pattern of recursing through a list, including
   sub-lists, i.e. trees. Note that ttrav always traverses then entire tree.
   rec: a function that takes two arguments. The first arg is the returned
   function applied to the current car of the list. The second arg is a
   function that is applied to the cdr of the list.
   base: a function or value. If a function it returns the base value, given
   the tree (which would be an atom). If a value, it is the base value itself.
   Some example uses:
   * tree copy:
     * (ttrav #'cons)
   * count leaves:
     * (ttrav (λ (l r) (+ l (or r 1))) 1)
   * flatten:
     * (ttrav #'nconc #'mklist)"
  (labels ((self (tree)
             (if (atom tree)
               (if (functionp base)
                   (funcall base tree)
                   base)
                 (funcall rec (self (car tree))
                              (if (cdr tree)
                                  (self (cdr tree)))))))
    #'self))

(defun trec (rec &optional (base #'identity))
  "trec stands for 'tree recurser'.
   It is a more general version of ttrav than supports the ability to not have
   to traverse the entire tree.
   rec: a function that takes three arguments. The first arg is the current
   object in the tree. The second arg is the recursion of the 'left side' of
   the tree. The third arg is the recursion of the 'right side' of the tree.
   Some example uses:
   * flatten:
     * (trec (λ (o l r) (nconc (funcall l) (funcall r)))
             #'mklist)
   * rfind-if using oddp:
     * (trec (λ (o l r) (or (funcall l) (funcall r)))
             (λ (tree) (and (oddp tree) tree)))"
  (labels
    ((self (tree)
       (if (atom tree)
           (if (functionp tree)
               (funcall base tree)
               base)
           (funcall rec tree
                        (λ () (self (car tree)))
                        (λ () (if (cdr tree) (self (cdr tree))))))))
    #'self))

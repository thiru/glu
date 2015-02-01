;; TODO: Find a way to ensure that this only gets called once...
(defun λ-reader (stream char)
  "Allow the character 'λ' to be used in place of the word 'lambda', for
   brevity's sake."
  (declare (ignore char stream))
  'LAMBDA)
(set-macro-character #\λ #'λ-reader)

(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~; and ~:;, ~]~}~]~}"
  "A control string to format a list of items in a friendly manner.
   E.g. '1 and 2', or '1, 2 and 3'.
   This was taken from PCL.")


;;; The following functions are based on, or taken directly from Paul Graham's
;;; On Lisp.

;;; List-based:

(proclaim '(inline last1 single? append1 conc1 mklist))

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
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  "Makes x into a flat list, removing any potentially nested lists.
   If x is not a list it is turned into one."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

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

(defun mappend (fn &rest lsts)
  "A non-destructive alternate to mapcan."
  (apply #'append (apply #'mapcar fn lsts)))

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

;; Strings and symbols

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

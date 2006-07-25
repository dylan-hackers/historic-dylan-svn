;;
 ; File: LibraryATN.Lisp
 ; This is a natural language interface for a library database system
 ; Copyright 1990 by Mark Watson
 ;;

(proclaim '(special *library-card-holders* *sentence* *author-hash-table*))

;;
 ; Main interface function:
 ;  Parse(sentence) - sentence is the sentence to process
 ;                  - returned value is a semantic memory structure
 ;;

(defun parse (sentence &aux original-sentence)
  (setq *subject* nil
        *verb* nil
        *book-title* nil
        *object-1* nil
        *modifier-1* nil
        *number-1* nil
        *modifier-type-1* nil
        *card-holder* nil
        *action* nil
        *last-noun* nil
        *last-verb* nil
        *last-prep* nil
        *last-word* nil
        *response* nil
        *response-type* nil)
  (setq *sentence* sentence original-sentence (copy-list sentence))
  (if (not (sentence?)) ;; cheap trick: if you fail to parse,
      (let ()           ;; discard the first word and try again
        (setq *last-word* (car *sentence*))
        (setq *sentence* (cdr *sentence*))
        (sentence?)))
  ;; if *object-1* has not been set, look for a book title:
  (if (null *object-1*)
      (dolist (w original-sentence)
        (if (stringp w)
            (setq *object-1* w))))
  (princ "Semantic memory structure:") (terpri)
  (princ " Subject       : ") (princ *subject*) (terpri)
  (princ " Action       : ") (princ *action*) (terpri)
  (princ " Library card holder : ") (princ *card-holder*) (terpri)
  (princ "   Action object   : ") (princ *object-1*) (terpri)
  (princ "   Object modifier type: ") (princ *modifier-type-1*) (terpri)
  (princ "   Object modifier  : ") (princ *modifier-1*) (terpri)
  (princ "   Number modifier  : ") (princ *number-1*) (terpri)
  (terpri)
;; Use semantics of user prompt to perform data base query
;; or update operation:
  (case *action*
    (nil (terpri))
    (list-info  (if *response*
                    (case *response-type*
                      (nil (princ "What?") (terpri))
                      (author (pp-author *response*))
                      (book (pp-book *response*)))))
    (return-book (princ "Returning the book: ")
                 (princ *object-1*)
                 (princ ".") (terpri)
                 (princ "Returned by ")
                 (dolist (x *card-holder*)
                   (princ x)
                   (princ " "))
                 (princ "and ") (princ *object-1*)
                 (princ " is returned to circulation.")
                 (terpri)
     (check-out-book (princ "Checking out the book: ")
                     (princ *object-1*) (princ ".") (terpri)
                     (princ "Checked out by confirmed library card holder")
                     (terpri)
                     (dolist (x *card-holder*)
                       (princ x)
                       (princ " "))
                     (princ "and data base is modified to show") (terpri)
                     (princ "that this volume is checked out.")
                     (terpri)))))

;;
 ; Print out information about author:
 ;;

(defun pp-author (data)
  (princ "The following book(s) are by ")
  (dolist (x (cadaar data))
    (princ x)
    (if (equal x (car (cadaar data)))
        (princ ","))
    (princ " "))
  (princ ":") (terpri)
  (dolist (book (car data))
    (princ (car book))
    (terpri)))

;;
 ; Print out information about a book:
 ;;

(defun pp-book (data)
  (princ (cadaaar data))
  (princ "is the author.")
  (terpri))

;;
 ; Word discriminators:
 ;;

(defun isNoun? (word)
  (if (member word *nouns*)
      (let ()
        (setq *last-noun* word)
        (if (or *subject*    ;; already defined
                *last-verb*) ;; must be an imperative
            (setq *object-1* word))
      t)))

(defun isVerb? (word)
  (if (member word *verbs* :test #'equal)
      (let ()
        (if (null *subject*)  ;; assume that sentence is an imperative
            (setq *subject* 'computer))
         ;; semantic hooks to set *action* == 'list-info:
        (if (or (member word '(list show))
                (and (equal word 'are)
                     (equal *last-word* 'what)))
            (setq *action* 'list-info))
         ;; semantic hooks to set *action* == 'return-book
        (if (or (member word '(return returned) :test #'equal)
                (and (member word '(check checked) :test #'equal)
                     (if (> (length *sentence*) 1)
                         (equal (cadr *sentence*) '(in back)))))
            (setq *action* 'return-book
                  *sentence* (cdr *sentence*)))
         ;; semantic hooks to set *action* == 'check-out-book
        (if (or (member word '(got receive received withdrew) :test #'equal)
                (and (member word '(check checked) :test #'equal)
                     (if (> (length *sentence*) 1)
                         (member (cadr *sentence*) '(out) :test #'equal))))
            (setq *action* 'check-out-book
                  *sentence* (cdr *sentence*)))
        (setq *last-verb* word))))

(defun isAdj? (word)
  (if (member word *adjs*)
      (if (null *modifier-1*)
          (setq *modifier-1* word)
        t)))

(defun isPrep? (word)
  (if (member word *preps*)
      (let ()
        (setq *last-prep* word)
        (if (member word '(a an one this that))
            (setq *number-1* 1)
          (if (member word '(all every))
              (setq *number-1* 999999)
            t)))))

(defun isBookTitle? (sentenceElement)
  (stringp sentenceElement))

;;
 ; Parsing networks:
 ;;

(defun np? ()
  (let ((save *sentence*) temp)
    (if (or ;; name of member of library?:
            (and ;; embedded semantics: do not allow parse to generate
                 ;; a form like 'by <author name>' if known action in
                 ;; the sentence is 'list-info':
                 (not (equal *action* 'list-info))
                 (setq temp (card-holder-name?))
                 (if (and temp
                          (null *card-holder*))
                     (setq *card-holder* temp)))
            (and (word? 'adj)
                 (or (np?) (word? 'noun)))
            (word? 'noun))
         (let ()
           (if (null *subject*)
               (setq *subject* *last-noun*))
         t)
      (let ()
        (setq *sentence* save)
        nil))))

(defun prepp? ()
  (let ((save *sentence*)
        (last-author-name nil)
        (last-card-holder-name nil)
        (temp nil))

;;
 ; Define an ATN to detect author names. Since this ATN is
 ; only called from inside the 'prepp?' ATN, we will define
 ; its LISP function lexically inside 'prepp?'
 ;;
(defun author-name? ()
  (if (gethash (car *sentence*) *author-hash-table*)
      (let ()
        (setq last-author-name (car *sentence*))
        (setq *sentence* (cdr *sentence*)) ;; throw away processed word
      t) ;; return True: ATN successfully traversed
         ;; check to see if the second word left in the sentence
         ;; is a known author's last name:
    (if (> (length *sentence*) 1)  ;; more than 1 word left ?
        (if (gethash (cadr *sentence*) *author-hash-table*)
            (let ()
              (setq last-author-name (cadr *sentence*))
              (setq *sentence* (cddr *sentence*))  ;; throw away processed words
              t)))))   ;; return True: ATN successfully traversed

    (if (or (and (word? 'prep)
                 (np?))
            (and   ;; special check for 'by <author name>'
                 (setq *sentence* save)
                 (word? 'prep)
                 (equal *last-prep* 'by)
                 (author-name?)
                 (let ()    ;; we found a form like: 'by <author name>'
                   (setq *modifier-type-1* 'by)
                   (setq *modifier-1* last-author-name)
                   t))
            (and   ;; special check for 'by <card holder name>
                 (setq *sentence* save)
                 (word? 'prep)
                 (equal *last-prep* 'by)
                      ;; embedded semantics: do not allow parse to generate
                      ;; a form like: 'by <author name>' if known action in
                      ;; the sentence is 'list-info'
                 (not (equal *action* 'list-info))
                 (setq temp (card-holder-name?))
                 (let ()     ;; we found a form like: 'by <card holder name>'
                   (setq *modifier-type-1+ 'by)
                   (setq *modifier-1* temp)
                   t)))

        (let ()
          (setq *sentence* save)
          nil))))

(defun vp? ()
  (let ((save *sentence*))
    (if (or (and (word? 'verb) (np?))
            (let ()
              (setq *sentence* save)
              (word? 'verb)))
         (let ()
           (if (null *action*)
               (setq *action* *last-verb*))
           t)
      (let ()
        (setq *sentence* save)
        nil))))

(defun sentence? ()
  (let ((save *sentence*))
    (if (or (and (vp?) ;; imperative
                 (prepp?)
                 (prepp?))
            (and (vp?)  ;; imperative
                 (prepp?))
            (let ()
              (setq *sentence* save)
              (and (np?)
                   (vp?)
                   (prepp?)))
            (let ()
              (setq *sentence* save)
              (and (np?)
                   (vp?)))
            (let ()
              (setq *sentence* save)
              (if (vp?)
                  (setq *subject* 'computer)  ;; imperative (command)
                nil)))
        t
      (let ()
        (setq *sentence* save)
        nil))))

;;
 ; Define an ATN to detect card holder names:
 ;;

(defun card-holder-name? ()
  ;; sentence must be long enough for a first and last name:
  (if (> (length *sentence*) 1)
      (let ((name-list (list (car *sentence*) (cadr *sentence*))))
        (if (member name-list *library-card-holders* :test #'equal)
            (let ()
              (setq last-card-holder-name name-list)
              (setq *sentence* (cddr *sentence*)) ;; throw away processed word
              name-list)))))  ;; return True: ATN successfully traversed

;;
 ; Test for word type:
 ;;

(defun word? (type)
    ;; grab semantic information on book/author if appropriate:
  (if (not *response*)
      (if (member *last-prep* '(by))
          (let ((author-data1 (gethash (car *sentence*) *author-hash-table*))
                (author-data2 (if (> (length *sentence*) 1)
                                  (gethash (cadr *sentence*) *author-hash-table*))))
            (if author-data1
                (setq *response-type* 'author
                      *modifier-1* (car *sentence*)
                      *response* (cons author-data1 *response*))
              (if author-data2
                  (setq *response-type* 'author
                        *modifier-1* (cadr *sentence*)
                        *response*   (cons author-data2 *response*)))))))
  (let ((save *sentence*)
        (w (if (null *sentence+)
               nil
             (car *sentence*)))
        (func (assoc type '((noun isNoun?) (verb isVerb?) (prep isPrep?) (adj isAdj?)))))
    (if func
        (if (funcall (cadr func) w)
            (let ()
              (remove-word)
              t)
          nil)
      nil)))


;;
 ; Utility to remove first word remaining in sentence AND to
 ; save the discarded word in *last-word*
 ;;

(defun remove-word ()
  (setq *last-word* (car *sentence*))
  (setq *sentence* (cdr *sentence*)))

;;
 ; Define words in the system:
 ;;

(setq *nouns* '(I book books library))

(setq *verbs* '(list show are is add was see check checked got returned))
(setq *adjs* '(what the a an one this that all every))
(setq *preps* '(in behind under by))

;;
 ; Turn on tracing for debug output:
 ;;

(trace isNoun? isVerb? isAdj? isPrep? np? prepp? vp? sentence?
   card-holder-name?)

;;
 ; Sample Data Base:
 ;;

(setq *book-list*
   '(
     ("The Sound And The Fury" (Faulkner William))
     ("The Sound of Rain" (Peebody Alfred))
     ("Broca's Brain" (Sagan Carl))
     ("Dragons of Eden" (Sagan Carl))))

(setq *library-card-holders*
   '(
     (Mark Watson)
     (Carol Watson)
     (Julie Kimmel)
     (David Kimmel)))

;;
 ; Utilities to pre-process book list to remove prepositions
 ; Book titles for more robust pattern matching. Also convert
 ; from a string to a list of words:
 ;;

(defun shrink-title (title-string)
  (let (retList)
    (with-input-from-string (aStream (string-upcase title-string))
      (dotimes (i 20)
        (let ((token (read aStream nil nil nil)))
          (if token
              (if (not (member token '(a an the that this)))
                  (setq retList (cons token retList)))))))
    (cons 'book\: (reverse retList))))

;;
 ; Data management functions: Initialize the database by
 ; shrinking down book titles and building two hash tables:
 ;
 ; 1. *book-hash-table*     - key is the first word of the
 ;                            shrunken book title.
 ; 2. *author-hash-table*   - key is the author's last name.
 ;
 ; *library-card-holders* = list of members of the library
 ;;

(defun init-database ()
  (setq *book-hash-table* (make-hash-table))
  (setq *author-hash-table* (make-hash-table))
  (dolist (aBook *book-list*)
    (let ((title-hash-key (cadr (shrink-title (car aBook))))
          (author-hash-key (caadr aBook)))
      (setf (gethash title-hash-key *book-hash-table*)
            (cons aBook (gethash title-hash-key *book-hash-table*)))
      (setf (gethash author-hash-key *author-hash-table*)
            (cons aBook (gethash author-hash-key *author-hash-table*))))))

(init-database) ;; initialize the system when this file is loaded.

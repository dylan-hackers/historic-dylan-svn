<?xml version="1.0" encoding="US-ASCII"?> 
<!--
    DylanDoc XSL Stylesheet - format DylanDoc documents for web
    Copyright (C) 2004 Brent Fulgham

    Based on Eric Kidd's original 1998 DSSSL Stylesheet.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    ****

    This stylesheet was written to work with DylanDoc 3.0, DocBook 3.0,
    and Norman Walsh's modular stylesheets, version 1.13. Other versions
    of each of these components may work.

    Autoswitch magic allows this stylesheet to support both print
    and HTML modes. Note that you might need to edit the CATALOG file
    to define the appropriate pubids for your platform.

    If you want to borrow portions of this code for non-GPL'd DSSSL
    stylesheets, I'll almost certainly grant permission. Just ask.

    Eric Kidd
    eric.kidd@pobox.com
-->
<xsl:stylesheet  
       xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  version="1.0"> 
  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl"/> 
  <xsl:include href="dylandoc.xsl" />

  <!-- Specify that we will be using a CSS Stylesheet -->
  <xsl:param name="html.stylesheet.type">text/css</xsl:param>
  <xsl:param name="html.stylesheet" select="'dylandoc.css'"/>

  <!--
    Dylan Literal Tags.  We use mono-spaced sequential text,
    since these are program elements, and they should be green.
    (Elements are dlibrary, dmodule, dname, dlit, and dparam)
  -->

</xsl:stylesheet>  
<!--
<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN"
 [
<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl SYSTEM "docbook/stylesheet/dsssl/modular/html/docbook.dsl" CDATA dsssl>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl SYSTEM "docbook/stylesheets/dsssl/modular/print/docbook.dsl" CDATA dsssl>
]]>
]>
<style-sheet>
<style-specification id="print" use="docbook">
<style-specification-body> 

;; customize the print stylesheet

</style-specification-body>
</style-specification>
<style-specification id="html" use="docbook">
<style-specification-body> 

;;;;=======================================================================
;;;; HTML Stylesheet
;;;;=======================================================================
;;;; This stylesheet uses James Clark's extensions for creating HTML from
;;;; a DSSSL stylesheet.

(define %html-ext%
  ;; We hate 8.3. Sorry.
  ".html")

(define %use-id-as-filename%
  ;; Make ID attributes into file names.
  #t)


;;;========================================================================
;;; Overrides
;;;========================================================================
;;; Customize DocBook to work well with our manuals.

(define (chunk-skip-first-element-list)
  ;; Do not lift the first SECT1 up to CHAPTER level.
  '())

(define (book-titlepage-recto-elements)
  (list (normalize "title")
	(normalize "subtitle")
	(normalize "graphic")
	(normalize "corpauthor")
	(normalize "authorgroup")
	(normalize "author")
	(normalize "editor")
	(normalize "copyright")
	(normalize "legalnotice")
	(normalize "abstract")))

(define ($person-info$ label)
  (make sequence
    (if (first-sibling? (current-node))
	(make element
	  gi: "P"
	  (make element
	    gi: "STRONG"
	    (literal label)))
	(empty-sosofo))
    (make element
      gi: "P"
      (make sequence
	(process-first-descendant "FIRSTNAME")
	(literal " ")
	(process-first-descendant "SURNAME")
	(if (have-child? "AFFILIATION")
	    (make sequence
	      (make empty-element gi: "BR")
	      (process-first-descendant "AFFILIATION"))
	    (empty-sosofo))))))

(element (docinfo editor)
  ($person-info$ "Edited by:"))

(element (docinfo author)
  ($person-info$ "Written by:"))

(element (docinfo authorgroup)
  (process-children))

(element (docinfo authorgroup author)
  ($person-info$ "Written by:"))

(element (docinfo firstname)
  (process-children))

(element (docinfo surname)
  (process-children))

(element affiliation
  (process-children))

(define ($affiliation-element$)
  (make sequence
    (process-children)
    (if (not (absolute-last-sibling? (current-node)))
	(make empty-element gi: "BR")
	(empty-sosofo))))

(element (affiliation shortaffil) ($affiliation-element$))
(element (affiliation jobtitle) ($affiliation-element$))
(element (affiliation orgname) ($affiliation-element$))
(element (affiliation orgdiv) ($affiliation-element$))

;(element (affiliation address)
;  ($linespecific-content$))

(element (affiliation address)
  ($affiliation-element$))

(element holder
  ;; Copyright holders should be separated by commas.
  (make sequence
    ($charseq$)
    (if (not (last-sibling? (current-node)))
	(literal ", ")
	(empty-sosofo))))




;;;========================================================================
;;; Generic Definition Support
;;;========================================================================
;;; This code implements the "look" shared by all defintions.

(define (process-def)
  (make sequence
    (process-defhead)
    (process-children)))

(define (process-defhead-helper defname defadjectives defsummary)
  (make sequence
    (make empty-element gi: "BR")
    (make element gi: "TABLE" attributes: '(("WIDTH" "100%")
					    ("CELLPADDING" "0")
					    ("BORDER" "0"))
	(make element gi: "TR"
	   (make sequence
	      (make element gi: "TD" defname)
	      (make element gi: "TD" attributes: '(("ALIGN" "RIGHT"))
		 (make element gi: "STRONG"
		    (make sequence
			  (literal "[")
			  defadjectives
			  (literal (attribute-string "dylan-def-name"))
			  (literal "]")))))))
    (make empty-element gi: "HR")
    defsummary))

(define (process-defhead)
  (with-mode defhead
    (process-defhead-helper 
     (process-first-descendant "defname")
     (process-first-descendant "defadjectives")
     (process-first-descendant "defsummary"))))
;    (make sequence
;     (make element
;       gi: "TABLE"
;	attributes: '(("WIDTH" "100%")
;		      ("CELLPADDING" "0")
;		      ("BORDER" "0"))
;	(make element
;	  gi: "TR"
;	  (make sequence
;	    (make element
;	      gi: "TD"
;	      (process-first-descendant "DefName"))
;	    (make element
;	      gi: "TD"
;	      attributes: '(("ALIGN" "RIGHT"))
;	      (make element
;		gi: "STRONG"
;		(make sequence
;		  (literal "[")
;		  (process-first-descendant "defadjectives")
;		  (literal (attribute-string "dylan-def-name"))
;		  (literal "]")))))))
;      (make empty-element gi: "HR")
;      (process-first-descendant "defsummary"))))

(mode defhead
  (element defname
    (make element
      gi: "B"
      ($dylan-literal$)))
  (element defadjectives
    (make sequence
      (process-children)
      (literal " "))) 
  (element defsummary
    (make element
      gi: "BLOCKQUOTE")))

(element defname
  (empty-sosofo))
(element defadjectives
  (empty-sosofo))
(element defsummary
  (empty-sosofo))

(define (have-children? #!optional (node (current-node)))
  (not (node-list-empty? (children node))))

(define (have-child? type #!optional (node (current-node)))
  (not (node-list-empty? (select-elements (children node) (normalize type)))))

(define ($raw-definition-section$ title contents)
  (make sequence
    (make element
      gi: "P"
      (make element
	gi: "STRONG"
	title))
    (make element
      gi: "BLOCKQUOTE"
      contents)))

(define (process-children-or-none)
  (if (have-children?)
      (process-children)
      (literal "None.")))

(define (process-section name #!optional (sect (process-children-or-none)))
  ($raw-definition-section$ (literal name) sect))

(element defdescription
  (process-section "Description"))

(element defsection
  ($raw-definition-section$ (with-mode defsection-title
			      (process-first-descendant "TITLE"))
			    (process-children)))

(element (defsection title)
  (empty-sosofo))

(mode defsection-title
  (element (DefSection Title)
    (process-children)))

;(define (dylan-object-type) (make entity-ref name: "object"))
(define (dylan-object-type)
  ($dylan-literal$ (make sequence
		     (make entity-ref
		       name: "lt")
		     (literal "object>"))))


;;;========================================================================
;;; Parameter lists
;;;========================================================================
;;; Parameter list processing shared between a number of different defining
;;; forms. Parts of this are often overridden.

(define (process-parameter-section name #!optional (node (current-node)))
  (process-section name
		   (if (have-children?)
		       (make element
			 gi: "TABLE"
			 attributes: '(("BORDER" "0")
				       ("COLUMNSPACING" "0")))
		       (literal "None."))))

(define (process-param #!optional (type-label "An instance of "))
  (make element
    gi: "TR"
    (make sequence
      (make element
	gi: "TD"
	attributes: '(("VALIGN" "TOP"))
	(process-first-descendant "param-name"))
      (make element
	gi: "TD"
	(make sequence
	  (literal type-label)
	  (if (have-child? "param-type")
	      (process-first-descendant "param-type")
	      (dylan-object-type))
	  (literal ". ")
	  (if (have-child? "param-summary")
	      (process-first-descendant "param-summary")
	      (empty-sosofo))
	  (if (have-child? "param-default")
	      (make sequence
		(literal " defaults to ")
		(process-first-descendant "param-default")
		(literal "."))
	      (empty-sosofo)))))))

(define (process-var #!optional (variable-label "variable.  "))
  (make sequence
    (make element
      gi: "TR"
      (make sequence
        (make element
          gi: "TD"
          attributes: '(("VALIGN" "TOP"))
          (process-first-descendant "variable-name"))
        (make element
          gi: "TD"
            (literal variable-label))))
    (make element gi: "TR"
      (make sequence
        (make element gi: "TD" attributes: '(("VALIGN" "TOP"))
          (process-first-descendant "binds-to"))
        (make element gi: "TD"
          (literal "Expression."))))))

(element var-param
  (with-mode var-param
    (process-var)))

(element param
  (process-param))

(element rest-param
  (process-param "Instances of "))

(element key-param
  (with-mode keyword-param
    (process-param)))

(element param-name
  (make element
    gi: "EM"))

(element param-type
  ($dylan-literal$))

(element param-default
  ($dylan-literal$))

(element param-summary
  (process-children))

(element dkeyword
  ($dylan-literal$
    (make sequence
	(process-children)
        (literal ":"))))

(element dclass
  ($dylan-literal$
    (make sequence
      (make entity-ref name: "lt")
      (process-children)
      (make entity-ref name: "gt"))))

(mode keyword-param
  (element param-name
    ($dylan-literal$ (make sequence
		       (process-children)
		       (literal ":")))))

(mode var-param
  (element variable-name
    (make element gi: "EM" (process-children)))
  (element BindsTo (make element gi: "EM" (process-children))))

;;;========================================================================
;;; Define Class
;;;========================================================================
;;; Everything needed to define a class in the standard DRM style.

(element dylan-class-def
  (process-def))

(element def-supers
  (process-section "Superclasses"))

(element def-super
  (make sequence
    ($dylan-literal$)
    (literal " ")))

(element def-init-keywords
  (process-parameter-section "Initialization Keywords"))


;;;========================================================================
;;; Define Constant & Define Variable
;;;========================================================================
;;; We group these two forms together because they're similar.

(element dylan-constant-def
  (process-def))

(element dylan-variable-def
  (process-def))

(element deftype
  (process-section "Type" ($dylan-literal$)))

(element defvalue
  (process-section "Value" ($dylan-literal$)))


;;;========================================================================
;;; Define Function, Method & Generic
;;;========================================================================
;;; These all look the same for now.

(define (process-function-def)
  (make sequence
    (process-defhead)
    (process-function-synopsis)
    (process-children)))

(element dylan-function-def
  (process-function-def))

(define (char-description title param type)
  (process-section title
    (make sequence
      (make element gi: "EM" (literal param))
      (literal "An instance of ")
      ($dylan-literal$
       (make sequence
	 (make entity-ref name: "lt")
	 (literal type)
	 (make entity-ref name: "gt"))))))

(define (char-summary attrib #!optional (show-false #f))
    (make sequence
      (literal "Returns ")
      ($dylan-literal$ (literal "#t"))
      (literal " if the character is ")
      (literal (attribute-string attrib))
      (if show-false
	  (make sequence
	   (literal ", ")
	   ($dylan-literal$ (literal "#f"))
	   (literal " otherwise."))
	(literal "."))))

(element dylan-char-fn-def
 (make sequence
  (with-mode defhead  ;; for the definition-head
   (process-defhead-helper
    (make element gi: "B" 
	  ($dylan-literal$ (literal (attribute-string "name"))))
    (literal "") ; adjectives
    (char-summary "condition")))  ; summary
  (with-mode function-synopsis ;; for the synopsis
   (process-section "Synopsis"
    (make sequence
     (literal (attribute-string "name"))
     (literal " (")
     (make element gi: "EM" (literal "character"))
     (literal ") => (")
     (make element gi: "EM" (literal "answer"))
     (literal ")"))))
  (char-description "Parameters" "character" "character")
  (char-description "Returns" "answer" "boolean")
  (process-section "Description"
   (char-summary "elaboration" #t))))

(element dylan-generic-def
  (process-function-def))

(element dylan-method-def
  (process-function-def))

;; right now a DylanMacroDef is a Statement macro only; ...
;; I'll re-write this later to include function macros
;; and definer macros, but if someone else does this first,
;; I won't complain.  Doug Auclair, Dec 3, 2000, dauclair@hotmail.com

(define (process-macro-def)
  (make sequence
    (process-defhead)
    (process-macro-synopsis)
    (process-children)))

(element dylan-macro-def
  (process-macro-def))

(define (process-macro-synopsis #!optional (node (current-node)))
  (with-mode function-synopsis
    (process-section "Synopsis"
                     (make sequence
                       (process-first-descendant "defname")
                       (process-first-descendant "defparameters")
                       (make element gi: "EM" (literal " body"))
		       (literal " end")))))

;; now back to our regularly scheduled programming ...

(element EM (make element gi: "EM"))

(element defparameters
  (process-parameter-section "Parameters"))

(element defreturns
  (process-parameter-section "Return Values"))

(define (process-function-synopsis #!optional (node (current-node)))
  (with-mode function-synopsis
    (process-section "Synopsis"
		     (make sequence
		       (process-first-descendant "defname")
		       (process-first-descendant "defparameters")
		       (process-first-descendant "defreturns")))))

(define (maybe-insert-spacer #!optional (spacer (literal ", ")))
  (if (not (absolute-first-sibling? (current-node)))
      spacer
      (empty-sosofo)))

(mode function-synopsis

  (element defname
    (process-children))

  (element param-name
    (make element
      gi: "EM"))

  (element variable-name
    (make element
      gi: "EM"))

  (element binds-to
    (make element
      gi: "EM"))

  (element defparameters
    (make sequence
      (literal " (")
      (process-children)
      (literal ")")))

  (element defreturns
    (make sequence
      (literal " => (")
      (process-children)
      (literal ")")))

  (element varparam
    (make sequence
      (maybe-insert-spacer)
      (process-first-descendant "variable-name")
      (literal " = ")
      (process-first-descendant "binds-to")))

  (element Param
    (make sequence
      (maybe-insert-spacer)
      (process-first-descendant "param-name")))

  (element rest-param
    (make sequence
      (maybe-insert-spacer)
      (literal "#rest ")
      (process-first-descendant "param-name")))

  (element key-param
    (make sequence
      (maybe-insert-spacer)
      (if (first-sibling? (current-node))
	  (literal "#key ")
	  (empty-sosofo))
      (process-first-descendant "param-name")))
  
  (element all-keys
    (make sequence
      (maybe-insert-spacer)
      (if (first-sibling? (current-node))
	  (literal "#key ")
	  (empty-sosofo))
      (literal "#all-keys")))
  )


;;;========================================================================
;;; Cruft
;;;========================================================================
;;; To be organized and rewritten...

(define (process-param-list)
  (if (have-children?)
       (process-children)
       (literal "()")))

(define (insert-spacer-unless-last #!optional (node (current-node)))
;;  (if (not (last-sibling? node))
;;      (literal " ")
;;      (empty-sosofo)))
;; XXX - last-sibling? ignores siblings of other types. For now, hack:
  (literal " "))

(define (parameter-keyword name)
  (make element
    gi: "B"
    (literal name)))

(mode unused
  (element defparameters
    (make sequence
      (make element
	gi: "CODE"
	(literal " "))
      (process-param-list)))
  
  (element defreturns
    (make sequence
    (make element
      gi: "CODE"
      (literal " => "))
    (process-param-list)))
  
  (element defparam
    (process-children))
  
  (element param-name
    (make sequence
      (make element
	gi: "I")
      (insert-spacer-unless-last (parent (current-node)))))
  
  (element param-type
    (empty-sosofo))

  (element param-summary
    (empty-sosofo))
  
  (element rest-type
    (empty-sosofo))
  
  (element Rest
    (make sequence
      (parameter-keyword "#rest")
      (literal " ")
      (process-children)))
  
  (element Key
    (make sequence
      (parameter-keyword "#key")
      (if (have-children?)
	  (literal " ")
	  (empty-sosofo))
      (process-children)
      (insert-spacer-unless-last)))
  
  (element AllKeys
    (parameter-keyword "#all-keys")))
  
(define (parameters-longform label)
  (if (have-children?)
      (make element
	gi: "BLOCKQUOTE" 
	(make sequence
	  (make element
	    gi: "P"
	    (make element
	      gi: "STRONG"
	      (literal label)))
	  (make element
	    gi: "TABLE"
	    attributes: '(("BORDER" "0")
			  ("CELLSPACING" "10"))
	    (process-children))))
      (empty-sosofo)))

(mode paramdesc
  (element param-name
    (empty-sosofo))
  (element param-type
    (make sequence
      (literal "An instance of type ")
      (make element
	gi: "CODE")
      (literal ". ")))
  (element rest-type
    (make sequence
      (literal "Instances of type ")
      (make element
	gi: "CODE")
      (literal ". ")))
  (element param-singleton
    (make sequence
      (literal "The object ")
      (make element
	gi: "CODE")
      (literal ". ")))
  (element param-summary
    (process-children)))

(define (describe-param)
  (make element
    gi: "TR"
    (make sequence
      (make element
	gi: "TD"
	(process-first-descendant "param-name"))
      (make element
	gi: "TD"
	(with-mode paramdesc
	  (process-children))))))

(mode parameter-summary
  (element defparameters
    (parameters-longform "Parameters"))
  (element defreturns
    (parameters-longform "Return Values"))
  (element defparam
    (describe-param))
  (element Rest
    (if (have-children?)
	(describe-param)
	(empty-sosofo)))
  (element Key
    (process-children))
  (element AllKeys
    (empty-sosofo))
  (element param-name
    (make element
      gi: "I")))

;;; Defining Constants

</style-specification-body>
</style-specification>
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>

-->

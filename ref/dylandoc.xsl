<?xml version="1.0" encoding="US-ASCII"?>
<!--
    DylanDoc XSL Stylesheet - format DylanDoc documents for web or print
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
  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl"/> 

  <!--
    Dylan Literal Tags.  We use mono-spaced sequential text,
    since these are program elements
  -->
  <xsl:template match="dlibrary" name="dylan.global.dlibrary">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>

  <xsl:template match="dmodule" name="dylan.global.dmodule">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>

  <xsl:template match="dname" name="dylan.global.dname">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>

  <xsl:template match="dlit" name="dylan.global.dlit">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>

  <xsl:template match="dparam" name="dylan.global.dparam">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>

  <xsl:template match="em">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>

  <xsl:template match="key-param">
    <table border="0" columnspacing="0">
      <tr>
	<td valign="top">
	  <code>
	    <xsl:apply-templates select="param-name"/>
	    <xsl:text>:</xsl:text>
	  </code>
	</td>
	<td>
	  <xsl:text>An instance of </xsl:text>
	  <xsl:apply-templates select="param-type"/>
	  <xsl:apply-templates select="param-summary"/>
	</td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="param">
    <table border="0" columnspacing="0">
      <tr>
	<td valign="top">
	  <em>
	    <xsl:apply-templates select="param-name"/>
	  </em>
	</td>
	<td>
	  <xsl:text>An instance of </xsl:text>
	  <xsl:apply-templates select="param-type"/>
	  <xsl:apply-templates select="param-summary"/>
	</td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="param-name">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="param-type">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>

  <xsl:template match="param-summary">
    <xsl:apply-templates/>
  </xsl:template>

  <!--
    Dylan Class definitions.
  -->
  <xsl:template match="dylan-class-def">
    <table width="100%" cellpadding="0" border="0">
      <tr>
	<xsl:apply-templates select="classname"/>
	<td align="right">
	  <strong>
	    <xsl:text>[</xsl:text>
	    <xsl:apply-templates select="defadjectives"/>
	    <xsl:text>Class]</xsl:text>
	  </strong>
	</td>
      </tr>
    </table>
    <hr/>
    <xsl:apply-templates select="defsummary"/>
    <xsl:apply-templates select="defsupers"/>
    <xsl:apply-templates select="def-init-keywords"/>
    <xsl:apply-templates select="defdescription"/>
    <xsl:apply-templates select="defsection"/>
    <br/>
  </xsl:template>

  <xsl:template match="dylan-class-def/classname">   
    <td>
      <b>
	<xsl:call-template name="inline.monoseq"/>
      </b>
    </td>
  </xsl:template>

  <xsl:template match="dylan-class-def/defsupers">
    <strong>Superclasses</strong><br/>
    <blockquote>
      <xsl:apply-templates/>
    </blockquote>
    <p/>
  </xsl:template>

  <xsl:template match="dylan-class-def/def-init-keywords">
    <strong>Initialization Keywords</strong><br/>
    <xsl:choose>
      <xsl:when test="not(normalize-space(.))">
	<blockquote><xsl:text>None</xsl:text></blockquote>
      </xsl:when>
      <xsl:otherwise>
	<blockquote>
	  <xsl:apply-templates/>
	</blockquote>
      </xsl:otherwise>
    </xsl:choose>
    <p/>
  </xsl:template>

  <!--
    Dylan Function definitions.
  -->
  <xsl:template match="dylan-function-def">
    <table width="100%" cellpadding="0" border="0">
      <tr>
	<xsl:apply-templates select="function"/>
	<td align="right">
	  <strong>
	    <xsl:text>[</xsl:text>
	    <xsl:apply-templates select="defadjectives"/>
	    <xsl:text>Function]</xsl:text>
	  </strong>
	</td>
      </tr>
    </table>
    <hr/>
    <xsl:apply-templates select="defsummary"/>
    <xsl:apply-templates select="defsynopsis"/>
    <xsl:apply-templates select="defparameters"/>
    <xsl:apply-templates select="defreturns"/>
    <xsl:apply-templates select="defdescription"/>
    <xsl:apply-templates select="defsection"/>
    <br/>
  </xsl:template>

  <xsl:template match="dylan-function-def/function">
    <td>
      <b>
	<xsl:call-template name="inline.monoseq"/>
      </b>
    </td>
  </xsl:template>

  <!--
    Shared Dylan Definition Formatters
  -->
  <xsl:template match="defadjectives">
    <xsl:call-template name="inline.charseq"/>
  </xsl:template>

  <xsl:template match="defsummary">
    <blockquote>
      <xsl:apply-templates/>
    </blockquote>
    <p/>
  </xsl:template>

  <xsl:template match="defreturns">
    <strong>Return Values</strong><br/>
    <xsl:choose>
      <xsl:when test="not(normalize-space(.))">
	<blockquote><xsl:text>None</xsl:text></blockquote>
      </xsl:when>
      <xsl:otherwise>
	<blockquote>
	  <xsl:apply-templates/>
	</blockquote>
      </xsl:otherwise>
    </xsl:choose>
    <p/>
  </xsl:template>

  <xsl:template match="defparameters">
    <strong>Parameters</strong><br/>
    <xsl:choose>
      <xsl:when test="not(normalize-space(.))">
	<blockquote><xsl:text>None</xsl:text></blockquote>
      </xsl:when>
      <xsl:otherwise>
	<blockquote>
	  <xsl:apply-templates/>
	</blockquote>
      </xsl:otherwise>
    </xsl:choose>
    <p/>
  </xsl:template>

  <xsl:template match="defdescription">
    <strong>Description</strong><br/>
    <xsl:choose>
      <xsl:when test="not(normalize-space(.))">
	<blockquote><xsl:text>None</xsl:text></blockquote>
      </xsl:when>
      <xsl:otherwise>
	<blockquote>
	  <xsl:apply-templates/>
	</blockquote>
      </xsl:otherwise>
    </xsl:choose>
    <p/>
  </xsl:template>

  <xsl:template match="defsection">
    <strong>Section</strong><br/>
    <blockquote>
      <xsl:apply-templates/>
    </blockquote>
    <p/>
  </xsl:template>

  <xsl:template match="defsynopsis">
    <strong>Synopsis</strong>
    <blockquote>
      <xsl:apply-templates/>
    </blockquote>
    <p/>
  </xsl:template>

</xsl:stylesheet>

<!--

;;;;=======================================================================
;;;; HTML Stylesheet
;;;;=======================================================================
;;;; This stylesheet uses James Clark's extensions for creating HTML from
;;;; a DSSSL stylesheet.

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

-->
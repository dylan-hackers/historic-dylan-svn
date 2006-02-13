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

  <!--
    Dylan Literal Tags.  We use mono-spaced sequential text,
    since these are program elements
  -->
  <xsl:template match="dlibrary|dmodule|dname|dclass|dkeyword|dlit|dparam">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>

  <xsl:template match="em">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>

  <xsl:template match="varparam|param|key-param|rest-param">
    <table border="0" columnspacing="0">
      <tr>
	<td valign="top">
	  <xsl:choose>
	    <xsl:when test="self::key-param">
	       <code>
		 <xsl:apply-templates select="param-name"/>
		 <xsl:text>:</xsl:text>
	      </code>
	    </xsl:when>
	    <xsl:otherwise>
	      <em>
		<xsl:apply-templates select="variable-name"/>
		<xsl:apply-templates select="param-name"/>
		<xsl:apply-templates select="binds-to"/>
	      </em>
	    </xsl:otherwise>
	  </xsl:choose>
	</td>
	<td>
	  <xsl:choose>
	    <xsl:when test="param-type">
	      <xsl:text>An instance of </xsl:text>
	      <xsl:apply-templates select="param-type"/>
	    </xsl:when>
	    <xsl:when test="variable-name">
	      <xsl:text>Variable</xsl:text>
	    </xsl:when>
	    <xsl:when test="binds-to">
	      <xsl:text>Expression</xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>An instance of </xsl:text>
	      <code>
		<xsl:text>&lt;object&gt;</xsl:text>
	      </code>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:text>.  </xsl:text>
	  <xsl:apply-templates select="param-summary"/>
	</td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="variable-name|binds-to|param-name|param-summary">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="param-type">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>

  <!--
    Dylan Constant/Variable definitions.
  -->
  <xsl:template match="dylan-constant-def|dylan-variable-def">
    <table width="100%" cellpadding="0" border="0">
      <tr>
	<xsl:choose>
	  <xsl:when test="self::dylan-constant-def">
	    <xsl:apply-templates select="constant"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="defname"/>
	  </xsl:otherwise>
	</xsl:choose>
	<td align="right">
	  <strong>
	    <xsl:text>[</xsl:text>
	    <xsl:choose>
	      <xsl:when test="self::dylan-constant-def">
		<xsl:text>Constant]</xsl:text>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text>Variable]</xsl:text>
	      </xsl:otherwise>
	    </xsl:choose>
	  </strong>
	</td>
      </tr>
    </table>
    <hr/>
    <xsl:apply-templates select="defsummary"/>
    <xsl:apply-templates select="deftype"/>
    <xsl:apply-templates select="defvalue"/>
    <xsl:apply-templates select="defdescription"/>
    <xsl:apply-templates select="defsection"/>
    <br/>
  </xsl:template>

  <xsl:template match="dylan-constant-def/constant">   
    <td>
      <b>
	<xsl:call-template name="inline.monoseq"/>
      </b>
    </td>
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

  <xsl:template match="defsuper">
    <xsl:call-template name="inline.monoseq"/>
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
  <xsl:template match="dylan-function-def|dylan-generic-def|dylan-method-def|dylan-macro-def">
    <table width="100%" cellpadding="0" border="0">
      <tr>
	<xsl:choose>
	  <xsl:when test="self::dylan-function-def">
	    <xsl:apply-templates select="function"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="defname"/>
	  </xsl:otherwise>
	</xsl:choose>
	<td align="right">
	  <strong>
	    <xsl:text>[</xsl:text>
	    <xsl:apply-templates select="defadjectives"/>
	    <xsl:choose>
	      <xsl:when test="self::dylan-method-def">
		<xsl:text>Method]</xsl:text>
	      </xsl:when>
	      <xsl:when test="self::dylan-generic-def">
		<xsl:text>Generic]</xsl:text>
	      </xsl:when>
	      <xsl:when test="self::dylan-macro-def">
		<xsl:text>Macro]</xsl:text>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text>Function]</xsl:text>
	      </xsl:otherwise>
	    </xsl:choose>
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

  <xsl:template match="defname|dylan-function-def/function|dylan-class-def/classname">
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
    <xsl:text> </xsl:text>
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

  <xsl:template match="defvalue">
    <strong>Value</strong>
    <blockquote>
      <xsl:apply-templates/>
    </blockquote>
    <p/>
  </xsl:template>

  <xsl:template match="deftype">
    <strong>Type</strong>
    <blockquote>
      <xsl:apply-templates/>
    </blockquote>
    <p/>
  </xsl:template>

</xsl:stylesheet>

<!--

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

-->

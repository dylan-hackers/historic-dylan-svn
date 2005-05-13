<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- ======================== dylanFunction ======================== -->

<xsl:template match="*[contains(@class,' dylanFunction/dylanFunctionDef ')]">
<div class="section">
  <h4 class="sectiontitle">Signature</h4>
  <xsl:call-template name="function-signature"/>
</div>
<div class="section">
  <h4 class="sectiontitle">Arguments</h4>
  <xsl:call-template name="function-arguments"/>
</div>
<div class="section">
  <h4 class="sectiontitle">Values</h4>
  <xsl:call-template name="function-returns"/>
</div>
</xsl:template>

<xsl:template name="function-signature">
  <p class="signature">
    <!-- function name -->
    <samp><xsl:apply-templates select="ancestor::*[contains(@class,' apiRef/apiRef ')]/*[contains(@class, ' apiRef/apiName ')]" mode='text-only'/></samp>
    <xsl:text>&#xA0;</xsl:text>
    <!-- function required parameters -->
    <xsl:apply-templates select="*[contains(@class, ' dylanFunction/dylanFunctionParam ')]" mode="signature"/>
    <!-- function #rest parameters -->
    <xsl:if test="*[contains(@class, ' dylanFunction/dylanFunctionRestParam ')]">
      <samp>#rest</samp>
      <xsl:text>&#xA0;</xsl:text>
      <xsl:apply-templates select="*[contains(@class, ' dylanFunction/dylanFunctionRestParam ')]" mode="signature"/>
    </xsl:if>
    <!-- function #key parameters -->
    <xsl:if test="*[contains(@class, ' dylanFunction/dylanFunctionKeywordParam ') or contains(@class, ' dylanFunction/dylanFunctionAllKeywords ')]">
      <samp>#key</samp>
      <xsl:text>&#xA0;</xsl:text>
      <xsl:apply-templates select="*[contains(@class, ' dylanFunction/dylanFunctionKeywordParam ')]" mode="signature"/>
      <xsl:apply-templates select="*[contains(@class, ' dylanFunction/dylanFunctionAllKeywords ')]" mode="signature"/>
    </xsl:if>
    <!-- Arrow -->
    <xsl:call-template name="signature-arrow"/>
    <xsl:text>&#xA0;</xsl:text>
    <!-- Return values -->
    <xsl:apply-templates select="*[contains(@class, ' dylanFunction/dylanFunctionReturn ')]" mode="signature"/>
    <!-- #rest return values -->
    <xsl:if test="*[contains(@class, ' dylanFunction/dylanFunctionRestReturn ')]">
      <samp>#rest</samp>
      <xsl:text>&#xA0;</xsl:text>
      <xsl:apply-templates select="*[contains(@class, ' dylanFunction/dylanFunctionRestReturn ')]" mode="signature"/>
    </xsl:if>
  </p>
</xsl:template>

<xsl:template name="signature-arrow">
  <xsl:text>&#x21D2;</xsl:text>
</xsl:template>

<xsl:template match="*[contains(@class,' apiOperation/apiParam ')]" mode="signature">
<span class="var">
  <xsl:apply-templates select="*[contains(@class,' apiRef/apiItemName ')]" mode='text-only'/>
</span>
  <xsl:text>&#xA0;</xsl:text>
</xsl:template>

<xsl:template match="*[contains(@class,' dylanFunction/dylanFunctionAllKeywords ')]" mode="signature">
  <samp>#all-keys</samp>
  <xsl:text>&#xA0;</xsl:text>
</xsl:template>

<xsl:template match="*[contains(@class,' apiOperation/apiReturn ')]" mode="signature">
<span class="var">
  <xsl:apply-templates select="*[contains(@class,' apiRef/apiItemName ')]" mode='text-only'/>
</span>
  <xsl:text>&#xA0;</xsl:text>
</xsl:template>

<xsl:template name="function-arguments">
  <xsl:choose>
    <xsl:when test="*[contains(@class,' apiOperation/apiParam ')]">
      <dl><xsl:apply-templates select="*[contains(@class,' apiOperation/apiParam ')]" mode="argument"/></dl>
    </xsl:when>
    <xsl:otherwise>
      <p>None.</p>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="*[contains(@class,' dylanFunction/dylanFunctionRestParam ')]" mode="argument" priority='1'>
<dt><span class="var">
  <xsl:apply-templates select="*[contains(@class,' apiRef/apiItemName ')]" mode='text-only'/>
</span></dt>
<dd>Zero or more instances of <xsl:apply-templates select="*[contains(@class,' apiOperation/apiOperationClassifier ')]"/>. 
<xsl:apply-templates select="*[contains(@class,' apiRef/apiDefNote ')]"/>
</dd>
</xsl:template>

<xsl:template match="*[contains(@class,' apiOperation/apiParam ')]" mode="argument">
<dt><span class="var">
  <xsl:apply-templates select="*[contains(@class,' apiRef/apiItemName ')]" mode='text-only'/>
</span></dt>
<dd>An instance of <xsl:apply-templates select="*[contains(@class,' apiOperation/apiOperationClassifier ')]"/>. 
<xsl:apply-templates select="*[contains(@class,' apiRef/apiDefNote ')]"/>
</dd>
</xsl:template>

<xsl:template name="function-returns">
  <xsl:choose>
    <xsl:when test="*[contains(@class,' apiOperation/apiReturn ')]">
      <dl><xsl:apply-templates select="*[contains(@class,' apiOperation/apiReturn ')]" mode="return"/></dl>
    </xsl:when>
    <xsl:otherwise>
      <p>None.</p>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="*[contains(@class,' dylanFunction/dylanFunctionRestReturn ')]" mode="return" priority='1'>
<dt><span class="var">
  <xsl:apply-templates select="*[contains(@class,' apiRef/apiItemName ')]" mode='text-only'/>
</span></dt>
<dd>Zero or more instances of <xsl:apply-templates select="*[contains(@class,' apiOperation/apiOperationClassifier ')]"/>. 
<xsl:apply-templates select="*[contains(@class,' apiRef/apiDefNote ')]"/>
</dd>
</xsl:template>

<xsl:template match="*[contains(@class,' apiOperation/apiReturn ')]" mode="return">
<dt><span class="var">
  <xsl:apply-templates select="*[contains(@class,' apiRef/apiItemName ')]" mode='text-only'/>
</span></dt>
<dd>An instance of <xsl:apply-templates select="*[contains(@class,' apiOperation/apiOperationClassifier ')]"/>. 
<xsl:apply-templates select="*[contains(@class,' apiRef/apiDefNote ')]"/>
</dd>
</xsl:template>

<!-- ======================== dylanClass ======================== -->

<xsl:template match="*[contains(@class,' dylanClass/dylanClassDef ')]">
<div class="section">
  <h4 class="sectiontitle">Superclasses</h4>
  <xsl:call-template name="class-superclasses"/>
</div>
<div class="section">
  <h4 class="sectiontitle">Init-keywords</h4>
  <xsl:call-template name="class-init-keywords"/>
</div>
</xsl:template>

<xsl:template name="class-superclasses">
  <xsl:choose>
    <xsl:when test="*[contains(@class,' dylanClass/dylanSuperClass ')]">
      <p><xsl:apply-templates select="*[contains(@class,' dylanClass/dylanSuperClass ')]"/></p>
    </xsl:when>
    <xsl:otherwise>
      <p>None.</p>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="class-init-keywords">
  <xsl:choose>
    <xsl:when test="*[contains(@class,' dylanClass/dylanInitKeyword ')]">
      <dl><xsl:apply-templates select="*[contains(@class,' dylanClass/dylanInitKeyword ')]"/></dl>
    </xsl:when>
    <xsl:otherwise>
      <p>None.</p>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="*[contains(@class,' dylanClass/dylanInitKeyword ')]">
<dt><samp>
  <xsl:apply-templates select="*[contains(@class,' apiRef/apiItemName ')]" mode='text-only'/>
</samp></dt>
<dd>An instance of <xsl:apply-templates select="*[contains(@class,' apiClassifier/apiOtherClassifier ')]"/>. 
<xsl:apply-templates select="*[contains(@class,' apiRef/apiDefNote ')]"/>
</dd>
</xsl:template>

</xsl:stylesheet>

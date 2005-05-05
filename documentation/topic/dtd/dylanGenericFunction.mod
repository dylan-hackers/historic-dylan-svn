<!-- Specialization of the DITA reference topic for Dylan-language functions -->

<!-- Default element entities -->

<!ENTITY % dylanGenericFunction "dylanGenericFunction"> 
<!ENTITY % dylanGenericFunctionDetail "dylanGenericFunctionDetail"> 
<!ENTITY % dylanGenericFunctionDef "dylanGenericFunctionDef">
<!ENTITY % dylanGenericFunctionSealing "dylanGenericFunctionSealing">

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanGenericFunction-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanGenericFunction ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanGenericFunctionDetail;)?,
                           (%related-links;)?,
			   (%dylanGenericFunction-info-types;)*)>
<!ATTLIST dylanGenericFunction id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanGenericFunctionDetail ((%dylanGenericFunctionDef;)?, (%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanGenericFunctionDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanGenericFunctionDef ((%dylanGenericFunctionSealing;)?,
	  	            (%dylanFunctionParam;)*,
                            (%dylanFunctionRestParam;)?,
                            (%dylanFunctionKeywordParam;)*,
                            (%dylanFunctionAllKeywords;)?,
                            (%dylanFunctionReturn;)*,
                            (%dylanFunctionRestReturn;)?)>
<!ATTLIST dylanGenericFunctionDef spectitle CDATA #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanGenericFunctionSealing EMPTY>
<!ATTLIST dylanGenericFunctionSealing name CDATA #FIXED "sealing"
                          value (sealed|open|dynamic) "sealed"
                          %univ-atts;
                          outputclass CDATA #IMPLIED>


<!-- Specialization attributes -->

<!ATTLIST dylanGenericFunction %global-atts; class CDATA "- topic/topic reference/reference apiRef/apiRef apiOperation/apiOperation dylanFunction/dylanFunction dylanGenericFunction/dylanGenericFunction ">
<!ATTLIST dylanGenericFunctionDetail %global-atts; class CDATA "- topic/body reference/refbody apiRef/apiDetail apiOperation/apiOperationDetail dylanFunction/dylanFunctionDetail dylanGenericFunction/dylanGenericFunctionDetail ">
<!ATTLIST dylanGenericFunctionDef %global-atts; class CDATA "- topic/section reference/section apiRef/apiDef apiOperation/apiOperationDef dylanFunction/dylanFunctionDef dylanGenericFunction/dylanGenericFunctionDef ">
<!ATTLIST dylanGenericFunctionSealing %global-atts; class  CDATA "- topic/state reference/state apiRef/apiQualifier apiOperation/apiQualifier dylanFunction/apiQualifier dylanGenericFunction/dylanGenericFunctionSealing ">

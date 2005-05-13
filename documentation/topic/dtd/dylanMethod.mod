<!-- Specialization of the DITA reference topic for Dylan-language functions -->

<!-- Default element entities -->

<!ENTITY % dylanMethod "dylanMethod"> 
<!ENTITY % dylanMethodDetail "dylanMethodDetail"> 
<!ENTITY % dylanMethodDef "dylanMethodDef">
<!ENTITY % dylanMethodSealing "dylanMethodSealing">

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanMethod-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanMethod ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanMethodDetail;)?,
                           (%related-links;)?,
			   (%dylanMethod-info-types;)*)>
<!ATTLIST dylanMethod id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanMethodDetail ((%dylanMethodDef;)?, (%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanMethodDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanMethodDef ((%dylanMethodSealing;)?,
	  	            (%dylanFunctionParam;)*,
                            (%dylanFunctionRestParam;)?,
                            (%dylanFunctionKeywordParam;)*,
                            (%dylanFunctionAllKeywords;)?,
                            (%dylanFunctionReturn;)*,
                            (%dylanFunctionRestReturn;)?)>
<!ATTLIST dylanMethodDef spectitle CDATA #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanMethodSealing EMPTY>
<!ATTLIST dylanMethodSealing name CDATA #FIXED "sealing"
                          value (sealed|open) "open"
                          %univ-atts;
                          outputclass CDATA #IMPLIED>


<!-- Specialization attributes -->

<!ATTLIST dylanMethod %global-atts; class CDATA "- topic/topic reference/reference apiRef/apiRef apiOperation/apiOperation dylanFunction/dylanFunction dylanMethod/dylanMethod ">
<!ATTLIST dylanMethodDetail %global-atts; class CDATA "- topic/body reference/refbody apiRef/apiDetail apiOperation/apiOperationDetail dylanFunction/dylanFunctionDetail dylanMethod/dylanMethodDetail ">
<!ATTLIST dylanMethodDef %global-atts; class CDATA "- topic/section reference/section apiRef/apiDef apiOperation/apiOperationDef dylanFunction/dylanFunctionDef dylanMethod/dylanMethodDef ">
<!ATTLIST dylanMethodSealing %global-atts; class  CDATA "- topic/state reference/state apiRef/apiQualifier apiOperation/apiQualifier dylanFunction/apiQualifier dylanMethod/dylanMethodSealing ">

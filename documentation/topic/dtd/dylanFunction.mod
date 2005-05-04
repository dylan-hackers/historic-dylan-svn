<!-- Specialization of the DITA reference topic for Dylan-language libraries -->

<!-- Default element entities -->

<!ENTITY % dylanFunction "dylanFunction"> 
<!ENTITY % dylanFunctionDetail "dylanFunctionDetail"> 
<!ENTITY % dylanFunctionDef "dylanFunctionDef">
<!ENTITY % dylanFunctionParam "dylanFunctionParam">
<!ENTITY % dylanFunctionRestParam "dylanFunctionRestParam">
<!ENTITY % dylanFunctionKeywordParam "dylanFunctionKeywordParam">
<!ENTITY % dylanFunctionAllKeywords "dylanFunctionAllKeywords">
<!ENTITY % dylanFunctionReturn "dylanFunctionReturn">
<!ENTITY % dylanFunctionRestReturn "dylanFunctionRestReturn">

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanFunction-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanFunction   ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanFunctionDetail;)?,
                           (%related-links;)?, (%dylanFunction-info-types;)*)>
<!ATTLIST dylanFunction   id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanFunctionDetail ((%dylanFunctionDef;)?, (%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanFunctionDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanFunctionDef ((%dylanFunctionParam;)*,
                            (%dylanFunctionRestParam;)?,
                            (%dylanFunctionKeywordParam;)*,
                            (%dylanFunctionAllKeywords;)?,
                            (%dylanFunctionReturn;)*,
                            (%dylanFunctionRestReturn;)?)>
<!ATTLIST dylanFunctionDef spectitle CDATA #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>


<!ELEMENT dylanFunctionParam ((%apiItemName;), (%apiType;|%apiOperationClassifier;)?, (%apiDefNote;)?)>
<!ATTLIST dylanFunctionParam keyref NMTOKEN #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanFunctionRestParam  ((%apiItemName;), (%apiType;|%apiOperationClassifier;)?, (%apiDefNote;)?)>
<!ATTLIST dylanFunctionRestParam  keyref NMTOKEN #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanFunctionKeywordParam  ((%apiItemName;), (%apiType;|%apiOperationClassifier;)?, (%apiDefNote;)?)>
<!ATTLIST dylanFunctionKeywordParam  keyref NMTOKEN #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanFunctionAllKeywords EMPTY>
<!ATTLIST dylanFunctionAllKeywords name CDATA #FIXED "all-keys"
                          value CDATA #FIXED "all-keys"
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanFunctionReturn ((%apiItemName;), (%apiType;|%apiOperationClassifier;)?, (%apiDefNote;)?)>
<!ATTLIST dylanFunctionParam keyref NMTOKEN #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanFunctionRestReturn ((%apiItemName;), (%apiType;|%apiOperationClassifier;)?, (%apiDefNote;)?)>
<!ATTLIST dylanFunctionRestParam  keyref NMTOKEN #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!-- Specialization attributes -->

<!ATTLIST dylanFunction %global-atts; class CDATA "- topic/topic reference/reference apiRef/apiRef apiOperation/apiOperation dylanFunction/dylanFunction ">
<!ATTLIST dylanFunctionDetail %global-atts; class CDATA "- topic/body reference/refbody apiRef/apiDetail apiOperation/apiOperationDetail dylanFunction/dylanFunctionDetail ">
<!ATTLIST dylanFunctionDef %global-atts; class CDATA "- topic/section reference/section apiRef/apiDef apiOperation/apiOperationDef dylanFunction/dylanFunctionDef ">

<!ATTLIST dylanFunctionParam %global-atts; class CDATA "- topic/ph reference/ph apiRef/apiDefItem apiOperation/apiParam dylanFunction/dylanFunctionParam ">
<!ATTLIST dylanFunctionRestParam %global-atts; class CDATA "- topic/ph reference/ph apiRef/apiDefItem apiOperation/apiParam dylanFunction/dylanFunctionRestParam ">
<!ATTLIST dylanFunctionAllKeywords %global-atts; class  CDATA "- topic/state reference/state apiRef/apiQualifier apiOperation/apiQualifier dylanFunction/dylanFunctionAllKeywords ">
<!ATTLIST dylanFunctionKeywordParam %global-atts; class CDATA "- topic/ph reference/ph apiRef/apiDefItem apiOperation/apiParam dylanFunction/dylanFunctionKeywordParam ">
<!ATTLIST dylanFunctionReturn %global-atts; class CDATA "- topic/ph reference/ph apiRef/apiDefItem apiOperation/apiReturn dylanFunction/dylanFunctionReturn ">
<!ATTLIST dylanFunctionRestReturn %global-atts; class CDATA "- topic/ph reference/ph apiRef/apiDefItem apiOperation/apiReturn dylanFunction/dylanFunctionRestReturn ">

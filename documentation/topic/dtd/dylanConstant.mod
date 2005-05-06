<!-- Specialization of the DITA reference topic for Dylan-language macros -->

<!-- Default element entities -->

<!ENTITY % dylanConstant "dylanConstant"> 
<!ENTITY % dylanConstantDetail "dylanConstantDetail"> 
<!ENTITY % dylanConstantDef "dylanConstantDef">

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanConstant-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanConstant      ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanConstantDetail;)?,
                           (%related-links;)?, (%dylanConstant-info-types;)*)>
<!ATTLIST dylanConstant      id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanConstantDetail ((%dylanConstantDef;)?,
                            (%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanConstantDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanConstantDef   ((%apiQualifier;)*,
                              (%apiType;|%apiValueClassifier;)?)>
<!ATTLIST dylanConstantDef   spectitle CDATA #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!-- Specialization attributes -->

<!ATTLIST dylanConstant %global-atts; class CDATA "- topic/topic reference/reference apiref/apiref apiValue/apiValue dylanVariable/dylanVariable dylanConstant/dylanConstant ">
<!ATTLIST dylanConstantDetail %global-atts; class CDATA  "- topic/body reference/refbody apiRef/apiDetail apiValue/apiValueDetail dylanVariable/dylanVariableDetail dylanConstant/dylanConstantDetail ">
<!ATTLIST dylanConstantDef %global-atts; class  CDATA "- topic/ph reference/ph apiRef/apiDef apiValue/apiValueDef dylanVariable/dylanConstantDef dylanConstant/dylanConstantDef ">

<!-- Specialization of the DITA reference topic for Dylan-language macros -->

<!-- Default element entities -->

<!ENTITY % dylanVariable "dylanVariable"> 
<!ENTITY % dylanVariableDetail "dylanVariableDetail"> 
<!ENTITY % dylanVariableDef "dylanVariableDef">

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanVariable-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanVariable      ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanVariableDetail;)?,
                           (%related-links;)?, (%dylanVariable-info-types;)*)>
<!ATTLIST dylanVariable      id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanVariableDetail ((%dylanVariableDef;)?,
                            (%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanVariableDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanVariableDef   ((%apiQualifier;)*,
                              (%apiType;|%apiValueClassifier;)?)>
<!ATTLIST dylanVariableDef   spectitle CDATA #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!-- Specialization attributes -->

<!ATTLIST dylanVariable %global-atts; class CDATA "- topic/topic reference/reference apiref/apiref apiValue/apiValue dylanVariable/dylanVariable ">
<!ATTLIST dylanVariableDetail %global-atts; class CDATA  "- topic/body reference/refbody apiRef/apiDetail apiValue/apiValueDetail dylanVariable/dylanVariableDetail ">
<!ATTLIST dylanVariableDef %global-atts; class  CDATA "- topic/ph reference/ph apiRef/apiDef apiValue/apiValueDef dylanVariable/dylanVariableDef ">

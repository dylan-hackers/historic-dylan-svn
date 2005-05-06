<!-- Specialization of the DITA reference topic for Dylan-language macros -->

<!-- Default element entities -->

<!ENTITY % dylanMacro "dylanMacro"> 
<!ENTITY % dylanMacroDetail "dylanMacroDetail"> 
<!ENTITY % dylanMacroDef "dylanMacroDef">

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanMacro-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanMacro      ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanMacroDetail;)?,
                           (%related-links;)?, (%dylanMacro-info-types;)*)>
<!ATTLIST dylanMacro      id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanMacroDetail ((%apiSyntax;)+, (%dylanMacroDef;)?,
                            (%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanMacroDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanMacroDef   ((%apiQualifier;)*)>
<!ATTLIST dylanMacroDef   spectitle CDATA #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!-- Specialization attributes -->

<!ATTLIST dylanMacro %global-atts; class CDATA "- topic/topic reference/reference apiRef/apiRef apiOperation/apiOperation dylanMacro/dylanMacro ">
<!ATTLIST dylanMacroDetail %global-atts; class CDATA "- topic/body reference/refbody apiRef/apiDetail apiOperation/apiOperationDetail dylanMacro/dylanMacroDetail ">
<!ATTLIST dylanMacroDef %global-atts; class CDATA "- topic/section reference/section apiRef/apiDef apiOperation/apiOperationDef dylanMacro/dylanMacroDef ">

<!-- Specialization of the DITA reference topic for Dylan-language libraries -->

<!-- Default element entities -->

<!ENTITY % dylanModule "dylanModule"> 
<!ENTITY % dylanModuleDetail "dylanModuleDetail"> 

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanModule-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanModule    ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanModuleDetail;)?,
                           (%related-links;)?, (%dylanModule-info-types;)*)>
<!ATTLIST dylanModule    id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanModuleDetail ((%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanModuleDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!-- Specialization attributes -->

<!ATTLIST dylanModule %global-atts; class CDATA "- topic/topic reference/reference apiRef/apiRef apiPackage/apiPackage dylanModule/dylanModule ">
<!ATTLIST dylanModuleDetail %global-atts; class CDATA "- topic/body reference/refbody apiRef/apiDetail dylanModule/dylanModuleDetail ">

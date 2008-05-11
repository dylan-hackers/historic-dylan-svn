<!-- Specialization of the DITA reference topic for Dylan-language libraries -->

<!-- Default element entities -->

<!ENTITY % dylanLibrary "dylanLibrary"> 
<!ENTITY % dylanLibraryDetail "dylanLibraryDetail"> 

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanLibrary-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanLibrary    ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanLibraryDetail;)?,
                           (%related-links;)?, (%dylanLibrary-info-types;)*)>
<!ATTLIST dylanLibrary    id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanLibraryDetail ((%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanLibraryDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!-- Specialization attributes -->

<!ATTLIST dylanLibrary %global-atts; class CDATA "- topic/topic reference/reference apiRef/apiRef apiPackage/apiPackage dylanLibrary/dylanLibrary ">
<!ATTLIST dylanLibraryDetail %global-atts; class CDATA "- topic/body reference/refbody apiRef/apiDetail dylanLibrary/dylanLibraryDetail ">

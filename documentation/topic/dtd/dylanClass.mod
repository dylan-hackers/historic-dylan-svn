<!-- Specialization of the DITA reference topic for Dylan-language classes -->

<!-- Default element entities -->

<!ENTITY % dylanClass "dylanClass"> 
<!ENTITY % dylanClassDetail "dylanClassDetail"> 
<!ENTITY % dylanClassDef "dylanClassDef"> 
<!ENTITY % dylanOpenClass "dylanOpenClass">
<!ENTITY % dylanPrimaryClass "dylanPrimaryClass">
<!ENTITY % dylanAbstractClass "dylanAbstractClass">
<!ENTITY % dylanSuperClass "dylanSuperClass">
<!ENTITY % dylanInitKeyword "dylanInitKeyword">

<!-- Default included domains entity -->

<!ENTITY included-domains "">

<!-- Default nested topics entity -->

<!ENTITY % dylanClass-info-types "no-topic-nesting">

<!-- Element and attribute definitions -->

<!ELEMENT dylanClass    ((%apiName;), (%shortdesc;), (%prolog;)?,
                           (%dylanClassDetail;)?,
                           (%related-links;)?, (%dylanClass-info-types;)*)>
<!ATTLIST dylanClass    id ID #REQUIRED
                          conref CDATA #IMPLIED
                          outputclass CDATA #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          domains CDATA "&included-domains;">

<!ELEMENT dylanClassDetail ((%dylanClassDef;)?, (%apiDesc;)?, (%section; | %example; | %apiImpl;)*)>
<!ATTLIST dylanClassDetail %id-atts;
                          translate (yes|no) #IMPLIED
                          xml:lang NMTOKEN #IMPLIED
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanClassDef ((%dylanOpenClass;)?, (%dylanPrimaryClass;)?,
                         (%dylanAbstractClass;)?, (%dylanSuperClass;)*,
                         (%dylanInitKeyword;)*)>
<!ATTLIST dylanClassDef   spectitle CDATA #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanOpenClass  EMPTY>
<!ATTLIST dylanOpenClass  name CDATA #FIXED "open"
                          value CDATA #FIXED "open"
                          %univ-atts;
                          outputclass CDATA #IMPLIED>
<!ELEMENT dylanPrimaryClass  EMPTY>
<!ATTLIST dylanPrimaryClass  name CDATA #FIXED "primary"
                          value CDATA #FIXED "primary"
                          %univ-atts;
                          outputclass CDATA #IMPLIED>
<!ELEMENT dylanAbstractClass EMPTY>
<!ATTLIST dylanAbstractClass name CDATA #FIXED "abstract"
                          value (abstract-uninstantiable|abstract-instantiable)
                            "abstract-uninstantiable"
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanSuperClass (#PCDATA)*>
<!ATTLIST dylanSuperClass href CDATA #IMPLIED
                          keyref NMTOKEN #IMPLIED
                          type   CDATA  #IMPLIED
                          %univ-atts;
                          format        CDATA   #IMPLIED
                          scope (local | peer | external) #IMPLIED
                          outputclass CDATA #IMPLIED>

<!ELEMENT dylanInitKeyword ((%apiItemName;), (%apiType;|%apiOtherClassifier;)?,
                            (%apiDefNote;)?)>
<!ATTLIST dylanInitKeyword spectitle CDATA #IMPLIED
                          %univ-atts;
                          outputclass CDATA #IMPLIED>

<!-- Specialization attributes -->

<!ATTLIST dylanClass %global-atts; class CDATA "- topic/topic reference/reference apiRef/apiRef apiClassifier/apiClassifier dylanClass/dylanClass ">
<!ATTLIST dylanClassDetail %global-atts; class CDATA "- topic/body reference/refbody apiRef/apiDetail apiClassifier/apiClassifierDetail dylanClass/dylanClassDetail ">
<!ATTLIST dylanClassDef %global-atts; class  CDATA "- topic/section reference/section apiRef/apiDef apiClassifier/apiClassifierDef dylanClass/dylanClassDef ">
<!ATTLIST dylanOpenClass %global-atts; class CDATA "- topic/state reference/state apiRef/apiQualifier apiClassifier/apiQualifier dylanClass/dylanOpenClass ">
<!ATTLIST dylanPrimaryClass %global-atts; class CDATA "- topic/state reference/state apiRef/apiQualifier apiClassifier/apiQualifier dylanClass/dylanPrimaryClass ">
<!ATTLIST dylanAbstractClass %global-atts; class CDATA "- topic/state reference/state apiRef/apiQualifier apiClassifier/apiQualifier dylanClass/dylanAbstractClass ">
<!ATTLIST dylanSuperClass %global-atts; class  CDATA "- topic/xref reference/xref apiRef/apiRelation apiClassifier/apiBaseClassifier dylanClass/dylanSuperClass ">
<!ATTLIST dylanInitKeyword %global-atts; class CDATA "- topic/ph reference/ph apiRef/apiDefItem apiClassifier/apiClassifierMember dylanClass/dylanInitKeyword ">

module: internal-rep
synopsis: Intermediate elements from quote directives.


define class <xref> (<interm-element>)
   // If the target is <topic>, <footnote>, <ph-marker>, or <section>, the DITA
   // xref tag will have a format attr of "dita," else based on the URL; the 
   // scope attr will be "local," else "external." If the text is <conref>,
   // its style must be #"title" and the target's title is used as the text.
   slot target :: type-union(<topic>, <footnote>, <section>, <ph-marker>, <url>,
                             <target-placeholder>, <footnote-placeholder>,
                             <line-marker-placeholder>),
         init-keyword: #"target";
   slot text :: type-union(<api-name>, <parm-name>, <string>, <conref>),
         init-keyword: #"text";
end class;

/// Synopsis: Represents a vi quotation directive (as opposed to <xref>'s qv
/// directive).
define class <vi-xref> (<xref>)
   inherited slot target /* :: type-union(<topic>, <target-placeholder>) */;
end class;

define class <api-name> (<interm-element>)
   slot text :: <string>;
end class;

define class <parm-name> (<interm-element>)
   slot text :: <string>;
end class;

define class <term> (<interm-element>)
   slot text;
end class;

/// Synopsis: Like <emphasis>, this is a style that will be rendered as bold,
/// italic, whatever. It is associated with a <term>, but is separate to allow
/// for typographical quotes that are appropriately styled but not part of the
/// actual term.
define class <term-style> (<interm-element>)
   slot text;
end class;

define class <code-phrase> (<interm-element>)
   slot text;
end class;

define class <entity> (<interm-element>)
   slot code :: <integer>, init-keyword: #"code";
end class;

define class <cite> (<interm-element>)
   slot text;
end class;

define class <bold> (<interm-element>)
   slot text;
end class;

define class <italic> (<interm-element>)
   slot text;
end class;

define class <underline> (<interm-element>)
   slot text;
end class;

define class <emphasis> (<interm-element>)
   slot text;
end class;

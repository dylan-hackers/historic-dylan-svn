Module: dylan-user
Creator: created from "D:\projects\libraries\tidy-com\type-library.spec" at 13:20 2000-11-12 New Zealand Daylight Time.


define module type-library-module
  use functional-dylan;
  use ole-automation;
  use c-ffi;
  export $TidyObject-class-id, make-TidyObject;
  export <ITidyObject>, ITidyObject/TidyToMem, ITidyObject/TidyToFile, 
        ITidyObject/TidyMemToMem, ITidyObject/TotalWarnings, 
        ITidyObject/TotalErrors, ITidyObject/Warning, ITidyObject/Error, 
        ITidyObject/Comments, ITidyObject/Options;
  export <ITidyOptions>, ITidyOptions/Load, ITidyOptions/Reset, 
        ITidyOptions/Doctype, ITidyOptions/Doctype-setter, 
        ITidyOptions/TidyMark, ITidyOptions/TidyMark-setter, 
        ITidyOptions/HideEndtags, ITidyOptions/HideEndtags-setter, 
        ITidyOptions/EncloseText, ITidyOptions/EncloseText-setter, 
        ITidyOptions/EncloseBlockText, 
        ITidyOptions/EncloseBlockText-setter, ITidyOptions/NewEmptyTags, 
        ITidyOptions/NewEmptyTags-setter, ITidyOptions/NewInlineTags, 
        ITidyOptions/NewInlineTags-setter, ITidyOptions/NewBlocklevelTags, 
        ITidyOptions/NewBlocklevelTags-setter, ITidyOptions/NewPreTags, 
        ITidyOptions/NewPreTags-setter, ITidyOptions/Clean, 
        ITidyOptions/Clean-setter, ITidyOptions/DropFontTags, 
        ITidyOptions/DropFontTags-setter, ITidyOptions/LogicalEmphasis, 
        ITidyOptions/LogicalEmphasis-setter, ITidyOptions/DropEmptyParas, 
        ITidyOptions/DropEmptyParas-setter, ITidyOptions/Word2000, 
        ITidyOptions/Word2000-setter, ITidyOptions/FixBadComments, 
        ITidyOptions/FixBadComments-setter, ITidyOptions/FixBackslash, 
        ITidyOptions/FixBackslash-setter, ITidyOptions/AltText, 
        ITidyOptions/AltText-setter, ITidyOptions/InputXml, 
        ITidyOptions/InputXml-setter, ITidyOptions/OutputXml, 
        ITidyOptions/OutputXml-setter, ITidyOptions/OutputXhtml, 
        ITidyOptions/OutputXhtml-setter, ITidyOptions/AddXmlDecl, 
        ITidyOptions/AddXmlDecl-setter, ITidyOptions/AssumeXmlProcins, 
        ITidyOptions/AssumeXmlProcins-setter, ITidyOptions/AddXmlSpace, 
        ITidyOptions/AddXmlSpace-setter, ITidyOptions/CharEncoding, 
        ITidyOptions/CharEncoding-setter, ITidyOptions/NumericEntities, 
        ITidyOptions/NumericEntities-setter, ITidyOptions/QuoteMarks, 
        ITidyOptions/QuoteMarks-setter, ITidyOptions/QuoteNbsp, 
        ITidyOptions/QuoteNbsp-setter, ITidyOptions/QuoteAmpersand, 
        ITidyOptions/QuoteAmpersand-setter, ITidyOptions/Indent, 
        ITidyOptions/Indent-setter, ITidyOptions/IndentSpaces, 
        ITidyOptions/IndentSpaces-setter, ITidyOptions/Wrap, 
        ITidyOptions/Wrap-setter, ITidyOptions/TabSize, 
        ITidyOptions/TabSize-setter, ITidyOptions/IndentAttributes, 
        ITidyOptions/IndentAttributes-setter, ITidyOptions/WrapAttributes, 
        ITidyOptions/WrapAttributes-setter, 
        ITidyOptions/WrapScriptLiterals, 
        ITidyOptions/WrapScriptLiterals-setter, ITidyOptions/WrapAsp, 
        ITidyOptions/WrapAsp-setter, ITidyOptions/WrapJste, 
        ITidyOptions/WrapJste-setter, ITidyOptions/WrapPhp, 
        ITidyOptions/WrapPhp-setter, ITidyOptions/BreakBeforeBr, 
        ITidyOptions/BreakBeforeBr-setter, ITidyOptions/UppercaseTags, 
        ITidyOptions/UppercaseTags-setter, 
        ITidyOptions/UppercaseAttributes, 
        ITidyOptions/UppercaseAttributes-setter, 
        ITidyOptions/LiteralAttributes, 
        ITidyOptions/LiteralAttributes-setter, ITidyOptions/Markup, 
        ITidyOptions/Markup-setter, ITidyOptions/Quiet, 
        ITidyOptions/Quiet-setter, ITidyOptions/ShowWarnings, 
        ITidyOptions/ShowWarnings-setter, ITidyOptions/Split, 
        ITidyOptions/Split-setter, ITidyOptions/KeepTime, 
        ITidyOptions/KeepTime-setter, ITidyOptions/ErrorFile, 
        ITidyOptions/ErrorFile-setter, ITidyOptions/GnuEmacs, 
        ITidyOptions/GnuEmacs-setter, ITidyOptions/SetPtr;
  export <CharEncoding>, $raw, $ascii, $latin1, $utf8, $iso2022, $macroman;
  export <IndentScheme>, $NoIndent, $IndentBlocks, $AutoIndent;
  export $TidyOptions-class-id, make-TidyOptions;
end module type-library-module;

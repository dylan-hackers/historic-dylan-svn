Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <comment> (<character-data>)
end class <comment>;

define method proxy-class( interface :: <IXMLDOMComment> ) => (class == <comment>)
  <comment>
end proxy-class;

define macro dom-comment-methods-definer
  { define dom-comment-methods ?:name ?:body end }
   => { 
        /* No Methods */
      }
end macro dom-comment-methods-definer;

define dom-node-methods IXMLDOMComment end;
define dom-character-data-methods IXMLDOMComment end;
define dom-comment-methods IXMLDOMComment end;


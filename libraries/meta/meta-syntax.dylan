module: meta-syntax
synopsis: allows adding syntax constructs (belongs in meta lib)
author: Douglas M. Auclair
copyright: (c) 2001, Cotillion Group, Inc.

// all this needs to go into the META library
define macro meta-definer
{ define meta ?:name ( ?meta-vars:* ) => (?results:*) ?meta:* end } 
 => { scan-helper(?name, (?meta-vars), (?results), (?meta)) }
{ define meta ?:name (?meta-vars:*) ?meta:* end }
 => { scan-helper(?name, (?meta-vars), (#t), (?meta)) }
end macro meta-definer;

define macro scan-helper
{ scan-helper(?:name, (?meta-vars:*), (?results:*), (?meta:*)) }
 => { scanner-builder(?name, ( meta-builder(?=string, ?=start, 
                                         (?meta-vars), 
                                         (?results), (?meta)))) }
end macro scan-helper;

define macro scanner-builder
{ scanner-builder(?:name, (?built-meta:*)) }
 => { define function "scan-" ## ?name (?=string, #key ?=start = 0, end: stop)
        ?built-meta
      end; }
end macro scanner-builder;

define macro meta-builder
{ meta-builder(?str:expression, ?start:expression, (?vars:*), (?results:*), (?meta:*)) }
 => {  with-meta-syntax parse-string (?str, start: ?start, pos: ?=index)
         variables(?vars);
         [ ?meta ];
         values(?=index, ?results);
       end with-meta-syntax; }
end macro meta-builder;

define macro collector-definer
{ define collector ?:name (?vars:*) => (?results:*) ?meta:* end }
 => { scanner-builder(?name, 
       (with-collector into-vector ?=str, collect: ?=collect;
          meta-builder(?=string, ?=start, (?vars), (?results), (?meta));
        end with-collector)) }
{ define collector ?:name (?vars:*) ?meta:* end }
 => { scanner-builder(?name, 
       (with-collector into-vector str, collect: ?=collect;
          meta-builder(?=string, ?=start, (?vars), (as(<string>, str)), (?meta));
        end with-collector)) }
end macro collector-definer;

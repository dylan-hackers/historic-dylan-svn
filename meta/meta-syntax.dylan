module: meta-syntax
synopsis: allows adding syntax constructs (belongs in meta lib)
author: Douglas M. Auclair
copyright: (c) 2001, Cotillion Group, Inc.

define variable *debug-meta-functions?* :: <boolean> = #f;

define macro meta-definer
{ define meta ?:name ( ?vars:* ) => (?results:*) ?meta:* end } 
 => { scan-helper(?name, (?vars), (?results), (?meta)) }
{ define meta ?:name (?vars:*) ?meta:* end }
 => { scan-helper(?name, (?vars), (#t), (?meta)) }
end macro meta-definer;

define macro scan-helper
{ scan-helper(?:name, (?vars:*), (?results:*), (?meta:*)) }
 => { scanner-builder(?name, ( meta-builder(?=string, ?=start, 
                                         (?vars), 
                                         (?results), (?meta)))) }
end macro scan-helper;

define function report-scanner-success(name :: <byte-string>, string, start, success)
  if(*debug-meta-functions?*)
    format-out("In meta fn %s, parse ", name);
    if(success)
      format-out("succeeded\n");
    else
      format-out("failed around character %d: \"%s\"\n", start,
                 choose(method(x) 
                            let y = as(<integer>, x);
                            y > 31 & y < 128
                        end, copy-sequence(string, start: start,
                                           end: min(string.size, start + 10))));
    end if;
    force-output(*standard-output*);
  end if;
end report-scanner-success;

define macro scanner-builder
{ scanner-builder(?:name, (?built-meta:*)) }
 => { define function "scan-" ## ?name
          (?=string :: <byte-string>,
           #key ?=start :: <integer> = 0, end: stop :: <integer> = ?=string.size)
        let (result0, #rest results) = ?built-meta;
        report-scanner-success(?"name", ?=string, ?=start, result0);
        apply(values, result0, results);
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

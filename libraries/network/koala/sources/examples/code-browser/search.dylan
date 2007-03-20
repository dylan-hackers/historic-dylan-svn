module: code-browser
Synopsis: Brwose Open Dylan environment objects
Author:   Andreas Bogk, Bastian Mueller, Hannes Mehnert


define thread variable *results* = #f;

define responder search-responder ("/search")
 (request, response)
  let search-string = get-query-value("search");
  let results = element($all-symbols, search-string, default: #());
  dynamic-bind(*results* = results)
    process-template(*result-page*, request, response);
  end;
end;
define page result-page (<code-browser-page>)
  (source: "results.dsp")
end;

define body tag results in code-browser
 (page :: <code-browser-page>, response :: <response>, do-body :: <function>)
 ()
  for (result in *results*)
    dynamic-bind(*project* = result.symbol-entry-project)
      dynamic-bind(*environment-object* = result.symbol-entry-name)
        do-body()
      end;
    end;
  end;
end;


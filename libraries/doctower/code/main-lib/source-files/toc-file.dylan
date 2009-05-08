module: source-files
synopsis: This file parses a table-of-contents file into a set of ordered trees.


define method toc-from-file (locator :: <file-locator>)
=> (toc :: <ordered-tree>)
   verbose-log("Parsing %s", locator);
   with-open-file (file = locator)
      let text = make(<canonical-text-stream>, inner-stream: file);
      block ()
         let toc = make(<ordered-tree>, root: #f);
         let last-key = toc.root-key;
         iterate next-line ()
            let line-num = text.line-col-position;
            let line = read-line (text, on-end-of-stream: #f);
            when (line)
               let loc = make(<file-source-location>, file: file.stream-locator,
                              start-line: line-num, end-line: line-num);
               let (dummy, dummy, dash-start, dash-end, link-start, link-end) =
                     regexp-position(line, "^(-+)\\s+(.*)\\s*$");

               unless (dash-start & dash-end & link-start & link-end)
                  bad-syntax-in-toc-file(location: loc);
               end unless;
         
               let dash-count = dash-end - dash-start;
               let line-key =
                     case
                        dash-count = last-key.key-depth + 1 =>
                           last-key.next-inf-key;
                        dash-count = last-key.key-depth =>
                           last-key.succ-key;
                        dash-count < last-key.key-depth =>
                           for (parent = last-key.sup-key then parent.sup-key,
                                until: parent.key-depth = dash-count)
                           finally
                              parent.sup-key.next-inf-key;
                           end for;
                        otherwise =>
                           skipped-level-in-toc-file(location: loc);
                     end case;
               last-key := line-key;

               let link-name = copy-sequence(line, start: link-start, end: link-end);
               let placeholder = make(<target-placeholder>, link: link-name,
                                      source-location: loc);
               let topic-ref = make(<topic-ref>, target: placeholder,
                                    source-location: loc);
               toc[line-key] := topic-ref;
               next-line();
            end when;
         end iterate;
         toc;
      cleanup
         text.close;
      end block
   end with-open-file
end method;

module: turboblog
author: turbo24prg

define responder service-document-responder ("/service-document")
  if (current-request().request-method = #"get")
    format(current-response().output-stream, as(<string>, with-xml-builder ()
      service (xmlns => "http://purl.org/atom/app#", 
       xmlns :: atom => "http://www.w3.org/2005/Atom") {
        do(do(method (blog)
            collect(generate-workspace(blog))
          end, map-as(<vector>, method (blog) 
              blog 
            end, *blog-server*.blogs)))
      }
    end));
  end if;
end;

define function generate-workspace (blog :: <blog>)
  let workspace = with-xml ()
    workspace {
      title { text(blog.title) },
      collection (href => feed-link(blog, "atom")) {
        title { text("Entries") }
      }
    }
  end;
  //TODO: fix namespace hack
  workspace.node-children[0].xml-name := "atom:title";
  workspace.node-children[1].node-children[0].xml-name := "atom:title";
  workspace;
end;

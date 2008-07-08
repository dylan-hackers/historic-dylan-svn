module: turboblog
author: turbo24prg

define object-tests (file) in turboblog end;

define action-tests
 (add-file, edit-file, remove-file, list-files)
in turboblog end;


define variable *edit-file-page* =
  make(<turboblog-page>, source: "edit-file.dsp");

define variable *view-file-page* =
  make(<turboblog-page>, source: "view-file.dsp");

define variable *remove-file-page* =
  make(<turboblog-page>, source: "remove-file.dsp");


define method respond-to (type == #"post", page :: <edit-file-page>)

// action
  let edit? :: <boolean> = (*action* = #"edit-file");
  let add? :: <boolean> = (*action* = #"add-file");

// form
  let filename = get-query-value("filename");
  let file = get-query-value("file");

// errors
  let errors = #();

// checks
  if (add? & ~ instance?(file, <http-file>))
      errors := add!(errors, #"file");
  end if;

  if (edit? & (~ instance?(filename, <string>) | filename = ""))
    errors := add!(errors, #"filename");
  end if;

  case
    ~ empty?(errors) =>
      dynamic-bind (*errors* = errors, *form* = current-request().request-query-values)
        next-method();
      end;
    add? => begin
        let locator = as(<file-system-locator>,
          concatenate(as(<string>,
            *blog*.directories[#"files"]),
            if (filename & filename ~= "")
              filename
            else
              file.http-file-filename
            end if));
        let file-stream = make(<file-stream>, locator: locator,
          direction: #"output", if-exists: #"replace",
          if-does-not-exist: #"create");
        write(file-stream, file.http-file-content);
        close(file-stream);
        redirect-to(*view-files-page*);
      end;
    edit? => begin
        let locator = merge-locators(as(<file-locator>,
          filename), *file*);
        rename-file(*file*, locator, if-exists: #"replace");
        redirect-to(*view-files-page*);
      end;
  end case;
end;

define method files-responder (blog :: <blog>)
  let request = current-request();
  authenticate();

  let add? :: <boolean> = (get-query-value("add") = #t);
  let edit? :: <boolean> = (get-query-value("edit") = #t);
  let remove? :: <boolean> = (get-query-value("remove") = #t);
  let view? :: <boolean> = (request.request-url-tail ~= "");

  let action :: <symbol> = case
      add? => #"add-file";
      edit? => #"edit-file";
      remove? => #"remove-file";
      view? => #"view-file";
      otherwise => #"list-files";
    end case;

  let file = if (view? | remove? | edit?)
      let locator = as(<file-system-locator>,
        concatenate(as(<string>,
            blog.directories[#"files"]),
          request.request-url-tail));
      file-exists?(locator) & locator;
    end if;

  if ((edit? | view? | remove?) & ~ file)
// TODO:
//    respond-to(#"get", *non-existing-file-page*, request, response);
  else
    with-permission(action, blog: blog, file: file)
      dynamic-bind(*action* = action, *blog* = blog, *file* = file)
        select (request.request-method)
          #"get" => case
              add? | edit? => respond-to(#"get", *edit-file-page*);
              remove? => respond-to(#"get", *remove-file-page*);
              view? => static-file-responder(file);
              otherwise => respond-to(#"get", *view-files-page*);
            end case;
          #"post" => case
              add? | edit? => respond-to(#"post", *edit-file-page*);
              remove? => begin
                  delete-file(file);
                  redirect-to(*view-files-page*);
                end;
            end case;
        end select;
      end;
    end;
  end if;
end;

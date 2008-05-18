module: web-framework

define taglib web-framework () end;

// errors
define thread variable *errors* = #();

// current action
define thread variable *action* = #f;

// sent values
define thread variable *form* = #f;

define url-map
  url "/login"
    action get () => (login);
  url "/logout"
    action get () => (logout);
end;

define tag show-login-url in web-framework (page :: <dylan-server-page>)
 (redirect :: type-union(<string>, <boolean>), current :: <boolean>)
  let url = parse-url("/login");
  if (redirect)
    url.uri-query["redirect"] := if (current) 
        build-uri(request-url(current-request()))
      else 
        redirect 
      end if;
  end if;
  output("%s", url);
end;

define tag show-logout-url in web-framework (page :: <dylan-server-page>)
 (redirect :: type-union(<string>, <boolean>), current :: <boolean>)
  let url = parse-url("/logout");
  if (redirect)
    url.uri-query["redirect"] := if (current) 
        build-uri(request-url(current-request())) 
      else 
        redirect 
      end if;
  end if;
  output("%s", url);
end;


define named-method authenticated? in web-framework (page :: <dylan-server-page>)
  authenticated-user()
end;

// action-test macros

define macro action-tests-definer 
 { define action-tests () in ?taglib:name end }
  => { }
  
 { define action-tests ( ?:name , ?more:* ) in ?taglib:name end }
  => { 
       define action-test ( ?name ) in ?taglib end;
       define action-tests ( ?more ) in ?taglib end
     }

 { define action-tests ( ?:name ) in ?taglib:name end }
  =>  { define action-test ( ?name ) in ?taglib end }
end;

define macro action-test-definer
 { define action-test ( ?:name ) in ?taglib:name end }
  => { 
       define named-method ?name ## "?" in ?taglib
        (page :: <dylan-server-page>)
         *action* = ?#"name"
       end;

       define named-method ?name ## "-permitted?" in ?taglib
        (page :: <dylan-server-page>)
         block ()
           permitted?(?#"name");
           #t;
         exception (condition :: type-union(<authentication-error>, <permission-error>))
           #f;
         end;
       end
     }
end;

// object-test macros

define macro object-tests-definer
 { define object-tests () in ?taglib:name end }
  => { }

 { define object-tests ( ?:name , ?more:* ) in ?taglib:name end }
  => {
       define object-test ( ?name ) in ?taglib end;
       define object-tests ( ?more ) in ?taglib end
     }

 { define object-tests ( ?:name ) in ?taglib:name end }
  =>  { define object-test ( ?name ) in ?taglib end }
end;

define macro object-test-definer
 { define object-test ( ?:name ) in ?taglib:name end }
  => { 
      define thread variable "*" ## ?name ## "*" = #f;

      define named-method ?name ## "?" in ?taglib
       (page :: <dylan-server-page>)
        "*" ## ?name ## "*"
      end 
    }
end;
  
// error-test macros

define macro error-tests-definer
 { define error-tests () in ?taglib:name end }
  => { }

 { define error-tests ( ?:name , ?more:* ) in ?taglib:name end }
  => { 
       define error-test ( ?name ) in ?taglib end;
       define error-tests ( ?more ) in ?taglib end
     }

 { define error-tests ( ?:name ) in ?taglib:name end }
  =>  { define error-test ( ?name ) in ?taglib end }
end;

define macro error-test-definer
 { define error-test (?:name) in ?taglib:name end }
  => { 
       define named-method ?name ## "-error?" in ?taglib
        (page :: <dylan-server-page>)
         member?(?#"name", *errors*)
       end 
     }
end; 

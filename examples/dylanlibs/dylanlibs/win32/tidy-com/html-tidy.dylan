Module:    html-tidy
Synopsis:  Wrapper to TidyCOM component
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define function bstr-to-byte-string(b :: <bstr>) => (result :: <byte-string>)
  block()
    as(<byte-string>, b);
  cleanup
    destroy(b);
  end;
end function bstr-to-byte-string;


define constant $tidy-lock = make(<lock>);

define method html-tidy(s :: <string>) 
 => (result :: <string>, warnings :: false-or(<sequence>), errors :: false-or(<sequence>))
  with-lock($tidy-lock)
    with-ole
      let tidy = #f;
      block()
        tidy := make(<ITidyObject>, class-id: $TidyObject-class-id);
        let result = bstr-to-byte-string(ITidyObject/TidyMemToMem(tidy, s));
        let warnings = 
           begin
             let num-warnings = ITidyObject/TotalWarnings(tidy);
             if(zero?(num-warnings))
               #f
             else
               let array = make(<stretchy-vector>);
               for(index from 0 below num-warnings)
                 array := add!(array, bstr-to-byte-string(ITidyObject/Warning(tidy, index)));
               finally
                 array
               end for;
             end if;
           end begin;
           let errors = 
           begin
             let num-errors = ITidyObject/TotalErrors(tidy);
             if(zero?(num-errors))
               #f
             else
               let array = make(<stretchy-vector>);
               for(index from 0 below num-errors)
                 array := add!(array, bstr-to-byte-string(ITidyObject/Error(tidy, index)));
               finally
                 array
               end for;
             end if;
           end begin;
           values(result, warnings, errors);
         cleanup
           when(tidy)
             IUnknown/Release(tidy);
           end when;
         end;
       end with-ole;
     end with-lock;
end method html-tidy;


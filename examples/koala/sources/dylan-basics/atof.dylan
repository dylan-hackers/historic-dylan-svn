define method atof (string :: <byte-string>, 
		    #key start :: <integer> = 0, 
		    end: finish :: <integer> = string.size)
 => (value :: <double-float>);

   let posn = start;
   let sign = 1;
   let mantissa = as(<extended-integer>, 0);
   let scale = #f;
   let exponent-sign = 1;
   let exponent = 0;

   // Parse the optional sign.
   if (posn < finish)
     let char = string[posn];
     if (char == '-')
       posn := posn + 1;
       sign := -1;
     elseif (char == '+')
       error("bogus float %=: explicit + not allowed according to grammar", string);
       posn := posn + 1;
     end if;
   end if;

   block (return)
     block (parse-exponent)
       // Parse the mantissa.
       while (posn < finish)
         let char = string[posn];
         posn := posn + 1;
         if (char >= '0' & char <= '9')
           let digit = as(<integer>, char) - as(<integer>, '0');
           mantissa := mantissa * 10 + digit;
           if (scale)
             scale := scale + 1;
           end if;
         elseif (char == '.')
           if (scale)
             error("bogus float %=: we already saw a decimal", string);
           end if;
           scale := 0;
         elseif (char == 'e' | char == 'E')
           parse-exponent();
         else
           error("bogus float %=", string);
         end if;
       end while;
       return();
     end block;

     // Parse the exponent.
     if (posn < finish)
       let char = string[posn];
       if (char == '-')
         exponent-sign := -1;
         posn := posn + 1;
       elseif (char == '+')
         error("bogus float %=: explicit + not allowed according to grammar", string);
         posn := posn + 1;
       end if;

       while (posn < finish)
         let char = string[posn];
         posn := posn + 1;
         if (char >= '0' & char <= '9')
           let digit = as(<integer>, char) - as(<integer>, '0');
           exponent := exponent * 10 + digit;
         else
           error("bogus float %=", string);
         end if;
       end while;
     end if;
   end block;

   as(<double-float>,
      sign * mantissa * ratio(10,1)
        ^ (exponent-sign * exponent - (scale | 0)));
end method atof;

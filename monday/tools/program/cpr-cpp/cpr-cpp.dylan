Module: cpr-cpp

define sideways method print-object
    (sr-error :: <parser-automaton-shift/reduce-error>,
     stream :: <stream>)
 => ();
  format(stream, "{shift/reduce error between %= on inputs %=}",
         sr-error.parser-automaton-error-productions,
         sr-error.parser-automaton-error-inputs);
end method;
          
define sideways method print-object
    (sr-error :: <parser-automaton-reduce/reduce-error>,
     stream :: <stream>)
 => ();
  format(stream, "{reduce/reduce error between %= on inputs %=}",
         sr-error.parser-automaton-error-productions,
         sr-error.parser-automaton-error-inputs);
end method;
          
define sideways method print-object
    (production :: <production>, stream :: <stream>)
 => ();
  format(stream, "{<production>: %s =>", production.production-nonterminal);
  for (symbol in production.production-derives)
    format(stream, " %s", symbol);
  end for;
  format(stream, "}");
end method;
          
define sideways method print-message
    (srcloc :: <file-source-location>, stream :: <stream>) => ();
  format(stream, "%s:", srcloc.source-file);
  if (srcloc.source-start-line = srcloc.source-end-line)
    if (srcloc.source-start-column = srcloc.source-end-column)
      format(stream, "%d.%d",
             srcloc.source-start-line, srcloc.source-start-column);
    else
      format(stream, "%d.%d-%d",
             srcloc.source-start-line, srcloc.source-start-column,
             srcloc.source-end-column);
    end if;
  else
    format(stream, "%d.%d-%d.%d",
           srcloc.source-start-line, srcloc.source-start-column,
           srcloc.source-end-line, srcloc.source-end-column);
  end if;
end;

define constant $linux-defines = #[
      // The following definitions are from running
      // gcc -E -o - -dM t.c
      // on an empty t.c file, which results in the set
      // of built-in preprocessor definitions for this
      // platform.
      "__DBL_MIN_EXP__", "(-1021)",
      "__FLT_MIN__", "1.17549435e-38F",
      "__CHAR_BIT__", "8",
      "__WCHAR_MAX__", "2147483647",
      "__DBL_DENORM_MIN__", "4.9406564584124654e-324",
      "__FLT_EVAL_METHOD__", "2",
      "__DBL_MIN_10_EXP__", "(-307)",
      "__FINITE_MATH_ONLY__", "0",
      "__GNUC_PATCHLEVEL__", "3",
      "__SHRT_MAX__", "32767",
      "__LDBL_MAX__", "1.18973149535723176502e+4932L",
      "__UINTMAX_TYPE__", "long long unsigned int",
      "__linux", "1",
      "__unix", "1",
      "__LDBL_MAX_EXP__", "16384",
      "__linux__", "1",
      "__SCHAR_MAX__", "127",
      "__USER_LABEL_PREFIX__", "",
      "__STDC_HOSTED__", "1",
      "__LDBL_HAS_INFINITY__", "1",
      "__DBL_DIG__", "15",
      "__FLT_EPSILON__", "1.19209290e-7F",
      "__tune_i686__", "1",
      "__LDBL_MIN__", "3.36210314311209350626e-4932L",
      "__unix__", "1",
      "__DECIMAL_DIG__", "21",
      "__gnu_linux__", "1",
      "__LDBL_HAS_QUIET_NAN__", "1",
      "__GNUC__", "4",
      "__DBL_MAX__", "1.7976931348623157e+308",
      "__DBL_HAS_INFINITY__", "1",
      "__DBL_MAX_EXP__", "1024",
      "__LONG_LONG_MAX__", "9223372036854775807LL",
      "__GXX_ABI_VERSION", "1002",
      "__FLT_MIN_EXP__", "(-125)",
      "__DBL_MIN__", "2.2250738585072014e-308",
      "__DBL_HAS_QUIET_NAN__", "1",
      "__REGISTER_PREFIX__", "",
      "__NO_INLINE__", "1",
      "__i386", "1",
      "__FLT_MANT_DIG__", "24",
      "__VERSION__", "\"4.0.3 20060304 (prerelease) (Debian 4.0.2-10)\"",
      "i386", "1",
      "__i486__", "1",
      "unix", "1",
      "__i386__", "1",
      "__SIZE_TYPE__", "unsigned int",
      "__ELF__", "1",
      "__FLT_RADIX__", "2",
      "__LDBL_EPSILON__", "1.08420217248550443401e-19L",
      "__FLT_HAS_QUIET_NAN__", "1",
      "__FLT_MAX_10_EXP__", "38",
      "__LONG_MAX__", "2147483647L",
      "__FLT_HAS_INFINITY__", "1",
      "linux", "1",
      "__LDBL_MANT_DIG__", "64",
      "__WCHAR_TYPE__", "int",
      "__FLT_DIG__", "6",
      "__INT_MAX__", "2147483647",
      "__i486", "1",
      "__FLT_MAX_EXP__", "128",
      "__DBL_MANT_DIG__", "53",
      "__WINT_TYPE__", "unsigned int",
      "__LDBL_MIN_EXP__", "(-16381)",
      "__LDBL_MAX_10_EXP__", "4932",
      "__DBL_EPSILON__", "2.2204460492503131e-16",
      "__tune_pentiumpro__", "1",
      "__INTMAX_MAX__", "9223372036854775807LL",
      "__FLT_DENORM_MIN__", "1.40129846e-45F",
      "__FLT_MAX__", "3.40282347e+38F",
      "__FLT_MIN_10_EXP__", "(-37)",
      "__INTMAX_TYPE__", "long long int",
      "__GNUC_MINOR__", "0",
      "__DBL_MAX_10_EXP__", "308",
      "__LDBL_DENORM_MIN__", "3.64519953188247460253e-4951L",
      "__PTRDIFF_TYPE__", "int",
      "__LDBL_MIN_10_EXP__", "(-4931)",
      "__LDBL_DIG__", "18"];
  
define function main(name, arguments)
  let filename = first(arguments);
  let ptu = make(<C-preprocessing-translation-unit-representation>);

  let (path, defines)
    = select ($os-name)
        #"win32" =>
          values(#("c:\\Program Files\\Microsoft Visual Studio 8\\VC\\include"),
                 #("_WIN32"));
        otherwise =>
          values(#("/usr/lib/gcc/i486-linux-gnu/4.0.3/include", 
                   "/usr/include", "/usr/local/include"),
                 #());
      end select;

  for (dir in path)
    let locator = as(<directory-locator>, dir);
    push-last(ptu.preprocessing-header-search-path, locator);
    push-last(ptu.preprocessing-system-header-search-path, locator);
  end for;

  for (identifier in defines)
    preprocessor-define(ptu, identifier, "1");
  end for;

  for (i from 0 below $linux-defines.size by 2)
    preprocessor-define(ptu, $linux-defines[i], $linux-defines[i + 1]);
  end;

  let file = #f;
  let line = #f;
  let position = #f;
  local
    method consumer (data, token-name, token-value,
                     rangemap, start-position, end-position)
     => ();
      let srcloc
        = range-source-location(rangemap, start-position, end-position);
      if (file ~= srcloc.source-file)
        line := srcloc.source-start-line;
        file := srcloc.source-file;
        format-out("\n# %d \"%s\"\n", line, file);
      elseif (line ~= srcloc.source-start-line)
        if (srcloc.source-start-line - line < 10)
          for (i from line below srcloc.source-start-line)
            new-line(*standard-output*);
          finally
            line := i;
          end for;
        else
          line := srcloc.source-start-line;
          format-out("\n# %d\n", line);
        end if;
      elseif (position ~= start-position)
        write-element(*standard-output*, ' ');
      end if;
      write(*standard-output*,
            preprocessor-token-string(token-name, token-value));
      position := end-position + 1;
    end method;

  block ()
    preprocess-C-source-file(ptu, as(<file-locator>, filename), consumer, #f);
  exception (e :: <source-error>)
    format(*standard-error*, "%s: %s\n", source-location(e), e);
    exit-application(1);
  end block;
end;
          
// Invoke our main() function.
main(application-name(), application-arguments());

module: xmpp
synopsis: 
author: 
copyright: 

define class <version> (<object>)
  slot major :: <integer>,
    required-init-keyword: major:;
  slot minor :: <integer>,
    required-init-keyword: minor:;
end class <version>;

define method \< (first :: <version>, second :: <version>)
 => (less-then? :: <boolean>);
  if (first.major < second.major)
    #t;
  elseif (first.major > second.major)
    #f;
  else
    first.minor < second.minor;
  end if;
end method \<;
/*
define method \> (first :: <version>, second :: <version>)
 => (greater-then? :: <boolean>);
  second < first;
end method \>;
*/
define method \= (first :: <version>, second :: <version>)
 => (equal? :: <boolean>);
  if ((first.minor = second.minor) & (first.major = second.major))
    #t;
  else
    #f;   
  end if;
end method \=;

define method as (class == <string>, version :: <version>)
 => (res :: <string>)
  concatenate(integer-to-string(version.major), ".", integer-to-string(version.minor));
end method as;

define method as (class == <version>, version :: <string>)
 => (res :: <version>)
  if (subsequence-position(version, "."))
    let splitted-version = split(version, '.');
    make(<version>, major: string-to-integer(splitted-version[0]), minor: string-to-integer(splitted-version[1]));
  else
    make(<version>, major: string-to-integer(version), minor: 0);
  end if;
end method as;

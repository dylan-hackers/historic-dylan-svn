module: xmpp
synopsis: 
author: 
copyright:

define element x ()
end element x;

define open generic x (element :: <element>) => (res :: false-or(<x>));

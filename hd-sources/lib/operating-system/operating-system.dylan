module:    operating-system
author:    Jonathan Bachrach
copyright: 1996 The Harlequin Group Limited. All rights reserved.

define constant $platform-name
    = as(<symbol>, 
         concatenate(as(<string>, $machine-name), "-", 
                     as(<string>, $os-name)));

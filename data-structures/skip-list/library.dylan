language: infix-dylan
module: Dylan-user

//==============================================================================
//
// Copyright (c) 1996 by Kai W. Zimmermann, Hamburg, Germany
// All rights reserved
//
// This code is provided as is for non-commercial, non-professional and 
// non-military use.  No warranty is taken for any consequences that may
// happen due to the use of this code, be it loss of software, hardware, 
// money, time, friends, etc..  You may use and distribute this code
// freely as long as this notice stays intact.  If you use this code in 
// your projects, please mention me in the documentation.  If you 
// redistribute this software you may not charge a fee for it.   
// 
// If you make changes to the code, port it to another platform, or use it
// in a major project, please let me know.  
// 
//==============================================================================

//==============================================================================
//
// Version 1.0
//
// Author
// 
// KWZ       =    Kai W. Zimmermann, Hamburg, Germany
//                zimmerma@informatik.uni-hamburg.de
// DJV       =    Dustin Voss, Seattle, United States
//                d_j_v@mac.com, |Agent on irc://irc.freenode.net/#dylan
//
// History
//
// 15.03.1996  KWZ  Created in Apple Dylan TR
// 08.03.2009  DJV  Modified for Gwydion Dylan and Open Dylan.
//                  Removed <basic-skip-list> and clear-cache; only
//                  implementors need those things.
//                  Added iteration independent of key order.
//
//==============================================================================


define library skip-list
  use common-dylan,
     import: {dylan, common-extensions, simple-random, transcendentals};
  use io,
     import: {streams, format, print, pprint, format-out};
  export skip-list;
end library;


define module skip-list
  // Copyright (c) 1996  Kai W. Zimmermann
  
  use dylan,
     // Actually redundant.  Only for documentation purpose.
     export: {element, element-setter, size, key-test,
              forward-iteration-protocol, backward-iteration-protocol,
              initialize, as, remove-key!};
  use common-extensions;
  use simple-random,
     import: {random};
  use transcendentals,
     import: {logn};
  use streams,
     import: {<stream>, write, write-element};
  use format,
     import: {format};
  use print,
     import: {print-object},
     export: all;
  use pprint,
     import: {printing-logical-block, pprint-newline, pprint-indent};
  export 
     // Classes
     <skip-list>,
     // Methods
     probability, level, max-level,
     key-order, element-sequence, element-sequence-setter,
     forward-by-key-iteration-protocol;
end module skip-list;

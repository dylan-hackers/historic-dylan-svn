module: mogrifier
synopsis: 
author: gabor@mac.com
copyright: 


define generic compile-function(f :: <function>)
 => compiled :: <function>;


define method compile-function(f :: <function>)
 => compiled :: <function>;
  format-out("Hello, world from mogrifier!\n");
  
  // get hold of the function entry point
  // and follow it
//  heap-object-at(f.object-address);
  follow(10,
         as(<statically-typed-pointer>,
              call-out("GENERAL_ENTRY", ptr:, ptr:, f.object-address)));
  
  f
end method compile-function;


define function follow(instructions :: <integer>, p :: <statically-typed-pointer>)
  for (instruction from 0 below instructions)
    format-out("value is: %d\n", unsigned-long-at(p, offset: instruction * 4));
    format-out("mnemonics is: %s\n", powerpc-disassemble(unsigned-long-at(p, offset: instruction * 4)));
  end for;
end;


define function powerpc-disassemble(instruction :: <integer>)
 => mnemonics :: <string>;
  let primary = ash(instruction, -26);
  let primary = primary >= 0 & primary | primary + #x40;
  powerpc-disassemble-primary(primary, logand(instruction, #x3FFFFFF));
end;


define method powerpc-disassemble-primary(primary :: <integer>, secondary :: <integer>)
 => mnemonics :: <string>;
  format-out("unknown primary: %d\n", primary);
  "???"
end;

define method powerpc-disassemble-primary(primary == 31, secondary :: <integer>)
 => mnemonics :: <string>;
  format-out("key: %d\n", logand(ash(secondary, -1), #x3FF));
 
  powerpc-disassemble-subcode(primary, logand(ash(secondary, -1), #x3FF), secondary)
end;

define method powerpc-disassemble-subcode(primary == 31, subcode == 339, secondary :: <integer>)
 => mnemonics :: <string>;
  format-out("spr: %d\n", logand(ash(secondary, -11), #x3FF));
 
  "mfspr"
end;

define macro instruction-definer
  { define instruction ?:name(?primary:expression; D, A, SIMM) end }
  =>
  { 
    define instruction ?"name"(?primary; D, A, SIMM) end
  }

  { define instruction ?:expression(?primary:expression; D, A, SIMM) end }
  =>
  { 
    define method powerpc-disassemble-primary(primary == ?primary, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("D: %d, A: %d, SIMM: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(secondary, #xFFFF));
      ?expression
    end;
  }

  { define instruction ?:name(?primary:expression; D, A, B; ?key:expression; RESERVE) end }
  =>
  { 
    define method powerpc-disassemble-subcode(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      powerpc-disassemble-DAB(?primary, ?key, secondary)
    end;
    define method powerpc-disassemble-DAB(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("D: %d, A: %d, B: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -11), #x1F));
      ?"name"
    end;
  }

  { define instruction ?:name(?primary:expression; S, A, B; ?key:expression; Rc) end }
  =>
  { define method powerpc-disassemble-subcode(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      powerpc-disassemble-SABRc(?primary, ?key, secondary)
    end;
    define method powerpc-disassemble-SABRc(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("S: %d, A: %d, B: %d, Rc: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -11), #x1F), logand(secondary, #x1));
      ?"name"
    end;
  }

  { define updatable instruction ?:name(?primary:expression; S, A, d) end }
  =>
  {
    define instruction ?name(?primary; S, A, d) end;
    define instruction ?name ## "u"(?primary + 1; S, A, d) end
  }
  { define instruction ?:name(?primary:expression; S, A, d) end }
  =>
  { define method powerpc-disassemble-primary(primary == ?primary, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("S: %d, A: %d, d: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -11), #x1F), logand(secondary, #xFFFF));
      ?"name"
    end;
  }

  { define instruction ?:name(?primary:expression; BO, BI; RESERVE; ?key:expression; LK) end }
  =>
  { define method powerpc-disassemble-primary(subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      powerpc-disassemble-BOBILK(?primary, logand(ash(secondary, -1), #x3FF), secondary)
    end;
    define method powerpc-disassemble-BOBILK(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("BO: %d, BI: %d, LK: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -1), #x3FF));
      concatenate(?"name", secondary.instruction-LK)
    end;
  }

  { define instruction ?:name(?primary:expression; BO, BI, BD, AA, LK) end }
  =>
  { define method powerpc-disassemble-primary(primary == ?primary, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("BO: %d, BI: %d, BD: %d, AA: %d, LK: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -2), #x3FFF), logand(ash(secondary, -1), #x1), logand(secondary, #x1));
      concatenate(?"name", secondary.instruction-LK)
    end;
  }
end macro instruction-definer;


define inline function instruction-LK(secondary :: <integer>)
 => lk :: <string>;
  logand(secondary, #x1) == 1 & "l" | "";
end;

define instruction addis(15; D, A, SIMM) end;
define instruction addi(14; D, A, SIMM) end;
define instruction addic(12; D, A, SIMM) end;
define instruction "addic."(13; D, A, SIMM) end;
define instruction mulli(7; D, A, SIMM) end;
//define instruction (; D, A, SIMM) end;

//define instruction (; D, A, UIMM) end;

define instruction or(31; S, A, B; 444; Rc) end;
define instruction lwarx(31; D, A, B; 20; RESERVE) end;

define instruction bc(16; BO, BI, BD, AA, LK) end;
define instruction bclr(19; BO, BI; RESERVE; 16; LK) end;
define instruction bcctr(19; BO, BI; RESERVE; 528; LK) end;
define instruction stmw(47; S, A, d) end;
define updatable instruction stw(36; S, A, d) end;
define updatable instruction sth(44; S, A, d) end;
define updatable instruction stfs(52; S, A, d) end;
define updatable instruction stfd(54; S, A, d) end;
define updatable instruction stb(38; S, A, d) end;
define updatable instruction lwz(32; S, A, d) end;
define instruction lmw(46; S, A, d) end;
define updatable instruction lhz(40; S, A, d) end;
define updatable instruction lha(42; S, A, d) end;
define updatable instruction lfs(48; S, A, d) end;
define updatable instruction lfd(50; S, A, d) end;
define updatable instruction lbz(34; S, A, d) end;

define function main(name, arguments)
  format-out("Hello, world!\n");
  
  compile-function(map);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());

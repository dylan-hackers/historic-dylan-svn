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
  follow(100,
         as(<statically-typed-pointer>,
              call-out("GENERAL_ENTRY", ptr:, ptr:, f.object-address)));
  
  f
end method compile-function;


define function follow(instructions :: <integer>, p :: <statically-typed-pointer>)
  for (instruction from 0 below instructions)
    format-out("[%d] value is: %d\n", instruction, unsigned-long-at(p, offset: instruction * 4));
    format-out("mnemonics is: %s\n", powerpc-disassemble(unsigned-long-at(p, offset: instruction * 4)));
    force-output(*standard-output*);
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

  { define instruction ?:name(?primary:expression; D, A, B, OE; ?key:expression; Rc) end }
  =>
  { 
    define method powerpc-disassemble-subcode(primary == ?primary, subcode :: one-of(?key, ?key + 0x100), secondary :: <integer>)
     => mnemonics :: <string>;
      powerpc-disassemble-DABOERc(?primary, ?key, secondary)
    end;
    define method powerpc-disassemble-DABOERc(primary == ?primary, subcode :: one-of(?key, ?key + 0x100), secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("D: %d, A: %d, B: %d, OE: %d, Rc: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -11), #x1F), logand(ash(secondary, -10), #x1), secondary.instruction-Rc);
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
      format-out("S: %d, A: %d, B: %d, Rc: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -11), #x1F), secondary.instruction-Rc);
      ?"name"
    end;
  }

  { define instruction ?:name(?primary:expression; S, A, SH; ?key:expression; Rc) end }
  =>
  { define method powerpc-disassemble-subcode(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      powerpc-disassemble-SASHRc(?primary, ?key, secondary)
    end;
    define method powerpc-disassemble-SASHRc(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("S: %d, A: %d, SH: %d, Rc: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -11), #x1F), secondary.instruction-Rc);
      concatenate(?"name", secondary.instruction-Rc-suffix);
    end;
  }
  
  { define instruction ?:name(?primary:expression; S, A, SH, MB, ME, Rc) end }
  =>
  { define method powerpc-disassemble-primary(primary == ?primary, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("S: %d, A: %d, SH: %d, MB: %d, ME: %d, Rc: %d\n", instruction-mask(secondary, 6, 10), instruction-mask(secondary, 11, 15),
                                                                   instruction-mask(secondary, 16, 20), instruction-mask(secondary, 21, 25),
                                                                   instruction-mask(secondary, 26, 30), secondary.instruction-Rc);
      concatenate(?"name", secondary.instruction-Rc-suffix);
    end;
  }
    
  { define instruction ?:name(?primary:expression; S, spr; ?key:expression; RESERVE) end }
  =>
  { define method powerpc-disassemble-subcode(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("spr: %d\n", 32 * instruction-mask(secondary, 16, 20) + instruction-mask(secondary, 11, 15));
      ?"name"
    end;
  }
  
  { define instruction ?:name(?primary:expression; crbD, crbA, crbB; ?key:expression; RESERVE) end }
  =>
  { define method powerpc-disassemble-subcode(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("crbD: %d, crbA: %d, crbB: %d\n", instruction-mask(secondary, 6, 10), instruction-mask(secondary, 11, 15), instruction-mask(secondary, 16, 20));
      ?"name"
    end;
  }
  
  { define instruction ?:name(?primary:expression; crfD; RESERVE; L, A, B; ?key:expression; RESERVE) end }
  =>
  { define method powerpc-disassemble-subcode(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("crfD: %d, L: %d, A: %d, B: %d\n", instruction-mask(secondary, 6, 8), instruction-mask(secondary, 10, 10), instruction-mask(secondary, 11, 15), instruction-mask(secondary, 16, 20));
      ?"name"
    end;
  }
    
  { define instruction ?:name(?primary:expression; crfD; RESERVE; L, A, SIMM) end }
  =>
  { define method powerpc-disassemble-primary(primary == ?primary, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("crfD: %d, L: %d, A: %d, SIMM: %d\n", instruction-mask(secondary, 6, 8), instruction-mask(secondary, 10, 10), instruction-mask(secondary, 11, 15), instruction-mask(secondary, 16, 31));
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
      format-out("BO: %d, BI: %d, LK: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), secondary.instruction-LK);
      concatenate(?"name", secondary.instruction-LK-suffix)
    end;
  }

  { define instruction ?:name(?primary:expression; BO, BI, BD, AA, LK) end }
  =>
  { define method powerpc-disassemble-primary(primary == ?primary, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("BO: %d, BI: %d, BD: %d, AA: %d, LK: %d\n", logand(ash(secondary, -21), #x1F), logand(ash(secondary, -16), #x1F), logand(ash(secondary, -2), #x3FFF), logand(ash(secondary, -1), #x1), secondary.instruction-LK);
      concatenate(?"name", secondary.instruction-LK-suffix)
    end;
  }

  { define instruction ?:name(?primary:expression; LI, AA, LK) end }
  =>
  { define method powerpc-disassemble-primary(primary == ?primary, secondary :: <integer>)
     => mnemonics :: <string>;
      format-out("LI: %d, AA: %d, LK: %d\n", instruction-mask(secondary, 6, 29), instruction-mask(secondary, 30, 30), secondary.instruction-LK);
      concatenate(?"name", secondary.instruction-LK-suffix)
    end;
  }
end macro instruction-definer;


define inline function instruction-LK(secondary :: <integer>)
 => lk :: <integer>;
  logand(secondary, #x1)
end;

define inline function instruction-LK-suffix(secondary :: <integer>)
 => lk :: <string>;
  secondary.instruction-LK == 1
  & "l"
  | ""
end;

define inline function instruction-Rc(secondary :: <integer>)
 => rc :: <integer>;
  logand(secondary, #x1)
end;

define inline function instruction-Rc-suffix(secondary :: <integer>)
 => rc :: <string>;
  secondary.instruction-Rc == 1
  & "."
  | ""
end;

define constant instruction-position = limited(<integer>, min: 0, max: 31);
define inline function instruction-mask(secondary :: <integer>, start :: instruction-position, stop :: instruction-position)
 => masked :: <integer>;
  logand(ash(secondary, 31 - stop), ash(1, stop - start + 1) - 1)
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

define instruction subf(31; D, A, B, OE; 40; Rc) end;
define instruction subfc(31; D, A, B, OE; 8; Rc) end;
define instruction subfe(31; D, A, B, OE; 136; Rc) end;
define instruction subfic(8; D, A, SIMM) end;


define instruction mulld(31; D, A, B, OE; 233; Rc) end;
define instruction mullw(31; D, A, B, OE; 235; Rc) end;
define instruction divd(31; D, A, B, OE; 489; Rc) end;
define instruction divdu(31; D, A, B, OE; 457; Rc) end;
define instruction divw(31; D, A, B, OE; 491; Rc) end;
define instruction divwu(31; D, A, B, OE; 459; Rc) end;
define instruction add(31; D, A, B, OE; 266; Rc) end;
define instruction addc(31; D, A, B, OE; 10; Rc) end;
define instruction adde(31; D, A, B, OE; 138; Rc) end;
/*define instruction (31; D, A, B, OE; RESERVE; Rc) end;
define instruction (31; D, A, B, OE; RESERVE; Rc) end;
define instruction (; D, A, B, OE; ; Rc) end;
define instruction (; D, A, B, OE; ; Rc) end;
define instruction (; D, A, B, OE; ; Rc) end;*/

define instruction bc(16; BO, BI, BD, AA, LK) end;
define instruction b(18; LI, AA, LK) end;
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
define instruction srawi(31; S, A, SH; 824; Rc) end;
define instruction mtspr(31; S, spr; 467; RESERVE) end;
define instruction mfspr(31; S, spr; 339; RESERVE) end;

define instruction cmp(31; crfD; RESERVE; L, A, B; 0; RESERVE) end;
define instruction cmpi(11; crfD; RESERVE; L, A, SIMM) end;
define instruction crand(19; crbD, crbA, crbB; 257; RESERVE) end;
define instruction crandc(19; crbD, crbA, crbB; 129; RESERVE) end;
define instruction creqv(19; crbD, crbA, crbB; 289; RESERVE) end;
define instruction crnand(19; crbD, crbA, crbB; 225; RESERVE) end;
define instruction crnor(19; crbD, crbA, crbB; 33; RESERVE) end;
define instruction cror(19; crbD, crbA, crbB; 449; RESERVE) end;
define instruction crorc(19; crbD, crbA, crbB; 417; RESERVE) end;
define instruction crxor(19; crbD, crbA, crbB; 193; RESERVE) end;
//define instruction (19; crbD, crbA, crbB; ; RESERVE) end;

define instruction rlwinm(21; S, A, SH, MB, ME, Rc) end;


define function main(name, arguments)
  format-out("Hello, world!\n");
  
  compile-function(map);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());

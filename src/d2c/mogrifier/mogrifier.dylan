module: mogrifier
synopsis: customizable machine-code processor/emitter
author: gabor@mac.com
copyright: 


define generic compile-function(f :: <function>)
 => compiled :: <function>;


define method compile-function(f :: <function>)
 => compiled :: <function>;
  format-out("Hello, world from mogrifier!\n");
  
  // get hold of the function entry point
  // and follow it
  follow(1000,
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


define method powerpc-disassemble-primary(primary :: limited(<integer>, min: 0, max: 64), secondary :: <integer>)
 => mnemonics :: <string>;
  format-out("unknown primary: %d, key: %d\n", primary, instruction-mask(secondary, 21, 30));
  "???"
end;


//
// Disassembler generics
//
define generic powerpc-disassemble-primary(primary :: limited(<integer>, min: 0, max: 64), secondary :: <integer>)
 => mnemo :: <string>;

define generic powerpc-disassemble-subcode
  (primary :: limited(<integer>, min: 0, max: 64),
   subcode :: limited(<integer>, min: 0, max: 1023),
   secondary :: <integer>)
 => mnemo :: <string>;



define method powerpc-disassemble-primary(primary :: one-of(19, 31), secondary :: <integer>)
 => mnemonics :: <string>;
  format-out("key: %d\n", instruction-mask(secondary, 21, 30));
 
  powerpc-disassemble-subcode(primary, instruction-mask(secondary, 21, 30), secondary)
end;

define macro instruction-definer
  
  { define updatable instruction ?:name(?primary:expression; S, A, d) end }
  =>
  {
    define instruction ?name(?primary; S, A, d) end;
    define instruction ?name ## "u"(?primary + 1; S, A, d) end
  }

  { define instruction ?:name(?primary:expression; ?key:expression; ?form) end }
  =>
  { define method powerpc-disassemble-subcode(primary == ?primary, subcode == ?key, secondary :: <integer>)
     => mnemonics :: <string>;
      concatenate(?"name", ?form)
    end;
  }

  { define instruction ?:name(?primary:expression; ?rest:*) end }
  =>
  { define instruction ?"name"(?primary; ?rest) end }

  { define instruction ?:expression(?primary:expression; ?form) end }
  =>
  { define method powerpc-disassemble-primary(primary == ?primary, secondary :: <integer>)
     => mnemonics :: <string>;
      concatenate(?expression, ?form)
    end;
  }
  
  form:
    { BO, BI, LK } => { secondary.instruction-field-LK, " ", secondary.instruction-field-BO, ",", secondary.instruction-field-BI }
    { BO, BI, BD, ?absolute-type-branch } => { ?absolute-type-branch, " ", secondary.instruction-field-BO, ",", secondary.instruction-field-BI, ",", secondary.instruction-BD-suffix }
    { LI, ?absolute-type-branch } => { ?absolute-type-branch, " ", secondary.instruction-LI-suffix }
    { S, A, d } => { " ", secondary.instruction-field-S, ",", secondary.instruction-field-SIMM, "(", secondary.instruction-field-A, ")" }
    { crfD, L, A, SIMM } => { " ", secondary.instruction-field-crfD, ",", secondary.instruction-field-L, ",", secondary.instruction-field-A, ",", secondary.instruction-field-SIMM }
    { crfD, L, A, B } => { " ", secondary.instruction-field-crfD, ",", secondary.instruction-field-L, ",", secondary.instruction-field-A, ",", secondary.instruction-field-B }
    { crbD, crbA, crbB } => { " ", secondary.instruction-field-crfD, ",", secondary.instruction-field-crbA, ",", instruction-field-crbB }
    { S, A, B, Rc } => { secondary.instruction-field-Rc, " ", secondary.instruction-field-S, ",", secondary.instruction-field-A, ",", secondary.instruction-field-B }
    { D, A, SIMM } => { " ", secondary.instruction-field-D, ",", secondary.instruction-field-A, ",", secondary.instruction-field-SIMM }
    { D, A, B } => { " ", secondary.instruction-field-D, ",", secondary.instruction-field-A, ",", secondary.instruction-field-B }
    { D, A, B, OE, Rc } => { secondary.instruction-field-OE, secondary.instruction-field-Rc, " ", secondary.instruction-field-D, ",", secondary.instruction-field-A, ",", secondary.instruction-field-B }
    { D } => { " ", secondary.instruction-field-D }
    { S, A, SH, Rc } => { secondary.instruction-field-Rc, " ", secondary.instruction-field-S, ",#########", secondary.instruction-field-A, ",", secondary.instruction-field-SH }
    { S, A, SH, MB, ME, Rc } => { secondary.instruction-field-Rc, " ", secondary.instruction-field-D, ",########", secondary.instruction-field-A, ",", secondary.instruction-field-B }
    { S, spr } => { " ", secondary.instruction-field-spr, ",", secondary.instruction-field-S }
    { D, spr } => { " ", secondary.instruction-field-D, ",", secondary.instruction-field-spr }
  absolute-type-branch:
    { AA, LK } => { secondary.instruction-field-LK, secondary.instruction-field-AA }

end macro instruction-definer;


define macro instruction-field-definer

  { define instruction-field ?:name(?formatter:expression; ?from:expression, ?to:expression) end }
  =>
  {
    define function "instruction-field-" ## ?name(secondary :: <integer>)
     => ?name :: <string>;
      format-to-string(?formatter, instruction-mask(secondary, ?from, ?to))
    end;
  }

  { define instruction-field ?:name(?formatter:expression; ?bit:expression) end }
  =>
  {
    define instruction-field ?name(?formatter; ?bit, ?bit) end
  }

  { define instruction-field ?:name(?all:*) end }
  =>
  {
    define instruction-field ?name("%d"; ?all) end
  }

  { define instruction-field ?:name(?from:expression, ?to:expression) ?:body end }
  =>
  {
    define function "instruction-field-" ## ?name(secondary :: <integer>)
     => ?name :: <string>;
      let ?name = instruction-mask(secondary, ?from, ?to);
      ?body
    end;
  }

  { define instruction-field ?:name(?bit:expression) ?:body end }
  =>
  {
    define instruction-field ?name(?bit, ?bit) ?body end
  }

end macro;


define instruction-field BO(6, 10) end;
define instruction-field BI(11, 15) end;
define instruction-field L(10) end;
define instruction-field S("r%d"; 6, 10) end;
define instruction-field D("r%d"; 6, 10) end;
define instruction-field A("r%d"; 11, 15) end;
define instruction-field B("r%d"; 16, 20) end;
define instruction-field crfD("cr%d"; 6, 8) end;
define instruction-field crfS("cr%d"; 11, 13) end;
define instruction-field crbD(6, 8) end;
define instruction-field crbA(11, 15) end;
define instruction-field crbB(16, 20) end;
define instruction-field SH(16, 20) end;


define inline function instruction-field-spr(secondary :: <integer>)
 => spr :: <string>;
  let spr = 32 * instruction-mask(secondary, 16, 20) + instruction-mask(secondary, 11, 15);
  format-to-string("%d", spr)
end;

define inline function instruction-field-SIMM(secondary :: <integer>)
 => SIMM :: <string>;
  let sign = instruction-mask(secondary, 16, 16);
  let d = instruction-mask(secondary, 17, 31);
  let d = sign == 1 & d - ash(1, 15) | d;
  format-to-string("%d", d)
end;

define inline function instruction-BD-suffix(secondary :: <integer>)
 => bd :: <string>;
  let aa = instruction-mask(secondary, 30, 30);
  let sign = instruction-mask(secondary, 16, 16);
  let bd = instruction-mask(secondary, 17, 29);
  let bd = sign == 1 & bd - ash(1, 13) | bd;
  let bd = 4 * bd;

  if (aa = 1)
    format-to-string("%d", bd)
  else
    format-to-string("$%s%d", bd < 0 & "" | "+", bd)
  end;
end;

define instruction-field AA(30)
  AA == 1
  & "a"
  | ""
end;

define inline function instruction-LI-suffix(secondary :: <integer>)
 => li :: <string>;
  let aa = instruction-mask(secondary, 30, 30);
  let sign = instruction-mask(secondary, 6, 6);
  let li = instruction-mask(secondary, 7, 29);
  let li = sign == 1 & li - ash(1, 23) | li;
  let li = 4 * li;
  if (aa = 1)
    format-to-string("%d", li)
  else
    format-to-string("$%s%d", li < 0 & "" | "+", li)
  end;
end;

define instruction-field LK(31)
  LK == 1
  & "l"
  | ""
end;

define instruction-field Rc(31)
  Rc == 1
  & "."
  | ""
end;

define instruction-field OE(21)
  OE == 1
  & "O"
  | ""
end;


define constant instruction-position = limited(<integer>, min: 0, max: 31);

define inline function instruction-mask(secondary :: <integer>, start :: instruction-position, stop :: instruction-position)
 => masked :: <integer>;
  logand(ash(secondary, stop - 31), ash(1, stop - start + 1) - 1)
end;

define instruction addis(15; D, A, SIMM) end;
define instruction addi(14; D, A, SIMM) end;
define instruction addic(12; D, A, SIMM) end;
define instruction "addic."(13; D, A, SIMM) end;
define instruction mulli(7; D, A, SIMM) end;
//define instruction (; D, A, SIMM) end;

//define instruction (; D, A, UIMM) end;

define instruction or(31; 444; S, A, B, Rc) end;
define instruction xor(31; 316; S, A, B, Rc) end;
define instruction lwarx(31; 20; D, A, B) end;

define instruction subfic(8; D, A, SIMM) end;
define instruction subf(31; 40; D, A, B, OE, Rc) end;
define instruction subfc(31; 8; D, A, B, OE, Rc) end;
define instruction subfe(31; 136; D, A, B, OE, Rc) end;


define instruction mulld(31; 233; D, A, B, OE, Rc) end;
define instruction mullw(31; 235; D, A, B, OE, Rc) end;
define instruction divd(31; 489; D, A, B, OE, Rc) end;
define instruction divdu(31; 457; D, A, B, OE, Rc) end;
define instruction divw(31; 491; D, A, B, OE, Rc) end;
define instruction divwu(31; 459; D, A, B, OE, Rc) end;
define instruction add(31; 266; D, A, B, OE, Rc) end;
define instruction addc(31; 10; D, A, B, OE, Rc) end;
define instruction adde(31; 138; D, A, B, OE, Rc) end;

define instruction mfcr(31; 19; D) end;

define instruction bc(16; BO, BI, BD, AA, LK) end;
define instruction b(18; LI, AA, LK) end;
define instruction bclr(19; 16; BO, BI, LK) end;
define instruction bcctr(19; 528; BO, BI, LK) end;
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
define instruction srawi(31; 824; S, A, SH, Rc) end;
define instruction mtspr(31; 467; S, spr) end;
define instruction mfspr(31; 339; D, spr) end;

define instruction cmp(31; 0; crfD, L, A, B) end;
define instruction cmpi(11; crfD, L, A, SIMM) end;
define instruction crand(19; 257; crbD, crbA, crbB) end;
define instruction crandc(19; 129; crbD, crbA, crbB) end;
define instruction creqv(19; 289; crbD, crbA, crbB) end;
define instruction crnand(19; 225; crbD, crbA, crbB) end;
define instruction crnor(19; 33; crbD, crbA, crbB) end;
define instruction cror(19; 449; crbD, crbA, crbB) end;
define instruction crorc(19; 417; crbD, crbA, crbB) end;
define instruction crxor(19; 193; crbD, crbA, crbB) end;

define instruction rlwinm(21; S, A, SH, MB, ME, Rc) end;


define function main(name, arguments)
  format-out("Hello, world!\n");
  
  compile-function(map);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());

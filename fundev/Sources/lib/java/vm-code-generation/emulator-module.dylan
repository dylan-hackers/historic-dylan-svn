module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module java-vm-code-generation
  use functional-dylan;
  use threads ;
  use streams ;
  use streams-internals ;
  use print ;
  use format ;
  use format-out ;
  use standard-io ;
  use date ;
  use byte-vector ;
  use big-integers ;

  use java-modeling, export: all;

  export

    *debug-jvm-instrs*,
    ensure-stack-model,
    mark-as-implementing,
    // java-opcodes

    <java-abstract-bytecode>, // opname, opcode,
    opname,
    opcode,
    <java-bytecode>,  // pushes, pop-list, push-list,
    <java-call-bytecode>, pops-instance?,
    $java/lang/Throwable$,

    j-nop,
    j-aconst-null,
    j-iconst-m1,
    j-iconst-0,  j-iconst-1,  j-iconst-2,  j-iconst-3,  j-iconst-4,  j-iconst-5,
    j-lconst-0,  j-lconst-1,
    j-fconst-0,  j-fconst-1,  j-fconst-2,
    j-dconst-0,  j-dconst-1,
    j-bipush,
    j-sipush,
    j-ldc1,  j-ldc2,  j-ldc2w,
    j-iconsts,
    j-iload,  j-lload,  j-fload,  j-dload,  j-aload,
    j-iload-0,  j-iload-1,  j-iload-2,  j-iload-3,
    j-lload-0,  j-lload-1,  j-lload-2,  j-lload-3,
    j-fload-0,  j-fload-1,  j-fload-2,  j-fload-3,
    j-dload-0,  j-dload-1,  j-dload-2,  j-dload-3,
    j-aload-0,  j-aload-1,  j-aload-2,  j-aload-3,
    j-iaload,  j-laload,  j-faload,  j-daload,  j-aaload,  j-baload,  j-caload,  j-saload,
    j-acode-for,
    j-istore,  j-lstore,  j-fstore,  j-dstore,  j-astore,
    j-istore-0,  j-istore-1,  j-istore-2,  j-istore-3,
    j-lstore-0,  j-lstore-1,  j-lstore-2,  j-lstore-3,
    j-fstore-0,  j-fstore-1,  j-fstore-2,  j-fstore-3,
    j-dstore-0,  j-dstore-1,  j-dstore-2,  j-dstore-3,
    j-astore-0,  j-astore-1,  j-astore-2,  j-astore-3,
    j-iastore,  j-lastore,  j-fastore,  j-dastore,  j-aastore,  j-bastore,  j-castore,  j-sastore,
    j-pop,  j-pop2,  j-dup,  j-dup-x1,  j-dup-x2,  j-dup2,  j-dup2-x1,  j-dup2-x2,  j-swap,
    j-iadd, j-ladd, j-fadd, j-dadd,
    j-isub, j-lsub, j-fsub, j-dsub,
    j-imul, j-lmul, j-fmul, j-dmul,
    j-idiv, j-ldiv, j-fdiv, j-ddiv,
    j-imod, j-lmod, j-fmod, j-dmod,
    j-ineg, j-lneg, j-fneg, j-dneg,
    j-ishl,  j-lshl,  j-ishr,  j-lshr,  j-iushr,  j-lushr,
    j-iand,  j-land,  j-ior,  j-lor,  j-ixor,  j-lxor,
    j-iinc,
    j-i2l,  j-i2f,  j-i2d,
    j-l2i,  j-l2f,  j-l2d,
    j-f2i,  j-f2l,  j-f2d,
    j-d2i,  j-d2l,  j-d2f,
    j-int2byte,  j-int2char,  j-int2short,
    j-lcmp,
    j-fcmpl,  j-fcmpg,
    j-dcmpl,  j-dcmpg,
    j-ifeq,  j-ifne,  j-iflt,  j-ifge,  j-ifgt,  j-ifle,  
    j-ifnull,  j-ifnonnull,
    j-if-icmpeq,  j-if-icmpne,  j-if-icmplt,  j-if-icmpge,  j-if-icmpgt,  j-if-icmple,
    j-if-acmpeq,  j-if-acmpne,
    j-goto,  j-goto-w,  j-jsr,  j-jsr-w,  j-ret,
    j-tableswitch,  j-lookupswitch,
    j-ireturn,  j-lreturn,  j-freturn,  j-dreturn,  j-areturn,  j-return,
    j-getstatic,  j-putstatic,  j-getfield,  j-putfield,
    j-getstatic2,  j-putstatic2,  j-getfield2,  j-putfield2,
    j-invokevirtual,  j-invokespecial,  j-invokestatic,  j-invokeinterface,
    j-new,  j-newarray,  j-anewarray,
    j-arraylength,
    j-athrow,
    j-checkcast,
    j-instanceof,
    j-monitorenter,  j-monitorexit,
    j-wide,
    j-multianewarray,
    j-software,  j-hardware,

    $one-retaddr$, $one-object$, $one-int$,  // internal?

    // zip-writer

    <zip-entry>,
      filename, filename-setter,
      filetime, filetime-setter,
      filesize, filesize-setter,
      file-crc, file-crc-setter,
      zip-details-upfront?,
    <zip-dir-entry>,
    <zip-crc-stream>, get-offset, write-zip-loc, write-zip-cen, write-zip-end,
    <zip-string-entry>,
      str, str-setter,
    writer,
    zip-timestamp,
    as-zip-date,
//    write-zip-file,
    unix-mode,
    write-zip-entry,
    compute-crc32, update-crc32,
    write16, write32, write-int, write-ascii,
    <jar-file-rep>,
      jar-name,    jar-name-setter,
      jar-library, jar-library-setter,
      jar-stream,  jar-stream-setter,
      zip-stream,  zip-stream-setter,
      open?,       open?-setter,
      jar-comment, jar-comment-setter,
    add-to-jar!,
    jar-close,
    current-jar, current-jar-setter,

    // java-emit-class

    $java-access-public$, $java-access-private$, $java-access-protected$,
    $java-access-static$, $java-access-final$,   $java-access-sync$,
    $java-access-sup$,    $java-access-interface$, $java-access-abstract$,

    <java-slot-spec>,
      java-class, // java-class-setter,
      slot-name,  // slot-name-setter,
      slot-type,  // slot-type-setter,
      static?,  // static?-setter,
      public?, // public?-setter,
    <java-method-spec>,
      invoke-op, // invoke-op-setter,
    total-pushes, total-args,
    slot-spec,
    meth-spec,
    java-method,
    java-interface-method,
    my-break,
    java-field,
    <java-concrete-class-or-interface>,
      constants,    // constants-setter,
      slots,        // slots-setter,
      symbol-slots-list, symbol-slots-list-setter,
      methods,      // methods-setter,
      outstream,    // outstream-setter,
      been-emitted, been-emitted-setter,
      been-inited,  been-inited-setter,
      library,      // library-setter,
      iep-emitted?, iep-emitted?-setter,
      mep-emitted?, mep-emitted?-setter,
      xep-emitted?, xep-emitted?-setter,
      ep-seqnum,    ep-seqnum-setter,
    <java-concrete-class>,
    <java-concrete-interface>,
    same-java-constant,  // probably internal
    pool-index,          // probably internal
    constant-javatype,   // probably internal
    <java-constant>,
    <java-int-constant>,
      value,  // value-setter,
    <java-long-constant>,
    <java-float-constant>,
    <java-double-constant>,
    <java-utf-constant>,
      string,  // string-setter,
    <java-string-constant>,
      utf,     // utf-setter,
      utf-index, utf-index-setter,
    <java-class-constant>,
      java-class,   // java-class-setter
      java-class-index, java-class-index-setter,
    <java-nat-constant>,
      nat-name,    // nat-name-setter,
      nat-type,    // nat-type-setter,
      nat-name-index, nat-name-index-setter,
      nat-type-index, nat-type-index-setter,
      hash-cache, hash-cache-setter,
    <java-slot-constant>,
      java-class,   // java-class-setter,
      nat,     // nat-setter,
      java-class-index, java-class-index-setter,
      nat-index,   nat-index-setter,
      hash-cache,  hash-cache-setter,
    <java-meth-constant>,
    <java-inter-meth-constant>,
    slot-not-already-present,
    java-io-class,
    java-reflect-class,
    $java/lang/Object-array$,
    $java/lang/String$,
    $java/lang/Math$,
    $java/lang/Thread$,
    $java/lang/Exception$,
    $java/lang/RuntimeException$,
    $java/lang/ClassCastException$,
    $java/lang/System$,
//  $dylan/dylanthread$, // JBE
//  predefined-multi-class, // JBE
//  predefined-dylan-class, // JBE
//  predefined-dylan-internal-class, // JBE
//  $dylan-runtime-class$, // JBE
// etc
    java-name-pool-index,
    write2,  // internal?
    write4,  // internal?
    java-emit,
    <java-slot>,
      java-class,   // java-class-setter,
      slot-name, // slot-name-setter,
      slot-type, // slot-type-setter,
      public?, // public?-setter,
      static?, // static?-setter,
      access-code, access-code-setter,
      slot-sig,     // slot-sig-setter,
      slots-spec, // slots-spec-setter,
    <java-abstract-method>,
    <java-code>,
      pc,         pc-setter,
      max-stack,  max-stack-setter,
      max-locals, max-locals-setter,
    <java-method>,
      excep-table, excep-table-setter,
      basic-blocks,
      bb-list,      bb-list-setter,
      finally-handlers,
      synchronized?, // synchronized?-setter,
      label-table,   // label-table-setter,
    <java-field>,
    <java-exception-entry>,
      start-pc,  // start-pc-setter,
      end-pc,    // end-pc-setter,
      excep-type,      // excep-type-setter,
      excep-pc, // excep-pc-setter,
    *max-max-stack*,
    *max-max-locals*,
    $SourceFile-name$,
    $dummy-file-name$,
    filename-for-class,
    $Code-attr-name$,
    <class-file-jar-entry>,
      java-class,   // java-class--setter,
    <class-string-jar-entry>,
      zstream, // zstream-setter,
    *the-pending-java-classes*,
    java-emit-class,
    java-unemit-class,

    // java-emit-code

    <java-label>,
      pc, pc-setter,
    *break-on-non-verify*,
    <java-basic-block>,
      meth,         // meth-setter,
      bytecodes,    // bytecodes-setter,
      the-label,    // the-label-setter,
      stack-depth,  stack-depth-setter,
      stack-model,  stack-model-setter,
      local-var-types, local-var-types-setter,
      initial-stack-depth, initial-stack-depth-setter,
      initial-stack-model, initial-stack-model-setter,
      initial-local-var-types, initial-local-var-types-setter,
      constants, // constants-setter,
    make-jbb,
    <java-abstract-frag>,
    <java-imm-frag>,
      imm-value,  // imm-value-setter,
    <java-frag>,
      opcode,     // opcode-setter,
    $dummy-java-frag$,
    output-frag,
    <java-op1-frag>,
      op,  op-setter,
    <java-op2-frag>,
    <java-op22-frag>,
      nargs, nargs-setter,
    <java-branch-frag>,
      meth, meth-setter,
      dest, dest-setter,
    branch-relative,
    resolve-branch-dest,
    add-bytecode,
    maintain-stack-depth,
    model-pop-discards,
    model-push-type,
    model-pop-a-type,
    model-set-a-local,
    maintain-stack-types,
    merge-stack-types,     // internal?
    merge-local-var-types, // internal?
    merge-bbs-types,       // internal?
    finish-with-jbb,
    *check-stack-types*,

    java-simple-op,
    java-marker-op,
    java-op1-op,
    make-java-constant,   // internal?
    java-op2-op,
    java-op2,
    java-call,
    java-if-call,
    java-read,
    java-write,
    java-branch-op,
    output-bytecodes,  // internal?
    emit-ret,
    emit-push-local,
    emit-push-this,
    emit-pop-local,
    emit-pop,
    emit-dup,
    emit-swap,
    emit-return,
    emit-java-ldc,
    emit-java-int,
    emit-java-string,
    emit-java-constant-load ;

end module;

// eof

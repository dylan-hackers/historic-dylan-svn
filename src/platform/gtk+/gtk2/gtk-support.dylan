module: gtk-support

c-include("gtk/gtk.h");
/*
define constant generic-dylan-marshaller = callback-method
    (closure :: <GClosure>, return-value :: <GValue>,
     n_param_values :: <guint>, param_values :: <GValue>,
     invocation_hint :: <gpointer>, marshal_data :: <gpointer>) => ();
  format-out("");
end;
*/
    
define constant generic-dylan-marshaller = 
    callback-method(stub-closure         :: <raw-pointer>,
                    stub-return-value    :: <raw-pointer>,
                    stub-n-param-values  :: <integer>,
                    stub-param-values    :: <raw-pointer>,
                    stub-invocation-hint :: <raw-pointer>,
                    stub-marshal-data    :: <raw-pointer>) => ();
     let gvalues = make(<GValue*>, pointer: stub-param-values);
      let values = #();
      for(i from 0 below stub-n-param-values)
        values := pair(g-value-to-dylan(pointer-value(gvalues, index: i)),
                       values);
      end for;
      values := reverse!(values);
      apply(import-value(<object>, 
                         make(<gpointer>, pointer: stub-marshal-data)),
            values);
    end;

define function g-signal-connect(instance :: <GObject>, 
                                  signal :: <byte-string>,
                                  function :: <function>,
                                  #key run-after? :: <boolean>)
  let closure = g-closure-new-simple(c-expr(int:, "sizeof(GClosure)"),
                                     #f);
  g-closure-set-meta-marshal
    (closure, function, 
     make(<GClosureMarshal>, 
          pointer: generic-dylan-marshaller.callback-entry));
  g-signal-connect-closure(instance, 
                           signal, 
                           closure, 
                           if(run-after?) 1 else 0 end)
end function g-signal-connect;

/*
define function g-signal-connect(instance, detailed-signal, c-handler, data)
  g-signal-connect-data (instance, detailed-signal, 
                         c-handler, data,
                         as(<GClosureNotify>, $null-pointer), 0)
end function g-signal-connect;
*/

define function g-signal-connect-swapped
    (instance, detailed-signal, c-handler, data)
  g-signal-connect-data (instance, detailed-signal, 
                         c-handler, data,
                         as(<GClosureNotify>, $null-pointer), 
                         $G-CONNECT-SWAPPED)
end function g-signal-connect-swapped;

define method export-value(cls == <GCallback>, value :: <function>) => (result :: <function-pointer>);
  make(<function-pointer>, pointer: value.callback-entry); 
end method export-value;

define method import-value(cls == <function>, value :: <GCallback>) => (result :: <function>);
  error("Is this possible?");
end method import-value;

define sealed method export-value(cls == <gpointer>, the-value :: <object>) 
 => (result :: <gpointer>);
  make(<gpointer>, 
       pointer: object-address(make(<value-cell>, value: the-value)));
end method export-value;

define sealed method import-value(cls == <object>, the-value :: <gpointer>) 
 => (result :: <object>);
  value(heap-object-at(the-value.raw-value));
end method import-value;

define sealed domain make (singleton(<gpointer>));

define function all-subclasses(x :: <class>)
  => (subclasses :: <collection>)
  apply(concatenate, x.direct-subclasses, 
        map(all-subclasses, x.direct-subclasses))
end;

define constant $all-gtype-instances = all-subclasses(<GTypeInstance>);

define function find-gtype-by-name(name :: <byte-string>)
  block(return)
    for(i in $all-gtype-instances)
      if(i.class-name = concatenate("<_", name, ">"))
        return(i)
      end if;
    finally
      error("Unknown GType %= encountered.", name)
    end for;
  end block;
end function find-gtype-by-name;

define function find-gtype(g-type :: <GType>)
 => (type :: <class>);
  let dylan-type = element($gtype-table, g-type, default: #f);
  unless(dylan-type)
    let type-name = g-type-name(g-type);
    dylan-type := find-gtype-by-name(type-name);
    $gtype-table[g-type] := dylan-type;
  end unless;
  dylan-type
end function find-gtype;
  
// map GTK type IDs to Dylan classes
define table $gtype-table = {
                             $G-TYPE-CHAR    => <gchar>,
                             $G-TYPE-UCHAR   => <guchar>,
                             $G-TYPE-INT     => <gint>,
                             $G-TYPE-UINT    => <guint>,
                             $G-TYPE-LONG    => <glong>,
                             $G-TYPE-ULONG   => <gulong>,
                             $G-TYPE-INT64   => <gint64>,
                             $G-TYPE-UINT64  => <guint64>,
                             $G-TYPE-FLOAT   => <gfloat>,
                             $G-TYPE-DOUBLE  => <gdouble>,
                             $G-TYPE-STRING  => <gstring>,
                             $G-TYPE-POINTER => <gpointer>,
                             };

define method make(type :: subclass(<GTypeInstance>), #rest args, 
                   #key pointer, #all-keys)
 => (result :: <GTypeInstance>)
  if(pointer)
    let instance = next-method(<GTypeInstance>, pointer: pointer);
    let g-type = g-type-from-instance(instance);
    let dylan-type = find-gtype(g-type);
    next-method(dylan-type, pointer: pointer);
  else
    next-method();
  end if;
end method make;

define function g-type-from-instance(instance :: <GTypeInstance>)
 => (type :: <GType>);
  c-decl("GType g_type_from_instance(gpointer instance) { return G_TYPE_FROM_INSTANCE(instance); }");
  call-out("g_type_from_instance", int:, ptr: instance.raw-value);
end function g-type-from-instance;

define function g-value-type(instance :: <GValue>)
 => (type :: <GType>);
  c-decl("GType g_value_type(gpointer instance) { return G_VALUE_TYPE(instance); }");
  call-out("g_value_type", int:, ptr: instance.raw-value);
end function g-value-type;

define functional class <GValue*> (<GValue>, <c-vector>) end;

define inline method pointer-value(ptr :: <GValue*>, #key index = 0)
 => (result :: <GValue>);
  pointer-at(ptr, offset: index * c-expr(int:, "sizeof(GValue)"), class: <GValue>);
end method pointer-value;
    

define function g-value-to-dylan(instance :: <GValue>)
 => (dylan-instance);
  let g-type = g-value-type(instance);
  if(g-type ~= $G-TYPE-INVALID)
    let dylan-type = find-gtype(g-type);
    if(subtype?(dylan-type, <GObject>))
      make(dylan-type, pointer: instance.g-value-peek-pointer.raw-value)
    else
      signal("Can't handle fundamental types yet.");
    end if;
  end if;
end function g-value-to-dylan;

// Another stupid workaround. Sometimes we need to access mapped types
// as pointers, and Melange doesn't provide any way to do so. Or does it?
define sealed functional class <c-pointer-vector> (<c-vector>) end;

define sealed domain make (singleton(<c-pointer-vector>));

define constant $pointer-size = 4;

define sealed method pointer-value
    (ptr :: <c-pointer-vector>, #key index = 0)
 => (result :: <statically-typed-pointer>);
  pointer-at(ptr,
	     offset: index * $pointer-size,
	     class: <statically-typed-pointer>);
end method pointer-value;

define sealed method pointer-value-setter
    (value :: <statically-typed-pointer>,
     ptr :: <c-pointer-vector>, #key index = 0)
 => (result :: <statically-typed-pointer>);
  pointer-at(ptr,
	     offset: index * $pointer-size,
	     class: <statically-typed-pointer>) := value;
  value;
end method pointer-value-setter;

define sealed method content-size (value :: subclass(<c-pointer-vector>))
 => (result :: <integer>);
  $pointer-size;
end method content-size;

define function gtk-init(progname, arguments)
  let (argc, argv) = c-arguments(progname, arguments);
  %gtk-init(argc, argv);
end function gtk-init;

define method c-arguments(progname :: <string>, arguments)
 => (<int*>, <char***>)
  let argc = 1 + arguments.size;
  let argv :: <c-string-vector> =
    make(<c-string-vector>, element-count: argc + 1);
  // XXX - We'd need to delete these if we weren't using
  // a garbage collector which handles the C heap.
  argv[0] := progname;
  for (i from 1 below argc,
       arg in arguments)
    argv[i] := arg;
  end for;
  as(<c-pointer-vector>, argv)[argc] := null-pointer;
  let pargc = make(<int*>);
  pointer-value(pargc) := argc;
  let pargv = make(<char***>);
  pointer-value(pargv) := argv;
  values(pargc, pargv);
end method c-arguments;

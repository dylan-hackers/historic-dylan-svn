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

/*
define functional class <GValue*> (<GValue>) end;

define sealed domain make (singleton(<GValue*>));

*/
define inline method pointer-value
    (ptr :: <GValue>, #key index = 0)
 => (result :: <GValue>);
  make(<GValue>, pointer: ptr.raw-value + index * c-expr(int: "sizeof(GValue)"));
end method pointer-value;

define constant generic-dylan-marshaller = 
    callback-method(stub-closure         :: <raw-pointer>,
                    stub-return-value    :: <raw-pointer>,
                    stub-n-param-values  :: <integer>,
                    stub-param-values    :: <raw-pointer>,
                    stub-invocation-hint :: <raw-pointer>,
                    stub-marshal-data    :: <raw-pointer>) => ();
      let gvalues = make(<GValue>, pointer: stub-param-values);
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

define function g-signal-connect-swapped
    (instance, detailed-signal, c-handler, data)
  g-signal-connect-data (instance, detailed-signal, 
                         c-handler, data,
                         as(<GClosureNotify>, $null-pointer), 
                         $G-CONNECT-SWAPPED)
end function g-signal-connect-swapped;

define sealed domain make (singleton(<gpointer>));

define function all-subclasses(x :: <class>)
  => (subclasses :: <collection>)
  apply(concatenate, x.direct-subclasses, 
        map(all-subclasses, x.direct-subclasses))
end;

// We cheat!
// define constant $all-gtype-instances = all-subclasses(<GTypeInstance>);
define constant $all-gtype-instances = all-subclasses(<statically-typed-pointer>);

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

define method find-gtype(g-type :: <GType>)
 => (type :: <class>);
  let dylan-type = element($gtype-table, g-type, default: #f);
  unless(dylan-type)
    let type-name = g-type-name(g-type);
    dylan-type := find-gtype-by-name(type-name);
    $gtype-table[g-type] := dylan-type;
  end unless;
  dylan-type
end method find-gtype;
  
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

define function g-value-to-dylan(instance :: <GValue>)
 => (dylan-instance);
  let g-type = g-value-type(instance);
  if(g-type ~= $G-TYPE-INVALID)
    let dylan-type = find-gtype(g-type);
    if(subtype?(dylan-type, <statically-typed-pointer>))
      make(dylan-type, pointer: instance.g-value-peek-pointer.raw-value)
    else
      signal("Can't handle fundamental types yet.");
    end if;
  end if;
end function g-value-to-dylan;


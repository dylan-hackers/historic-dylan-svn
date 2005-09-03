module: buddha

define method browse (stream :: <stream>, object :: <object>,
                      obj-string :: false-or(<string>))
  
  let class = object.object-class;
  for (slot in class.slot-descriptors)
    if (slot-initialized?(object, slot))
      let name = slot.slot-getter.debug-name;
      let link = if (obj-string)
                   concatenate(obj-string, "/", name);
                 else
                   name;
                 end;
      let type = slot.slot-type;
      if (type = <list>)
        for (ele in slot.slot-getter(object),
             i from 0)
          format(stream, "<li><a href=/browse?obj=%s[%d]>%s %d</a></li>\n",
                 link, i, escape-html(name), i);
        end;
      elseif (type = <table>)
        for (ele in slot.slot-getter(object),
             i in slot.slot-getter(object).key-sequence)
          format(stream, "<li><a href=/browse?obj=%s[%d]>%s %d</a></li>\n",
                 link, i, escape-html(name), i);
        end;
      elseif (type = <string>)
        format(stream, "<li>%s: %s</li>\n",
               escape-html(name), escape-html(slot.slot-getter(object)));
      elseif (type = <integer>)
        format(stream, "<li>%s: %d</li>\n",
               escape-html(name), slot.slot-getter(object));
      elseif (type = <boolean>)  
        format(stream, "<li>%s: %=</li>\n",
               escape-html(name), slot.slot-getter(object));
      else
        let string = format-to-string("%= %=", name, slot.slot-getter(object));
        format(stream, "<li><a href=/browse?obj=%s>%s</a></li>\n",
               link, escape-html(string));
      end;
      
    end;
  end;
end;

define method find-slot (slot-name :: <string>, object :: <object>)
  => (slot)
  let class = object.object-class;
  let number = #f;
  if (any?(method(x) x = '[' end, slot-name))
    let (all, real-slot-name, element-number)
      = regexp-match(slot-name, "(.*)\\[(.*)\\]");
    slot-name := real-slot-name;
    number := string-to-integer(element-number);
  end;
  block (found)
    for (slot in class.slot-descriptors)
      if (slot.slot-getter.debug-name = slot-name)
        if (number)
          found(slot.slot-getter(object)[number]);
        else
          found(slot.slot-getter(object));
        end;
      end;
    end;
  end;
end;
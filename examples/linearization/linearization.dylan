module: linearization
synopsis: experiment with the Linearization algorithm in d2c
author: Bruce Hoult

// this is the example motivating C3 over the Dylan linearization in
// http://www.webcom.com/haahr/dylan/linearization-oopsla96.html
//
// Dylan and CLOS linearizations are both:
//   <editable-scrollable-pane>, <scrollable-pane>, <editable-pane>, <pane>,
//      <editing-mixin>, <scrolling-mixin>, <object>
//
// C3 linearization is:
//   <editable-scrollable-pane>, <scrollable-pane>, <editable-pane>, <pane>,
//      <scrolling-mixin>, <editing-mixin>, <object>

define class <pane> (<object>) end;
define class <scrolling-mixin> (<object>) end;
define class <editing-mixin> (<object>) end;
define class <scrollable-pane> (<pane>, <scrolling-mixin>) end;
define class <editable-pane> (<pane>, <editing-mixin>) end;
define class <editable-scrollable-pane> (<scrollable-pane>, <editable-pane>) end;

begin
  let dylan-cpl = vector(<editable-scrollable-pane>,
                         <scrollable-pane>, <editable-pane>, <pane>,
                         <editing-mixin>, <scrolling-mixin>,
                         <object>);

  let C3-cpl = vector(<editable-scrollable-pane>,
                      <scrollable-pane>, <editable-pane>, <pane>,
                      <scrolling-mixin>, <editing-mixin>,
                      <object>);

  let cpl = <editable-scrollable-pane>.all-superclasses;

  local
    method check(other, name)
      if (cpl = other)
        format-out("The linearization in this compiler seems to be %s\n", name);
      end;
    end;
 
  check(dylan-cpl, "Dylan");
  check(C3-cpl, "C3");

  format-out("The CPL is: %=\n", cpl);
end;

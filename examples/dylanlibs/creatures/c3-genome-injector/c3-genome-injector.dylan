Module:    c3-genome-injector
Synopsis:  Genome Injector for Creatures 3
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define constant $gender-values = #[ #["Random", 0], #["Male", 1], #["Female", 2] ];

define frame <genome-injector-frame> (<simple-frame>)
  pane moniker-pane (frame)
    make(<text-field>);

  pane gender-pane (frame)
    make(<option-box>, 
      items: $gender-values,
      value-key: second,
      label-key: first);

  pane inject-button (frame)
    make(<push-button>, label: "Inject", activate-callback: on-inject-button);

  pane about-button (frame)
    make(<push-button>, label: "About", activate-callback: on-about-button);

  layout (frame)
    vertically()
      make(<table-layout>,
        columns: 2,
        children: vector(make(<label>, label: "Moniker:"), 
                         frame.moniker-pane,
                         make(<label>, label: "Gender:"), 
                         frame.gender-pane));
      horizontally() frame.inject-button; frame.about-button; end;
    end;

  keyword title: = "C3 Genome Injector";
  keyword width: = 200;
end frame;

define constant $caos-code =
  "inst "
  "new: simp 3 4 1 \"eggs\" 8 0 4 "
  "puhl 0 15 45 "
  "puhl 1 15 35 "
  "puhl 2 15 25 "
  "puhl 3 15 10 "
  "puhl 4 15 10 "
  "puhl 5 15 10 "
  "emit 11 0.65 "
  "gene load targ 1 \"%s\" "
  "setv ov01 %d "
  "elas 10 "
  "fric 100 "
  "attr 195 "
  "bhvr 32 "
  "aero 1 "
  "accg 4 "
  "perm 60 "
  "mvto 580 920 "
  "tick 900 "
  "setv ov61 100";

define method on-inject-button( g )
  let frame = g.sheet-frame;
  block()
    with-busy-cursor( frame )    
      let result = 
        c3-caos(format-to-string($caos-code, 
          frame.moniker-pane.gadget-value,
          frame.gender-pane.gadget-value));
      when(result.size > 0)
        notify-user("Could not inject genome. Check to see if moniker is correct.");
      end when;
    end;
  exception(e :: <condition>)
    notify-user("Could not communicate with the creatures engine.\n"
                "Please make sure that Creatures 3 is running and try again.");
  end;
end method;

define method on-about-button( g )
  notify-user(concatenate("Version 1.1\nCopyright (c) 1999, Chris Double.\n"
              "All Rights Reserved.\n"
              "Using c3-engine.dll V", c3-engine-version(), "\n",                          
              "http://www.double.co.nz/creatures"),
              title: "Creatures 3 Genome Injector");
end method;

define method main () => ()
  start-frame(make(<genome-injector-frame>));
end method main;

begin
  main();
end;

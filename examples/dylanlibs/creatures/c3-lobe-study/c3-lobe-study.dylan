Module:    c3-lobe-study
Synopsis:  Lobe Study program for Creatures 3
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define constant $lobes = #[
    #["Drive", 0],
    #["Decision", 1],
    #["Attention", 2],
    #["Vision", 3],
    #["Movement", 4],
//    #["Combination", 5],
    #["Stimuli", 6],
    #["Noun", 7],
    #["Verb", 8],
    #["Detail", 9],
    #["Situation", 10],
    #["Response", 11],
    #["Friend or Foe", 12],
    #["Mood", 13],
    #["Smell", 14]];

define method get-cell-description( lobe, cell )
  "Unknown"
end;

define method get-cell-description( lobe :: one-of(0, 11), cell )
  select(cell)
    0 => "Pain";
    1 => "Hunger for protein";
    2 => "Hunger for carbohydrate";
    3 => "Hunger for fat";
    4 => "Coldness";
    5 => "Hotness";
    6 => "Tiredness";
    7 => "Sleepiness";
    8 => "Loneliness";
    9 => "Crowded";
    10 => "Fear";
    11 => "Boredom";
    12 => "Anger";
    13 => "Sex drive";
    14 => "Comfort";
    15 => "Up";
    16 => "Down";
    17 => "Exit";
    18 => "Enter";
    19 => "Wait";
    otherwise => next-method();
  end;
end;

define method get-cell-description( lobe :: one-of(1, 8), cell )
  select(cell)
    0 => "Quiescent";
    1 => "Push";
    2 => "Pull";
    3 => "Stop";
    4 => "Come";
    5 => "Run";
    6 => "Get";
    7 => "Drop";
    8 => "Say need";
    9 => "Rest";
    10 => "Left";
    11 => "Eat";
    12 => "Hit";
    otherwise => next-method();
  end;
end;

define method get-cell-description( lobe :: one-of(2, 3, 4, 6, 7), cell )
    select(cell)
      0  => "Myself - norn";
      1  => "Hand";
      2  => "Door";
      3  => "Seed";
      4  => "Plant";
      5  => "Weed";
      6  => "Leaf";
      7  => "Flower";
      8  => "Fruit";
      9  => "Manky";
      10 => "Detritus";
      11 => "Food";
      12 => "Button";
      13 => "Bug";
      14 => "Pest";
      15 => "Critter";
      16 => "Beast";
      17 => "Nest";
      18 => "Animal Egg";
      19 => "Weather";
      20 => "Bad";
      21 => "Toy";
      22 => "Incubator";
      23 => "Dispenser";
      24 => "Tool";
      25 => "Potion";
      26 => "Elevator";
      27 => "Teleporter";
      28 => "Machinery";
      29 => "Creature egg";
      30 => "Norn home";
      31 => "Grendel home";
      32 => "Ettin home";
      33 => "Gadget";
      34 => "Something34";
      35 => "Vehicle";
      36 => "Norn";
      37 => "Grendel";
      38 => "Ettin";
      39 => "Something39";
      otherwise => next-method();
    end;
end;

define method get-cell-description( lobe == 9, cell )
  select(cell)
    0 => "It is being carried by me";
    1 => "It is being carried by someone else";
    2 => "It is this close to me";
    3 => "It is a creature";
    4 => "It is my sibling";
    5 => "It is my parent";
    6 => "It is my child";
    7 => "It is opposite sex and my genus";
    8 => "It is of this size";
    9 => "It is smelling this much";
    10 => "It is stopped";
    otherwise => next-method();
  end;
end;

define method get-cell-description( lobe == 10, cell )
  select(cell)
    0 => "I am this old";
    1 => "I am inside a vehicle";
    2 => "I am carrying something";
    3 => "I am being carried";
    4 => "I am falling";
    5 => "I am near a creature of opposite sex and my genus";
    6 => "I am at this mood musically";
    7 => "I am at this threat level musically";
    8 => "I am the selected norn";
    otherwise => next-method();
  end;
end;

define frame <lobe-study-frame> (<simple-frame>)
  slot frame-connection = #f;
  slot frame-abort-thread? = #f;

  pane lobe-list (frame)
    make(<option-box>, items: $lobes, value: 2, label-key: first, value-key: second);

  pane about-button (frame)
    make(<push-button>, label: "About", activate-callback: on-about);

  pane connect-button (frame)
    make(<push-button>, label: "Connect", activate-callback: on-button-connect);

  pane pause-button (frame)
    make(<push-button>, label: "Pause", activate-callback: 
      method(g)
        when(frame.frame-connection)
          execute-caos(frame.frame-connection, "dbg: paws")
        end when
      end method);

  pane step-button (frame)
    make(<push-button>, label: "Single Tick", activate-callback: 
      method(g)
        when(frame.frame-connection)
          execute-caos(frame.frame-connection, "dbg: tock")
        end when
      end method);

  pane resume-button (frame)
    make(<push-button>, label: "Resume", activate-callback: 
      method(g)
        when(frame.frame-connection)
          execute-caos(frame.frame-connection, "dbg: play")
        end when
      end method);

  pane disconnect-button (frame)
    make(<push-button>, label: "Disconnect", activate-callback: on-button-disconnect);

  pane cell-list (frame)
    make(<table-control>,      
      enabled?: #t,
      headings: #("Cell", "Description", "Input", "State 0", "State 1", "State 2", "State 3", "State 4", "State 5",
        "State 6", "State 7"),
      widths: #(60, 120, 60, 60, 60, 60, 60, 60, 60, 60, 60),
      generators: vector( neuron-number, neuron-description, neuron-input, neuron-state-0, neuron-state-1, neuron-state-2,
        neuron-state-3, neuron-state-4, neuron-state-5, neuron-state-6, neuron-state-7));

  layout (frame)
    horizontally( spacing: 2)
      vertically()
        frame.lobe-list;
        frame.cell-list;
      end;
      vertically( spacing: 2)
        frame.connect-button;
        frame.pause-button;
        frame.step-button;
        frame.resume-button;
        frame.disconnect-button;
        frame.about-button;
      end;
    end;

  keyword title: = "Creatures 3 Lobe Study";
  keyword width: = 400;
  keyword height: = 250;
end frame;

define method handle-event( frame :: <lobe-study-frame>, event :: <frame-unmapped-event> ) => ()
  when(frame.frame-connection)
    disconnect(frame.frame-connection);
    frame.frame-connection := #f;
    frame.frame-abort-thread? := #t;
  end;
end method;

define class <neuron-data> (<object>)
  slot neuron-lobe, init-keyword: lobe:;
  slot neuron-number, init-keyword: number:;
  slot neuron-input = 0.0, init-keyword: input:;
  slot neuron-state-0 = 0.0, init-keyword: state-0:;
  slot neuron-state-1 = 0.0, init-keyword: state-1:;
  slot neuron-state-2 = 0.0, init-keyword: state-2:;
  slot neuron-state-3 = 0.0, init-keyword: state-3:;
  slot neuron-state-4 = 0.0, init-keyword: state-4:;
  slot neuron-state-5 = 0.0, init-keyword: state-5:;
  slot neuron-state-6 = 0.0, init-keyword: state-6:;
  slot neuron-state-7 = 0.0, init-keyword: state-7:;
end class;

define method neuron-description( nd )
  get-cell-description(nd.neuron-lobe, nd.neuron-number)
end;

define method get-float-value( address, offset )
  let addr = \%+(address, offset);
  let lp-float = make(<c-float*>, address: addr);
  with-stack-structure (lp-float2 :: <c-float*>)
     c-unsigned-char-at(lp-float2, byte-index: 0) :=
       c-unsigned-char-at(lp-float, byte-index: 0);
     c-unsigned-char-at(lp-float2, byte-index: 1) :=
       c-unsigned-char-at(lp-float, byte-index: 1);
     c-unsigned-char-at(lp-float2, byte-index: 2) :=
       c-unsigned-char-at(lp-float, byte-index: 2);
     c-unsigned-char-at(lp-float2, byte-index: 3) :=
       c-unsigned-char-at(lp-float, byte-index: 3);
     if(zero?(as(<integer>, c-unsigned-char-at(lp-float2, byte-index: 3))))
       0.0
     else
       pointer-value(lp-float2);
     end;
  end with-stack-structure; 

//  pointer-value(make(<c-float*>, address: \%+(address, offset)));
end;

define method create-neuron-list ( engine, lobe )
  let v = #f;
  with-mutex(engine.engine-mutex)
    execute-caos(engine, format-to-string("targ norn brn: dmpl %d", lobe));
    let base = pointer-address(engine.engine-memory-pointer);
    when(zero?(pointer-value(make(<c-unsigned-long*>, address: \%+(base, 8)))))
      let width = pointer-value(make(<c-unsigned-long*>, address: \%+( base, 48 )));
      let height = pointer-value(make(<c-unsigned-long*>, address: \%+( base, 52)));
      let number-of-cells = width * height;
      v := make(<vector>, size: number-of-cells);
      for(i from 0 below number-of-cells)
        let number = pointer-value(make(<c-unsigned-long*>, address: \%+(base, 584 + 40 * i + 4)));
        let input = get-float-value(base, 584 + 40 * i);
        let state-0 = get-float-value(base, 584 + 40 * i + 8);
        let state-1 = get-float-value(base, 584 + 40 * i + 12);
        let state-2 = get-float-value(base, 584 + 40 * i + 16);
        let state-3 = get-float-value(base, 584 + 40 * i + 20);
        let state-4 = get-float-value(base, 584 + 40 * i + 24);
        let state-5 = get-float-value(base, 584 + 40 * i + 28);
        let state-6 = get-float-value(base, 584 + 40 * i + 32);
        let state-7 = get-float-value(base, 584 + 40 * i + 36);
        v[i] := make(<neuron-data>, lobe: lobe, number: number, input: input, state-0: state-0,
                     state-1: state-1, state-2: state-2, state-3: state-3, state-4: state-4,
                     state-5: state-5, state6: state-6, state-7: state-7);
      end for;
    end when;
  end with-mutex;
  v
end;

define method on-button-connect( g )
  let frame = g.sheet-frame;
  local method connection-thread ()
          while(~frame.frame-abort-thread?)
            local method do-in-frame()                
              when(frame.lobe-list.gadget-value)
                let v = create-neuron-list(frame.frame-connection, frame.lobe-list.gadget-value);
                when(v)              
                  frame.cell-list.gadget-items := 
                  choose(method(cell)
                        ~zero?(cell.neuron-input) |
                        ~zero?(cell.neuron-state-0) |
                        ~zero?(cell.neuron-state-1) |
                        ~zero?(cell.neuron-state-2) |
                        ~zero?(cell.neuron-state-3) |
                        ~zero?(cell.neuron-state-4) |
                        ~zero?(cell.neuron-state-5) |
                        ~zero?(cell.neuron-state-6) |
                        ~zero?(cell.neuron-state-7) end, v);
                  frame.frame-title := concatenate("C3 Lobe Study - ", 
                    execute-caos(frame.frame-connection, "targ norn outs hist name gtos 0"));
                end when;
              end when;
            end method;
            call-in-frame(frame, do-in-frame);            
            sleep(1);
          end while;
          call-in-frame(frame, method() frame.cell-list.gadget-items := #[] end);
  end method;

  unless(frame.frame-connection)
    block()
      frame.frame-connection := make(<creatures-engine>);
      connect(frame.frame-connection, "Creatures 3");
      frame.frame-abort-thread? := #f;
      make(<thread>, function: connection-thread);
    exception(e :: <condition>)
      notify-user("Could not connect to Creatures 3. Please check that it is running and try again.");
      disconnect(frame.frame-connection);
      frame.frame-connection := #f;
    end;
  end;
end;

define method on-button-disconnect( g )
  let frame = g.sheet-frame;
  when(frame.frame-connection)
    disconnect(frame.frame-connection);
    frame.frame-connection := #f;
    frame.frame-abort-thread? := #t;
    frame.frame-title := "C3 Lobe Study";
  end;
end;

define method on-about(gadget)
  notify-user(concatenate("Version 1.3\nCopyright (c) 1999, Chris Double.\n"
              "All Rights Reserved.\n"
              "Using c3-engine.dll V", c3-engine-version(), "\n" 
              "http://www.double.co.nz/creatures"),
              title: "Creatures 3 Lobe Study");
end method;

define method main () => ()
  start-frame(make(<lobe-study-frame>));
end method main;

begin
  main();
end;

Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1996-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Progress notes

// The current progress note
define thread variable *progress-note* = #f;

define open generic clear-progress-note
    (framem :: <abstract-frame-manager>, note) => ();
define open generic display-progress-note
    (framem :: <abstract-frame-manager>, note) => ();
define open generic raise-progress-note
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>)
 => (sheet :: false-or(<sheet>));
define open generic lower-progress-note
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>, sheet) => ();


define sealed class <progress-note> (<object>)
  sealed slot progress-note-sheet = #f,
    init-keyword: sheet:;
  sealed slot progress-note-frame = #f,
    init-keyword: frame:;
  sealed slot progress-note-label = #f,
    init-keyword: label:,
    setter: %label-setter;
  sealed slot %numerator = 0;
  sealed slot %denominator = 1;
  // Some state to help keep the flicker down
  sealed slot %label-displayed? = #f;
  sealed slot %bar-length = 0;
end class <progress-note>;
    
define sealed domain make (singleton(<progress-note>));
define sealed domain initialize (<progress-note>);

define method progress-note-label-setter (label, note :: <progress-note>) => (label)
  note.%label := label;
  note.%label-displayed? := #f;
  note.%bar-length := 0;
  let frame = progress-note-frame(note);
  when (frame)
    display-progress-note(frame-manager(frame), note)
  end;
  label
end method progress-note-label-setter;


define macro noting-progress
  { noting-progress (?label:expression) ?:body end }
    => { begin
	   let noting-progress-body = method () ?body end;
	   do-noting-progress(#f, ?label, noting-progress-body)
	 end }
  { noting-progress (?sheet:expression, ?label:expression) ?:body end }
    => { begin
	   let noting-progress-body = method () ?body end;
	   do-noting-progress(?sheet, ?label, noting-progress-body)
	 end }
end macro noting-progress;

define method do-noting-progress
    (sheet :: <sheet>, label, continuation :: <function>,
     #key frame = sheet-frame(sheet), cursor) => (#rest values)
  let old-note = *progress-note*;
  let new-note = make(<progress-note>,
		      label: label,
		      sheet: sheet,
		      frame: frame);
  let pointer = port(sheet) & port-pointer(port(frame));
  let old-cursor = pointer & pointer-cursor(pointer);
  dynamic-bind (*progress-note* = new-note)
    block ()
      when (pointer & cursor)
	pointer-cursor(pointer) := cursor
      end;
      display-progress-note(frame-manager(frame), new-note);
      continuation()
    cleanup
      // If there was an old note, restore it, otherwise get rid
      // of the progress note display
      when (pointer & cursor)
	pointer-cursor(pointer) := old-cursor
      end;
      if (old-note)
	display-progress-note(frame-manager(frame), old-note);
      else
	lower-progress-note(frame-manager(frame), frame, sheet)
      end
    end
  end
end method do-noting-progress;

define method do-noting-progress
    (sheet == #f, label, continuation :: <function>,
     #key frame = current-frame(), cursor) => (#rest values)
  let sheet = raise-progress-note(frame-manager(frame), frame);
  when (sheet)
    do-noting-progress(sheet, label, continuation,
                       frame: frame, cursor: cursor)
  end
end method do-noting-progress;

define method do-noting-progress
    (frame :: <frame>, label, continuation :: <function>,
     #key frame: _frame = frame, cursor) => (#rest values)
  ignore(_frame);
  do-noting-progress(#f, label, continuation,
                     frame: frame, cursor: cursor)
end method do-noting-progress;


define method note-progress
    (numerator, denominator,
     #key note = *progress-note*, label, cursor) => ()
  when (note)
    let pointer = port-pointer(port(progress-note-frame(note)));
    when (pointer & cursor)
      pointer-cursor(pointer) := cursor
    end;
    when (label)
      progress-note-label(note) := label
    end;
    note.%numerator := numerator;
    note.%denominator := denominator
  end;
  display-progress-note(frame-manager(progress-note-frame(note)), note)
end method note-progress;

define method note-progress-in-phases
    (numerator, denominator,
     #key note = *progress-note*, label, phase-number = 0, n-phases = 1) => ()
  when (note)
    note-progress(denominator * phase-number + numerator,
		  denominator * n-phases,
		  note: note, label: label)
  end
end method note-progress-in-phases;


/// Default implementation of progress notes

define method clear-progress-note
    (framem :: <basic-frame-manager>, note :: <progress-note>) => ()
  let sheet = progress-note-sheet(note);
  when (sheet)
    gadget-value(sheet) := #f
  end
end method clear-progress-note;

define method display-progress-note
    (framem :: <basic-frame-manager>, note :: <progress-note>) => ()
  let sheet = progress-note-sheet(note);
  let denominator = note.%denominator;
  when (denominator > 0)
    gadget-value-range(sheet) := range(from: 0, to: denominator)
  end;
  gadget-label(sheet) := progress-note-label(note);
  gadget-value(sheet) := note.%numerator
end method display-progress-note;


define method raise-progress-note
    (framem :: <basic-frame-manager>, frame :: <frame>) => (sheet :: false-or(<sheet>))
  frame-status-bar(frame)
end method raise-progress-note;

define method lower-progress-note
    (framem :: <basic-frame-manager>, frame :: <frame>, sheet) => ()
  // Switch off the progress control, but leave the last message
  gadget-value(sheet) := #f
end method lower-progress-note;

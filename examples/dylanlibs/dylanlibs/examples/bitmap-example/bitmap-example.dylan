Module:    bitmap-example
Synopsis:  Example of reading bitmaps from files.
Author:    Chris Double
Copyright: (C) 2000, Chris Double. All rights reserved.

define frame <bitmap-example-frame> (<simple-frame>)
  slot current-bitmap = #f;

  pane load-button (frame)
    make(<push-button>, label: "Load Bitmap", activate-callback: do-load-bitmap);

  pane bitmap-pane (frame)
    make(<drawing-pane>, display-function: draw-bitmap-pane);

  layout (frame)
    vertically()
      frame.load-button;
      frame.bitmap-pane;
    end;

  keyword title: = "Bitmap Example 1";
end frame;


define constant $image-filters = #[
    #["Bitmap Files (*.bmp)", "*.bmp"],
    #["All Files (*.*)", "*.*"]
];

define method do-load-bitmap( gadget )
  let frame = gadget.sheet-frame; 
  let bitmap-file = choose-file(owner: frame, filters: $image-filters);

  when(bitmap-file)
    frame.current-bitmap := load-bitmap-from-file(bitmap-file);
    unless(frame.current-bitmap)
      notify-user("Could not load image file.");
    end;                  
    force-display(frame.bitmap-pane);
  end;
end method;

define method draw-bitmap-pane ( pane :: <drawing-pane>, 
    medium :: <medium>, region :: <region> ) => ()
  when(pane.sheet-frame.current-bitmap)
    draw-image(pane, pane.sheet-frame.current-bitmap, 0, 0);
  end;
end method;


define method main () => ()
  start-frame(make(<bitmap-example-frame>));
end method main;

begin
  main();
end;

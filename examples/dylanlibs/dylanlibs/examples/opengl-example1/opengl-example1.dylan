Module:    opengl-example1
Synopsis:  Example OpenGL Project
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.

define constant $PFD-TYPE-RGBA = 0;
define constant $PFD-MAIN-PLANE = 0;
define constant $PFD-DOUBLEBUFFER = #x0001;
define constant $PFD-DRAW-TO-WINDOW = #x0004;
define constant $PFD-SUPPORT-OPENGL = #x0020;

// Couldn't find this in the Dylan GL libraries
define C-function wglDeleteContext
  parameter hglrc :: <HGLRC>;
  result    var :: <BOOL>;
  c-name: "wglDeleteContext", c-modifiers: "__stdcall";
end C-function;

// Some macros for doing resource acquisition nicely.
define macro with-opengl-context
  { with-opengl-context ( ?context:expression, ?medium:expression ) ?:body end }
    => { begin
          wglMakeCurrent(?medium.medium-drawable.get-dc, ?context);
          block()
            ?body
          cleanup
            wglMakeCurrent($null-handle, $null-handle);
          end;
         end }
end macro with-opengl-context;

// Some macros for making OpenGL calls a bit easier to grok
define macro with-gl-begin
  { with-gl-begin ( ?type:expression ) ?:body end }
    => { begin
           glBegin( ?type );
           block()
             ?body
           cleanup
             glEnd()
           end
         end }
end macro with-gl-begin;


define frame <example1-frame> (<simple-frame>)
  slot %opengl-context = #f;
  virtual constant slot opengl-context;

  pane picture-pane (frame)
    make(<drawing-pane>, 
      display-function: draw-picture-pane,
      min-width: 400,
      min-height: 400);

  layout (frame)
    vertically()
      frame.picture-pane;
    end;

  keyword title: = "OpenGL Example 1";
//  keyword width: = 400;
//  keyword height: = 400;
end frame;

// Virtual slot that creates the context if it
// does not already exist.
define method opengl-context( frame :: <example1-frame> ) 
  frame.%opengl-context |
    begin
      let hwnd = frame.picture-pane.window-handle; 
      let hdc = GetDC(hwnd);
      // Pretty much taken from the harlequin OpenGL example
      with-stack-structure (ppfd :: <PPIXELFORMATDESCRIPTOR>)
        ppfd.nSize-value := size-of(<PIXELFORMATDESCRIPTOR>);
        ppfd.nVersion-value := 1;
        ppfd.dwFlags-value := logior($PFD-DRAW-TO-WINDOW, $PFD-SUPPORT-OPENGL ,
                                    $PFD-DOUBLEBUFFER);
        ppfd.dwLayerMask-value := $PFD-MAIN-PLANE; 
        ppfd.iPixelType-value := $PFD-TYPE-RGBA; 
        ppfd.cColorBits-value := 8; 
        ppfd.cDepthBits-value := 16; 
        ppfd.cAccumBits-value := 0; 
        ppfd.cStencilBits-value := 0; 
 
        let pixelformat = ChoosePixelFormat(hdc, ppfd); 
        SetPixelFormat(hdc, pixelformat, ppfd);
      end with-stack-structure;
      frame.%opengl-context := wglCreateContext(hdc);
      ReleaseDC(hdc, hwnd);  
      frame.%opengl-context;
    end;
end method opengl-context;


define method handle-event( frame :: <example1-frame>, event :: <frame-unmapped-event> ) => ()
  wglMakeCurrent($null-handle, $null-handle);
  wglDeleteContext( frame.opengl-context );
  frame.%opengl-context := #f;
end method handle-event;

define method draw-picture-pane ( pane :: <drawing-pane>, 
    medium :: <medium>, region :: <region> ) => ()
  with-opengl-context( pane.sheet-frame.opengl-context, medium )
    let (width, height) = pane.sheet-size;
    glViewport(0, 0, width, height);
    glClear( $GL-COLOR-BUFFER-BIT );
    with-gl-begin( $GL-TRIANGLES )
      glColor3f( 1.0, 0.0, 0.0 );
      glVertex2i( 0, 1 );
      glColor3f(0.0, 1.0, 0.0 );
      glVertex2i( -1, -1 );
      glColor3f(0.0, 0.0, 1.0);
      glVertex2i( 1, -1 );
    end with-gl-begin;
    glFlush();
    Swapbuffers(medium.medium-drawable.get-dc);
  end;
end method draw-picture-pane;

define method main () => ()
  start-frame(make(<example1-frame>));
end method main;

begin
  main();
end;

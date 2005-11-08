module:    inertia
synopsis:  Inertia initializations
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-main.dylan
//
/*
define macro method-definer
  { define method (?parameters:*)
      ?:body
    end }
   => { define method (?parameters)
          ?body
        end }
end;
*/

define variable *function-table* = make (<table>);
/*
define macro method-definer
  { define ?adjectives:* method ?:name ?rest:* end }
   => { dylan-method-definer( ?adjectives method ?name ?rest end );
        *function-table*[?"name"] := ?name }
end;
*/
c-include("/usr/include/w32api/GL/glu.h");

define generic make-from-symbol (symbol :: <symbol>, #rest init-args, #key);

define method make-from-symbol (symbol :: <symbol>, #rest init-args, #key) end;

define variable reshape-callback :: <function> = callback-method (width :: <integer>, height :: <integer>) => ();
//define variable reshape = callback-method (width :: <integer>, height :: <integer>) => ();
  glMatrixMode ($GL-PROJECTION);
    glLoadIdentity ();
    gluOrtho2D (0.0, as(<double-float>, width), as(<double-float>, height), 0.0);
  glMatrixMode ($GL-MODELVIEW);
  glViewport (0, 0, width, height);
end;

define variable display :: <function> = callback-method () => ();
  glClear ($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);
  glLoadIdentity ();
  glTranslate (0.375, 0.375, 0.0);

  glColor (0.0, 0.0, 0.0, 1.0);

  draw-shape (*screen*);

  glutSwapBuffers ();
end;

define variable *button* = #"none-button";

define variable mouse-callback :: <function> = callback-method (button :: <integer>,
                                  state :: <integer>, x :: <integer>, y :: <integer>) => ();
  let origin = make (<point>, x: as(<double-float>, x), y: as(<double-float>, y));
  *button* := select (button)
    $GLUT-LEFT-BUTTON => $left-button;
    $GLUT-MIDDLE-BUTTON => $middle-button;
    $GLUT-RIGHT-BUTTON => $right-button;
  end;

  if (state == $GLUT-DOWN)
    send-event (*screen*, make (<mouse-down-event>, origin: origin), *button*);
  elseif (state == $GLUT-UP)
    send-event (*screen*, make (<mouse-up-event>, origin: origin), *button*);
    *button* := #"none-button";
  end;
  glutPostRedisplay ();
end;

define variable passive-motion :: <function> = callback-method (x :: <integer>, y :: <integer>)
 => ();
  let origin = make (<point>, x: as(<double-float>, x), y: as(<double-float>, y));
  send-event (*screen*, make (<mouse-motion-event>, origin: origin), *button*);
end;

define variable motion-callback :: <function> = callback-method (x :: <integer>, y :: <integer>)
 => ();
  let origin = make (<point>, x: as(<double-float>, x), y: as(<double-float>, y));
  send-event (*screen*, make (<mouse-drag-event>, origin: origin), *button*);
  glutPostRedisplay ();
end;

define variable polygon-begin :: <function> = callback-method (mode :: <integer>) => ();
end;

// ------------------------------------------------------------------------- //

define class <persistent> (<object>)
  each-subclass slot fields = make (<table>);
end;

define method table (#rest args, #key, #all-keys)
  let table = make(<table>);
  for (i from 0 below args.size by 2)
    table[args[i]] := args[i + 1];
  end;
  table;
end;

define method duplicate (string :: <string>, count :: <integer>)
  let stream = make (<string-stream>, direction: #"output");
  while (count ~= 0)
    write (stream, string);
    count := count - 1;
  end;
  stream-contents (stream);
end;

define class <field> (<object>)
  slot category = "general", init-keyword: category:;
  slot getter, init-keyword: getter:;
  slot setter, init-keyword: setter:;
end;

define method build-object (object :: <persistent>, filename :: <string>, #key)
  let stream = make (<file-stream>, locator: filename, direction: #"input");
  build-object (object, stream);
end;

define method build-object (object :: <persistent>, stream :: <stream>, #key level = 0)
  block (return)
    let name = #f;
    let value = #f;

    while (~stream-at-end? (stream))
      block (break)
        while (whitespace? (peek (stream)))
          read (stream, 1);
        end;

        if (peek (stream) == '#')
          read-line (stream);
        end;

        let data = read-to (stream, ' ');
        if (data = "object")
          let data = read-to (stream, ' ');
          let name = as(<symbol>, copy-sequence (data, start: 0, end: data.size - 1));
          let value = read-line (stream);
          format-out ("%sobject [%s] [%s]\n", duplicate (" ", level * 2), name, value);
          let child = make-from-symbol (as(<symbol>, value));
          build-object (child, stream, level: level + 1);
          *function-table*[name] := child;
          add-child (object, child);
          break ();
        elseif (data = "end")
          return ();
        end;

        name := as(<symbol>, copy-sequence (data, start: 0, end: data.size - 1));

        let data = read-line (stream);
        value := case
          digit? (data[0]) => string-to-float (data);
          data[0] == '\"' => copy-sequence (data, start: 1, end: data.size - 1);
          alpha? (data[0]) => as(<symbol>, data);
        end;

        format-out ("%s[%s] [%s]\n", duplicate (" ", level * 2), name, value);
        object.fields[name].setter (value, object);
      end block;
    end while;
  cleanup
    close (stream);
  exception (xerror :: <error>)
    //apply (format-out, xerror.condition-format-string, xerror.condition-format-arguments);
  end;
end;

define method asdf (abc)
  abc * 2;
end;

*function-table*[#"asdf"] := asdf;

define method perform (method-name :: <symbol>, #rest args)
  apply (*function-table*[method-name], args);
end;

// ------------------------------------------------------------------------- //

begin
  glut-init ();
  glutInitDisplayMode ($GLUT-RGBA + $GLUT-DEPTH + $GLUT-DOUBLE + $GLUT-STENCIL);

  glutInitWindowPosition (200, 100);
  glutInitWindowSize (1024, 768);
  glutCreateWindow ("Dylan Inertia");

  glutDisplayFunc (display);
  glutReshapeFunc (reshape-callback);
  glutPassiveMotionFunc (passive-motion);
  glutMouseFunc (mouse-callback);
  glutMotionFunc (motion-callback);
  
  glClearColor (1.0s0, 1.0s0, 1.0s0, 1.0s0);
  
  glEnable ($GL-BLEND); glBlendFunc ($GL-SRC-ALPHA, $GL-ONE-MINUS-SRC-ALPHA);
  glEnable ($GL-LINE-SMOOTH); glLineWidth (1.5s0);
  glEnable ($GL-POINT-SMOOTH); glPointSize( 1.5s0);

  define variable *tess-object* = gluNewTess ();

  call-out ("gluTessCallback", void:, ptr: *tess-object*.raw-value, int: 100100, ptr: c-expr (ptr: "glBegin"));
  call-out ("gluTessCallback", void:, ptr: *tess-object*.raw-value, int: 100101, ptr: c-expr (ptr: "glVertex3dv"));
  call-out ("gluTessCallback", void:, ptr: *tess-object*.raw-value, int: 100102, ptr: c-expr (ptr: "glEnd"));

  define variable *screen* = make (<screen>, width: 1024.0, height: 768.0);
  define variable *menu* = make (<shape-menu>, width: 100.0, height: 180.0);
  add-child (*screen*, *menu*);
  
  format-out ("%=\n", perform (#"asdf", 10));
end;


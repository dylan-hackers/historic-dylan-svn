module: opengl
synopsis: Dylan bindings for OpenGL functions
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

/****************************************************************************
 * with-glBegin(primitive type)
 *   <code>
 * end;
 *
 * On exit from this block (scheduled or otherwise), calls glEnd() for you.
 ****************************************************************************/

define macro with-glBegin
  { with-glBegin (?:expression) ?:body end }
    => { glBegin(?expression);
         block ()
           ?body
	 cleanup
	   glEnd();
         end block };
end macro; 

/****************************************************************************
 * with-glNewList(display list, primitive type)
 *   <code>
 * end;
 *
 * On exit from this block (scheduled or otherwise), calls glEndList() for you.
 ****************************************************************************/

define macro with-glNewList
  { with-glNewList(?d:expression, ?p:expression) ?:body end }
    => { glNewList(?d, ?p);
         block ()
           ?body
         cleanup
           glEndList();
         end block };
end macro; 

/****************************************************************************
 * with-glPushMatrix ()
 *   <code>
 * end;
 *
 * On exit from this block (scheduled or otherwise), calls glPopMatrix() for you.
 ****************************************************************************/

define macro with-glPushMatrix
  { with-glPushMatrix() ?:body end }
    => { glPushMatrix();
         block ()
           ?body
         cleanup
           glPopMatrix();
         end block };
end macro; 

/*****************************************************************************
 * glAccum(op :: <Glenum>, val :: <single-float>)
 *
 * Calls glAccum after verifying enum argument.
 ****************************************************************************/
define inline function glAccum
    (op :: one-of($GL-ACCUM, $GL-LOAD, $GL-ADD, $GL-MULT, $GL-RETURN), 
     val :: <single-float>) => ();
  glAccum-internal(op, val);
end function;

/*****************************************************************************
 * glAlphaFunc(func :: <Glenum>, val :: <single-float>)
 *
 * Calls glAlphaFunc after verifying enum argument.
 ****************************************************************************/

define inline function glAlphaFunc
    (func :: one-of($GL-NEVER, $GL-LESS, $GL-EQUAL, $GL-LEQUAL, $GL-GREATER,
		    $GL-NOTEQUAL, $GL-GEQUAL, $GL-ALWAYS), 
     ref :: <single-float>)
 => ()
  glAlphaFunc-internal(func, ref);
end function;

/*****************************************************************************
 * glBegin(mode :: <Glenum>)
 *
 * Calls glBegin after verifying enum argument.
 ****************************************************************************/

define inline function glBegin
    (mode :: one-of($GL-POINTS, $GL-LINES, $GL-LINE-STRIP, $GL-LINE-LOOP,
		    $GL-TRIANGLES, $GL-TRIANGLE-STRIP, $GL-TRIANGLE-FAN,
		    $GL-QUADS, $GL-QUAD-STRIP, $GL-POLYGON))
 => () 
  glBegin-internal(mode);
end function;

/*****************************************************************************
 * glBindTexture(target :: <Glenum>)
 *
 * Calls glBindTexture after verifying enum argument.
 ****************************************************************************/

define inline function glBindTexture
    (target :: one-of($GL-TEXTURE-1D, $GL-TEXTURE-2D), texture :: <integer>)
 => () 
  glBindTExture-internal(target, texture);
end function;

/*****************************************************************************
 * glBlendFunc(sfactor :: <Glenum>, dfactor :: <GLenum>)
 *
 * Calls glBlendFunc after verifying enum argument.
 ****************************************************************************/

define inline function glBlendFunc
    (sfactor :: one-of($GL-ZERO, $GL-ONE, $GL-DST-COLOR, 
		       $GL-ONE-MINUS-DST-COLOR, $GL-SRC-ALPHA, 
		       $GL-ONE-MINUS-SRC-ALPHA, $GL-DST-ALPHA,
		       $GL-ONE-MINUS-DST-ALPHA, $GL-SRC-ALPHA-SATURATE), 
     dfactor :: one-of($GL-ZERO, $GL-ONE, $GL-SRC-COLOR, 
		       $GL-ONE-MINUS-SRC-COLOR, $GL-SRC-ALPHA, 
		       $GL-ONE-MINUS-SRC-ALPHA, $GL-DST-ALPHA,
		       $GL-ONE-MINUS-DST-ALPHA))
 => () 
  glBlendFunc-internal(sfactor, dfactor);
end function;

/*****************************************************************************
 * glColor(r, g, b[, a])
 *
 * Calls glColor[34]u?[bdfis]v? as appropriate.
 ****************************************************************************/
 
define macro glColor
  { glColor(?r:expression, ?g:expression, ?b:expression) }
    => { glColor3(?r, ?g, ?b) }
  { glColor(?r:expression, ?g:expression, ?b:expression, ?a:expression) }
    => { glColor4(?r, ?g, ?b, ?a) }
end macro;


define function-family-3 glColor3
  <double-float> => glColor3d;
  <single-float> => glColor3f;
  <integer>      => glColor3i;
end function-family-3;

define function-family-4 glColor4
  <double-float> => glColor4d;
  <single-float> => glColor4f;
  <integer>      => glColor4i;
end function-family-4;

/*****************************************************************************
 * glColorMaterial(face :: <GLenum>, mode :: <Glenum>)
 *
 * Calls glColorMaterial after verifying enum argument.
 ****************************************************************************/

define inline function glColorMaterial
    (face :: one-of($GL-FRONT, $GL-BACK, $GL-FRONT-AND-BACK), 
     mode :: one-of($GL-EMISSION, $GL-AMBIENT, $GL-DIFFUSE, $GL-SPECULAR,
		    $GL-AMBIENT-AND-DIFFUSE))
 => () 
  glColorMaterial-internal(face, mode);
end function;

/*****************************************************************************
 * glCopyPixels(x :: <integer>, y :: <integer>, 
 *		width :: <integer>, height :: <integer>, type :: <GLenum>)
 *
 * Calls glCopyPixels after verifying width, height, and enum argument.
 ****************************************************************************/

define inline function glCopyPixels
    (x :: <integer>, y :: <integer>, 
     width :: limited(<integer>, min: 0), 
     height :: limited(<integer>, min: 0), 
     type :: one-of($GL-COLOR, $GL-DEPTH, $GL-STENCIL))
 => () 
  glCopyPixels-internal(x, y, width, height, type);
end function;

/*****************************************************************************
 * glCopyTexImage1D(target :: <GLenum>, level :: <integer>, 
 *		    internalFormat :: <GLenum>,
 *		    x :: <integer>, width :: <integer>, border :: <integer>)
 *
 * Calls glCopyTexImage1D after verifying 
 * level, width, height, border, and enum arguments.
 ****************************************************************************/

define inline function glCopyTexImage1D
    (target == $GL-TEXTURE-1D, 
     level :: limited(<integer>, min: 0 /*, 
		      max: log(base: 2, $GL-MAX-TEXTURE-SIZE) */),
     internalFormat :: one-of($GL-ALPHA, $GL-ALPHA4, $GL-ALPHA8, $GL-ALPHA12,
			      $GL-ALPHA16, $GL-LUMINANCE, $GL-LUMINANCE4,
			      $GL-LUMINANCE8, $GL-LUMINANCE12, 
			      $GL-LUMINANCE16, $GL-LUMINANCE-ALPHA,
			      $GL-LUMINANCE4-ALPHA4, $GL-LUMINANCE6-ALPHA2,
			      $GL-LUMINANCE8-ALPHA8, $GL-LUMINANCE12-ALPHA4,
			      $GL-LUMINANCE12-ALPHA12, 
			      $GL-LUMINANCE16-ALPHA16, $GL-INTENSITY,
			      $GL-INTENSITY4, $GL-INTENSITY8, 
			      $GL-INTENSITY12, $GL-INTENSITY16, $GL-RGB,
			      $GL-R3-G3-B2, $GL-RGB4, $GL-RGB5, $GL-RGB8,
			      $GL-RGB10, $GL-RGB12, $GL-RGB16, $GL-RGBA,
			      $GL-RGBA2, $GL-RGBA4, $GL-RGB5-A1, $GL-RGBA8,
			      $GL-RGB10-A2, $GL-RGBA12, $GL-RGBA16),
     x :: <integer>, y :: <integer>,
     width :: limited(<integer>, min: 0, max: 2 + $GL-MAX-TEXTURE-SIZE),
     border :: limited(<integer>, min: 0, max: 1))
 => ()
  glCopyTexImage1D-internal(target, level, internalFormat, x,y, width,border);
end function;

/*****************************************************************************
 * glCopyTexImage2D(target :: <GLenum>, level :: <integer>, 
 *		    internalFormat :: <GLenum>,
 *		    x :: <integer>, y :: <integer>, 
 *		    width :: <integer>, height :: <integer>,
 *		    border :: <integer>)
 *
 * Calls glCopyTexImage2D after verifying 
 * level, width, height, border, and enum arguments.
 ****************************************************************************/

define inline function glCopyTexImage2D
    (target == $GL-TEXTURE-2D, 
     level :: limited(<integer>, min: 0 /*,
		      max: log(base: 2, $GL-MAX-TEXTURE-SIZE)*/),
     internalFormat :: one-of($GL-ALPHA, $GL-ALPHA4, $GL-ALPHA8, $GL-ALPHA12,
			      $GL-ALPHA16, $GL-LUMINANCE, $GL-LUMINANCE4,
			      $GL-LUMINANCE8, $GL-LUMINANCE12, 
			      $GL-LUMINANCE16, $GL-LUMINANCE-ALPHA,
			      $GL-LUMINANCE4-ALPHA4, $GL-LUMINANCE6-ALPHA2,
			      $GL-LUMINANCE8-ALPHA8, $GL-LUMINANCE12-ALPHA4,
			      $GL-LUMINANCE12-ALPHA12, 
			      $GL-LUMINANCE16-ALPHA16, $GL-INTENSITY,
			      $GL-INTENSITY4, $GL-INTENSITY8, 
			      $GL-INTENSITY12, $GL-INTENSITY16, $GL-RGB,
			      $GL-R3-G3-B2, $GL-RGB4, $GL-RGB5, $GL-RGB8,
			      $GL-RGB10, $GL-RGB12, $GL-RGB16, $GL-RGBA,
			      $GL-RGBA2, $GL-RGBA4, $GL-RGB5-A1, $GL-RGBA8,
			      $GL-RGB10-A2, $GL-RGBA12, $GL-RGBA16),
     x :: <integer>, y :: <integer>, 
     width :: limited(<integer>, min: 0, max: 2 + $GL-MAX-TEXTURE-SIZE),
     height :: limited(<integer>, min: 0, max: 2 + $GL-MAX-TEXTURE-SIZE),
     border :: limited(<integer>, min: 0, max: 1))
 => ()
  glCopyTexImage2D-internal(target, level, internalFormat, 
			    x, y, width, height, border);
end function;

/*****************************************************************************
 * glCopyTexSubImage1D(target :: <GLenum>, level :: <integer>, 
 *		    xoffset :: <integer>, x :: <integer>, y :: <integer>, 
 *		    width :: <integer>)
 *
 * Calls glCopyTexSubImage1D after verifying target and level.
 ****************************************************************************/

define inline function glCopyTexSubImage1D
    (target == $GL-TEXTURE-1D,
     level :: limited(<integer>, min: 0 /*, 
		      max: log(base: 2, $GL-MAX-TEXTURE-SIZE)*/),
     xoffset :: <integer>, x :: <integer>, y :: <integer>,
     width :: <integer>)
 => ()
  glCopyTexSubImage1D-internal(target, level, xoffset, x, y, width);
end function;

/*****************************************************************************
 * glCopyTexSubImage2D(target :: <GLenum>, level :: <integer>, 
 *		    xoffset :: <integer>, yoffset :: <integer>,
 *		    x :: <integer>, y :: <integer>,
 *		    width :: <integer>, height :: <integer>)
 *
 * Calls glCopyTexSubImage2D after verifying target and level.
 ****************************************************************************/

define inline function glCopyTexSubImage2D
    (target == $GL-TEXTURE-2D,
     level :: limited(<integer>, min: 0 /*,
		      max: log(base: 2, $GL-MAX-TEXTURE-SIZE) */),
     xoffset :: <integer>, yoffset :: <integer>, 
     x :: <integer>, y :: <integer>,
     width :: <integer>, height :: <integer>)
 => ()
  glCopyTexSubImage2D-internal(target, level, xoffset, yoffset,
			       x, y, width, height);
end function;


/*****************************************************************************
 * glEvalCoord(u[, v])
 *
 * Calls glEvalCoord[12][df] as appropriate.
 ****************************************************************************/

define macro glEvalCoord
  { glEvalCoord(?u:expression) }
    => { glEvalCoord1(?u) }
  { glEvalCoord(?u:expression, ?v:expression) }
    => { glEvalCoord2(?u, ?v) }
end macro;


define function-family-1 glEvalCoord1
  <double-float> => glEvalCoord1d;
  <single-float> => glEvalCoord1f;
end function-family-1;

define function-family-2 glEvalCoord2
  <double-float> => glEvalCoord2d;
  <single-float> => glEvalCoord2f;
end function-family-2;

/*****************************************************************************
 * glFog(pname, param)
 *
 * Calls glFog[fi] as appropriate
 ****************************************************************************/

define sealed generic glFog(pname :: <GLenum>, 
			    arg1 :: <number>, #rest rest);

define method glFog(pname :: <GLenum>,
		    arg1 :: <integer>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glFogi(pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-integer-vector> = make(<c-integer-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := arg1;
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := arg;
    end for;
    
    glFogiv(pname, paramlist);
  end if;
end method glFog;

define method glFog(pname :: <GLenum>,
		    arg1 :: <float>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glFogf(pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-float-vector> = make(<c-float-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := as(<single-float>, arg1);
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := as(<single-float>, arg);
    end for;
    
     glFogfv(pname, paramlist);
  end if;
end method glFog;

/*****************************************************************************
 * glIndex({<integer>|<float>} color...)
 * 
 * Calls glIndexu?[dfisb]v? as needed.
 ****************************************************************************/

define function-family-1 glIndex
  <double-float> => glIndexd;
  <single-float> => glIndexf;
  <integer>      => glIndexi;
end function-family-1;

/*****************************************************************************
 * glLight(<integer> light, <GLenum> pname, {<integer>|<float>} arg...)
 * 
 * Changes a parameter on a light.
 ****************************************************************************/

define sealed generic glLight(light :: limited(<integer>, 
			       min: $GLenum$GL-LIGHT0, 
			       max: $GLenum$GL-LIGHT0 + $GLenum$GL-MAX-LIGHTS),
			      pname :: <GLenum>, 
			      arg1 :: <number>, #rest rest);

define method glLight(light :: limited(<integer>, 
			       min: $GLenum$GL-LIGHT0, 
			       max: $GLenum$GL-LIGHT0 + $GLenum$GL-MAX-LIGHTS),
		      pname :: <GLenum>,
		      arg1 :: <integer>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glLighti(light, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-integer-vector> = make(<c-integer-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := arg1;
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := arg;
    end for;
    
    glLightiv(light, pname, paramlist);
  end if;
end method glLight;

define method glLight(light :: limited(<integer>, 
			       min: $GLenum$GL-LIGHT0, 
			       max: $GLenum$GL-LIGHT0 + $GLenum$GL-MAX-LIGHTS),
		      pname :: <GLenum>,
		      arg1 :: <float>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glLightf($GLenum$GL-LIGHT0 + light, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-float-vector> = make(<c-float-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := as(<single-float>, arg1);
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := as(<single-float>, arg);
    end for;
    
     glLightfv( light, pname, paramlist);
  end if;
end method glLight;

/*****************************************************************************
 * glLightModel(pname, param)
 *
 * Calls glLightModel[fi] as appropriate
 ****************************************************************************/

define sealed generic glLightModel(pname :: <GLenum>, 
				   arg1 :: <number>, #rest rest);

define method glLightModel(pname :: <GLenum>,
			   arg1 :: <integer>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glLightModeli(pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-integer-vector> = make(<c-integer-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := arg1;
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := arg;
    end for;
    
    glLightModeliv(pname, paramlist);
  end if;
end method glLightModel;

define method glLightModel(pname :: <GLenum>,
			   arg1 :: <float>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glLightModelf(pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-float-vector> = make(<c-float-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := as(<single-float>, arg1);
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := as(<single-float>, arg);
    end for;
    
     glLightModelfv(pname, paramlist);
  end if;
end method glLightModel;

/*****************************************************************************
 * glMaterial(pname, param, ...)
 *
 * Calls glMaterial[fi] as appropriate
 ****************************************************************************/

define sealed generic glMaterial(face :: <GLenum>, pname :: <GLenum>, 
				 arg1 :: <number>, #rest rest);

define method glMaterial(face :: <GLenum>, pname :: <GLenum>,
			 arg1 :: <integer>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glMateriali(face, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-integer-vector> = make(<c-integer-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := as(<integer>, arg1);
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := as(<integer>, arg);
    end for;
    
    glMaterialiv(face, pname, paramlist);
  end if;
end method glMaterial;

define method glMaterial(face :: <GLenum>, pname :: <GLenum>,
			 arg1 :: <float>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glMaterialf(face, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-float-vector> = make(<c-float-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := as(<single-float>, arg1);
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := as(<single-float>, arg);
    end for;
    
     glMaterialfv(face, pname, paramlist);
  end if;
end method glMaterial;

/****************************************************************************
 * glMultMatrix(matrix) => ()
 * 
 * Calls the appropriate glMultMatrix[df] function.
 ***************************************************************************/

define method glMultMatrix(m :: <collection>) => ();
  if (instance?(m[0], <double-float>))
    let paramlist :: <c-double-vector> =
      make(<c-double-vector>, element-count: 16);
    for (i from 0 below 16, arg in m)
      paramlist[i] := as(<double-float>, arg);
    end for;
    glMultMatrixd(paramlist);
  else
    let paramlist :: <c-float-vector> =
      make(<c-float-vector>, element-count: 16);
    for (i from 0 below 16, arg in m)
      paramlist[i] := as(<single-float>, arg);
    end for;
    glMultMatrixf(paramlist);
  end if;
end method glMultMatrix;

/****************************************************************************
 * glNormal(x, y, z) => ()
 * 
 * Calls the appropriate glNormal3[bdfis] function.
 ***************************************************************************/

define function-family-3 glNormal
  <double-float> => glNormal3d;
  <single-float> => glNormal3f;
  <integer>      => glNormal3i;
end function-family-3;

/****************************************************************************
 * glPixelStore(pname, param) => ()
 * 
 * Calls the appropriate glPixelStore[fi] function.
 ***************************************************************************/

define sealed generic glPixelStore
    (pname :: <GLenum>, param :: <number>) => ();

define inline method glPixelStore
    (pname :: <GLenum>, param :: <single-float>) => ()
  glPixelStoref(pname, param);
end glPixelStore;

define inline method glPixelStore
    (pname :: <GLenum>, param :: <integer>) => ()
  glPixelStorei(pname, param);
end glPixelStore;

/****************************************************************************
 * glPixelTransfer(pname, param) => ()
 * 
 * Calls the appropriate glPixelTransfer[fi] function.
 ***************************************************************************/

define sealed generic glPixelTransfer
    (pname :: <GLenum>, param :: <number>) => ();

define inline method glPixelTransfer
    (pname :: <GLenum>, param :: <single-float>) => ()
  glPixelTransferf(pname, param);
end glPixelTransfer;

define inline method glPixelTransfer
    (pname :: <GLenum>, param :: <integer>) => ()
  glPixelTransferi(pname, param);
end glPixelTransfer;

/****************************************************************************
 * glRasterPos(x, y[, z[, w]]) => ()
 * 
 * Calls the appropriate glRasterPos[234][idf] function.
 ***************************************************************************/

define macro glRasterPos
  { glRasterPos(?x:expression, ?y:expression) }
    => { glRasterPos2(?x, ?y) }
  { glRasterPos(?x:expression, ?y:expression, ?z:expression) }
    => { glRasterPos3(?x, ?y, ?z) }
  { glRasterPos(?x:expression, ?y:expression, ?z:expression, ?w:expression) }
    => { glRasterPos4(?x, ?y, ?z, ?w) }
end macro;

define function-family-2 glRasterPos2
  <integer>      => glRasterPos2i;
  <single-float> => glRasterPos2f;
  <double-float> => glRasterPos2d;
end function-family-2;

define function-family-3 glRasterPos3
  <integer>      => glRasterPos3i;
  <single-float> => glRasterPos3f;
  <double-float> => glRasterPos3d;
end function-family-3;

define function-family-4 glRasterPos4
  <integer>      => glRasterPos4i;
  <single-float> => glRasterPos4f;
  <double-float> => glRasterPos4d;
end function-family-4;

/****************************************************************************
 * glRect(x1, y1, x2, y2) => ()
 * 
 * Calls the appropriate glRect[idf] function.
 ***************************************************************************/

define function-family-4 glRect
  <integer>      => glRecti;
  <single-float> => glRectf;
  <double-float> => glRectd;
end function-family-4;

/****************************************************************************
 * glRotate(angle, x, y, z]) => ()
 * 
 * Calls the appropriate glRotate[df] function.
 ***************************************************************************/

define function-family-4 glRotate
  <single-float> => glRotatef;
  <double-float> => glRotated;
end function-family-4;

/****************************************************************************
 * glScale(x, y, z) => ()
 * 
 * Calls the appropriate glScale[df] function.
 ***************************************************************************/

define function-family-3 glScale
  <single-float> => glScalef;
  <double-float> => glScaled;
end function-family-3;

/****************************************************************************
 * glTexCoord(s[, t[, r[, q]]]) => ()
 * 
 * Calls the appropriate glTexCoord[1234][isdf]v? function.
 ***************************************************************************/

define macro glTexCoord
  { glVertex(?s:expression) }
    => { glTexCoord1(?s) }
  { glVertex(?s:expression, ?t:expression) }
    => { glTexCoord2(?s, ?t) }
  { glVertex(?s:expression, ?t:expression, ?r:expression) }
    => { glTexCoord3(?s, ?t, ?r) }
  { glVertex(?s:expression, ?t:expression, ?r:expression, ?q:expression) }
    => { glTexCoord4(?s, ?t, ?r, ?q) }
end macro;

define function-family-1 glTexCoord1
  <single-float> => glTexCoord1f;
  <double-float> => glTexCoord1d;
  <integer>      => glTexCoord1i;
end function-family-1;

define function-family-2 glTexCoord2
  <single-float> => glTexCoord2f;
  <double-float> => glTexCoord2d;
  <integer>      => glTexCoord2i;
end function-family-2;

define function-family-3 glTexCoord3
  <single-float> => glTexCoord3f;
  <double-float> => glTexCoord3d;
  <integer>      => glTexCoord3i;
end function-family-3;

define function-family-4 glTexCoord4
  <single-float> => glTexCoord4f;
  <double-float> => glTexCoord4d;
  <integer>      => glTexCoord4i;
end function-family-4;

/*****************************************************************************
 * glTexEnv(target, pname, param)
 *
 * Calls glTexEnv[fi] as appropriate
 ****************************************************************************/

define sealed generic glTexEnv(target :: <GLenum>, pname :: <GLenum>, 
				 arg1 :: <number>, #rest rest);

define method glTexEnv(target :: <GLenum>, pname :: <GLenum>,
			 arg1 :: <integer>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glTexEnvi(target, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-integer-vector> = make(<c-integer-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := arg1;
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := arg;
    end for;
    
    glTexEnviv(target, pname, paramlist);
  end if;
end method glTexEnv;

define method glTexEnv(target :: <GLenum>, pname :: <GLenum>,
			 arg1 :: <float>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glTexEnvf(target, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-float-vector> = make(<c-float-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := as(<single-float>, arg1);
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := as(<single-float>, arg);
    end for;
    
     glTexEnvfv(target, pname, paramlist);
  end if;
end method glTexEnv;

/*****************************************************************************
 * glTexGen(coord, pname, param)
 *
 * Calls glTexGen[fdi] as appropriate
 ****************************************************************************/

define sealed generic glTexGen(coord :: <GLenum>, pname :: <GLenum>, 
			       arg1 :: <number>, #rest rest);

define method glTexGen(coord :: <GLenum>, pname :: <GLenum>,
		       arg1 :: <integer>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glTexGeni(coord, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-integer-vector> = make(<c-integer-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := arg1;
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := arg;
    end for;
    
    glTexGeniv(coord, pname, paramlist);
  end if;
end method glTexGen;

define method glTexGen(coord :: <GLenum>, pname :: <GLenum>,
		       arg1 :: <single-float>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glTexGenf(coord, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-float-vector> = make(<c-float-vector>, 
					     element-count: size(rest) + 1);
    paramlist[0] := arg1;
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := arg;
    end for;
    
     glTexGenfv(coord, pname, paramlist);
  end if;
end method glTexGen;

define method glTexGen(coord :: <GLenum>, pname :: <GLenum>,
		       arg1 :: <double-float>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glTexGend(coord, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-double-vector> = make(<c-double-vector>, 
					      element-count: size(rest) + 1);
    paramlist[0] := arg1;
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := arg;
    end for;
    
     glTexGendv(coord, pname, paramlist);
  end if;
end method glTexGen;

/*****************************************************************************
 * glTexParameter(target, pname, param)
 *
 * Calls glTexParameter[fi] as appropriate
 ****************************************************************************/

define sealed generic glTexParameter(target :: <GLenum>, pname :: <GLenum>, 
				 arg1 :: <number>, #rest rest);

define method glTexParameter(target :: <GLenum>, pname :: <GLenum>,
			 arg1 :: <integer>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glTexParameteri(target, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-integer-vector> = make(<c-integer-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := arg1;
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := arg;
    end for;
    
    glTexParameteriv(target, pname, paramlist);
  end if;
end method glTexParameter;

define method glTexParameter(target :: <GLenum>, pname :: <GLenum>,
			 arg1 :: <float>, #rest rest) => ()
  // FIXME: add arg check in debug mode here....
  if (empty?(rest))
    glTexParameterf(target, pname, arg1);
  else
    // stick #rest into array...
    let paramlist :: <c-float-vector> = make(<c-float-vector>, 
					       element-count: size(rest) + 1);
    paramlist[0] := as(<single-float>, arg1);
    for (i from 1 below (size(rest) + 1),
	 arg in rest)
      paramlist[i] := as(<single-float>, arg);
    end for;
    
     glTexParameterfv(target, pname, paramlist);
  end if;
end method glTexParameter;


/****************************************************************************
 * glTranslate(x, y, z) => ()
 * 
 * Calls the appropriate glTranslate[df] function.
 ***************************************************************************/

define function-family-3 glTranslate
  <single-float> => glTranslatef;
  <double-float> => glTranslated;
end function-family-3;

/****************************************************************************
 * glVertex(x, y[, z[, w]]) => ()
 * 
 * Calls the appropriate glVertex[234][idf] function.
 ***************************************************************************/

define macro glVertex
  { glVertex(?x:expression, ?y:expression) }
    => { glVertex2(?x, ?y) }
  { glVertex(?x:expression, ?y:expression, ?z:expression) }
    => { glVertex3(?x, ?y, ?z) }
  { glVertex(?x:expression, ?y:expression, ?z:expression, ?w:expression) }
    => { glVertex4(?x, ?y, ?z, ?w) }
end macro;

define function-family-2 glVertex2
  <integer>      => glVertex2i;
  <single-float> => glVertex2f;
  <double-float> => glVertex2d;
end function-family-2;

define function-family-3 glVertex3
  <integer>      => glVertex3i;
  <single-float> => glVertex3f;
  <double-float> => glVertex3d;
end function-family-3;

define function-family-4 glVertex4
  <integer>      => glVertex4i;
  <single-float> => glVertex4f;
  <double-float> => glVertex4d;
end function-family-4;

/****************************************************************************
 * Now, suck in the system OpenGL header...
 ***************************************************************************/


define interface
  #include "GL/gl.h",
    name-mapper: minimal-name-mapping,
    exclude: {"GLvoid"},
    map: {"GLubyte*" => <byte-string>},
    equate: {"GLubyte*" => <c-string>},
    equate: {"GLvoid*" => <statically-typed-pointer>},

// Exclude stupid & annoying functions:
    exclude: {"glEdgeFlagv"},
// Exclude all the extensions:
    exclude: {"GL_CONSTANT_COLOR_EXT",
	      "GL_ONE_MINUS_CONSTANT_COLOR_EXT",
	      "GL_CONSTANT_ALPHA_EXT",
	      "GL_ONE_MINUS_CONSTANT_ALPHA_EXT",
	      "GL_BLEND_EQUATION_EXT",
	      "GL_MIN_EXT",
	      "GL_MAX_EXT",
	      "GL_FUNC_ADD_EXT",
	      "GL_FUNC_SUBTRACT_EXT",
	      "GL_FUNC_REVERSE_SUBTRACT_EXT",
	      "GL_BLEND_COLOR_EXT",
	      "GL_POLYGON_OFFSET_EXT",
	      "GL_POLYGON_OFFSET_FACTOR_EXT",
	      "GL_POLYGON_OFFSET_BIAS_EXT",
	      "GL_VERTEX_ARRAY_EXT",
	      "GL_NORMAL_ARRAY_EXT",
	      "GL_COLOR_ARRAY_EXT",
	      "GL_INDEX_ARRAY_EXT",
	      "GL_TEXTURE_COORD_ARRAY_EXT",
	      "GL_EDGE_FLAG_ARRAY_EXT",
	      "GL_VERTEX_ARRAY_SIZE_EXT",
	      "GL_VERTEX_ARRAY_TYPE_EXT",
	      "GL_VERTEX_ARRAY_STRIDE_EXT",
	      "GL_VERTEX_ARRAY_COUNT_EXT",
	      "GL_NORMAL_ARRAY_TYPE_EXT",
	      "GL_NORMAL_ARRAY_STRIDE_EXT",
	      "GL_NORMAL_ARRAY_COUNT_EXT",
	      "GL_COLOR_ARRAY_SIZE_EXT",
	      "GL_COLOR_ARRAY_TYPE_EXT",
	      "GL_COLOR_ARRAY_STRIDE_EXT",
	      "GL_COLOR_ARRAY_COUNT_EXT",
	      "GL_INDEX_ARRAY_TYPE_EXT",
	      "GL_INDEX_ARRAY_STRIDE_EXT",
	      "GL_INDEX_ARRAY_COUNT_EXT",
	      "GL_TEXTURE_COORD_ARRAY_SIZE_EXT",
	      "GL_TEXTURE_COORD_ARRAY_TYPE_EXT",
	      "GL_TEXTURE_COORD_ARRAY_STRIDE_EXT",
	      "GL_TEXTURE_COORD_ARRAY_COUNT_EXT",
	      "GL_EDGE_FLAG_ARRAY_STRIDE_EXT",
	      "GL_EDGE_FLAG_ARRAY_COUNT_EXT",
	      "GL_VERTEX_ARRAY_POINTER_EXT",
	      "GL_NORMAL_ARRAY_POINTER_EXT",
	      "GL_COLOR_ARRAY_POINTER_EXT",
	      "GL_INDEX_ARRAY_POINTER_EXT",
	      "GL_TEXTURE_COORD_ARRAY_POINTER_EXT",
	      "GL_EDGE_FLAG_ARRAY_POINTER_EXT",
	      "GL_TEXTURE_PRIORITY_EXT",
	      "GL_TEXTURE_RESIDENT_EXT",
	      "GL_TEXTURE_1D_BINDING_EXT",
	      "GL_TEXTURE_2D_BINDING_EXT",
	      "GL_PACK_SKIP_IMAGES_EXT",
	      "GL_PACK_IMAGE_HEIGHT_EXT",
	      "GL_UNPACK_SKIP_IMAGES_EXT",
	      "GL_UNPACK_IMAGE_HEIGHT_EXT",
	      "GL_TEXTURE_3D_EXT",
	      "GL_PROXY_TEXTURE_3D_EXT",
	      "GL_TEXTURE_DEPTH_EXT",
	      "GL_TEXTURE_WRAP_R_EXT",
	      "GL_MAX_3D_TEXTURE_SIZE_EXT",
	      "GL_TEXTURE_3D_BINDING_EXT",
	      "GL_TABLE_TOO_LARGE_EXT",
	      "GL_COLOR_TABLE_FORMAT_EXT",
	      "GL_COLOR_TABLE_WIDTH_EXT",
	      "GL_COLOR_TABLE_RED_SIZE_EXT",
	      "GL_COLOR_TABLE_GREEN_SIZE_EXT",
	      "GL_COLOR_TABLE_BLUE_SIZE_EXT",
	      "GL_COLOR_TABLE_ALPHA_SIZE_EXT",
	      "GL_COLOR_TABLE_LUMINANCE_SIZE_EXT",
	      "GL_COLOR_TABLE_INTENSITY_SIZE_EXT",
	      "GL_TEXTURE_INDEX_SIZE_EXT",
	      "GL_COLOR_INDEX1_EXT",
	      "GL_COLOR_INDEX2_EXT",
	      "GL_COLOR_INDEX4_EXT",
	      "GL_COLOR_INDEX8_EXT",
	      "GL_COLOR_INDEX12_EXT",
	      "GL_COLOR_INDEX16_EXT",
	      "GL_SHARED_TEXTURE_PALETTE_EXT",
	      "GL_POINT_SIZE_MIN_EXT",
	      "GL_POINT_SIZE_MAX_EXT",
	      "GL_POINT_FADE_THRESHOLD_SIZE_EXT",
	      "GL_DISTANCE_ATTENUATION_EXT",
	      "GL_RESCALE_NORMAL_EXT",
	      "GL_ABGR_EXT",
	      // "GL_SELECTED_TEXTURE_SGIS",
	      // "GL_SELECTED_TEXTURE_COORD_SET_SGIS",
	      // "GL_MAX_TEXTURES_SGIS",
	      // "GL_TEXTURE0_SGIS",
	      // "GL_TEXTURE1_SGIS",
	      // "GL_TEXTURE2_SGIS",
	      // "GL_TEXTURE3_SGIS",
	      // "GL_TEXTURE_COORD_SET_SOURCE_SGIS",
	      "GL_CLAMP_TO_EDGE_SGIS",
	      "glBlendEquationEXT",
	      "glBlendColorEXT",
	      "glPolygonOffsetEXT",
	      "glVertexPointerEXT",
	      "glNormalPointerEXT",
	      "glColorPointerEXT",
	      "glIndexPointerEXT",
	      "glTexCoordPointerEXT",
	      "glEdgeFlagPointerEXT",
	      "glGetPointervEXT",
	      "glArrayElementEXT",
	      "glDrawArraysEXT",
	      "glGenTexturesEXT",
	      "glDeleteTexturesEXT",
	      "glBindTextureEXT",
	      "glPrioritizeTexturesEXT",
	      "glAreTexturesResidentEXT",
	      "glIsTextureEXT",
	      "glTexImage3DEXT",
	      "glTexSubImage3DEXT",
	      "glCopyTexSubImage3DEXT",
	      "glColorTableEXT",
	      "glColorSubTableEXT",
	      "glGetColorTableEXT",
	      "glGetColorTableParameterfvEXT",
	      "glGetColorTableParameterivEXT",
//	      "glMultiTexCoord1dSGIS",
//	      "glMultiTexCoord1dvSGIS",
//	      "glMultiTexCoord1fSGIS",
//	      "glMultiTexCoord1fvSGIS",
//	      "glMultiTexCoord1iSGIS",
//	      "glMultiTexCoord1ivSGIS",
//	      "glMultiTexCoord1sSGIS",
//	      "glMultiTexCoord1svSGIS",
//	      "glMultiTexCoord2dSGIS",
//	      "glMultiTexCoord2dvSGIS",
//	      "glMultiTexCoord2fSGIS",
//	      "glMultiTexCoord2fvSGIS",
//	      "glMultiTexCoord2iSGIS",
//	      "glMultiTexCoord2ivSGIS",
//	      "glMultiTexCoord2sSGIS",
//	      "glMultiTexCoord2svSGIS",
//	      "glMultiTexCoord3dSGIS",
//	      "glMultiTexCoord3dvSGIS",
//	      "glMultiTexCoord3fSGIS",
//	      "glMultiTexCoord3fvSGIS",
//	      "glMultiTexCoord3iSGIS",
//	      "glMultiTexCoord3ivSGIS",
//	      "glMultiTexCoord3sSGIS",
//	      "glMultiTexCoord3svSGIS",
//	      "glMultiTexCoord4dSGIS",
//	      "glMultiTexCoord4dvSGIS",
//	      "glMultiTexCoord4fSGIS",
//	      "glMultiTexCoord4fvSGIS",
//	      "glMultiTexCoord4iSGIS",
//	      "glMultiTexCoord4ivSGIS",
//	      "glMultiTexCoord4sSGIS",
//	      "glMultiTexCoord4svSGIS",
//	      "glMultiTexCoordPointerSGIS",
//	      "glSelectTextureSGIS",
//	      "glSelectTextureCoordSetSGIS",
	      "glPointParameterfEXT",
	      "glPointParameterfvEXT",
	      "glWindowPos2iMESA",
	      "glWindowPos2sMESA",
	      "glWindowPos2fMESA",
	      "glWindowPos2dMESA",
	      "glWindowPos2ivMESA",
	      "glWindowPos2svMESA",
	      "glWindowPos2fvMESA",
	      "glWindowPos2dvMESA",
	      "glWindowPos3iMESA",
	      "glWindowPos3sMESA",
	      "glWindowPos3fMESA",
	      "glWindowPos3dMESA",
	      "glWindowPos3ivMESA",
	      "glWindowPos3svMESA",
	      "glWindowPos3fvMESA",
	      "glWindowPos3dvMESA",
	      "glWindowPos4iMESA",
	      "glWindowPos4sMESA",
	      "glWindowPos4fMESA",
	      "glWindowPos4dMESA",
	      "glWindowPos4ivMESA",
	      "glWindowPos4svMESA",
	      "glWindowPos4fvMESA",
	      "glWindowPos4dvMESA",

	      // The following paragraph contains new functions in gl.h to ignore.
	      "glResizeBuffersMESA",
	      "glNewTraceMESA",
	      "glTraceTextureMESA",
	      "glDisableTraceMESA",
	      "glTraceCommentMESA",
	      "glEndTraceMESA",
	      "glTracePointerRangeMESA",
	      "glEnableTraceMESA",
	      "glTraceListMESA",
	      "glTracePointerMESA",
	      "glTraceAssertAttribMESA",

	      "GL_EXT_blend_color",
	      "GL_EXT_blend_logic_op",
	      "GL_EXT_blend_minmax",
	      "GL_EXT_blend_subtract",
	      "GL_EXT_polygon_offset",
	      "GL_EXT_vertex_array",
	      "GL_EXT_texture_object",
	      "GL_EXT_texture3D",
	      "GL_EXT_paletted_texture",
	      "GL_EXT_shared_texture_palette",
	      "GL_EXT_point_parameters",
	      "GL_EXT_rescale_normal",
	      "GL_EXT_abgr",
	      "GL_MESA_window_pos",
	      "GL_MESA_resize_buffers"};
	      //"GL_SGIS_multitexture",
	      //"GL_SGIS_texture_edge_clamp"};

  pointer "GLdouble*" =>  <c-double-vector>, superclasses: {<c-vector>};
  pointer "GLfloat*"  =>   <c-float-vector>, superclasses: {<c-vector>};
  pointer "GLint*"    => <c-integer-vector>, superclasses: {<c-vector>};
  pointer "GLubyte*"  => <c-byte-vector>,    superclasses: {<c-vector>};

  function "glAccum"       => glAccum-internal;
  function "glAlphaFunc"   => glAlphaFunc-internal;
  function "glBegin"       => glBegin-internal;
  function "glBindTexture" => glBindTexture-internal;
  function "glBlendFunc"   => glBlendFunc-internal;
  function "glColorMaterial"   => glColorMaterial-internal;
  function "glCopyPixels"   => glCopyPixels-internal;
  function "glCopyTexImage1D"   => glCopyTexImage1D-internal;
  function "glCopyTexImage2D"   => glCopyTexImage2D-internal;
  function "glCopyTexSubImage1D"   => glCopyTexSubImage1D-internal;
  function "glCopyTexSubImage2D"   => glCopyTexSubImage2D-internal;
  function "glGenTextures", output-argument: 2;
end interface;

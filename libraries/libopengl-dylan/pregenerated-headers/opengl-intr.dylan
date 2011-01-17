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
			       min: $GL-LIGHT0, 
			       max: $GL-LIGHT0 + $GL-MAX-LIGHTS),
			      pname :: <GLenum>, 
			      arg1 :: <number>, #rest rest);


define method glLight(light :: limited(<integer>, 
			       min: $GL-LIGHT0, 
			       max: $GL-LIGHT0 + $GL-MAX-LIGHTS),
		      pname :: <GLenum>,
		      arg1 :: <integer>, #rest rest) => ()

  // FIXME Add arg check in debug mode here.
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
			       min: $GL-LIGHT0, 
			       max: $GL-LIGHT0 + $GL-MAX-LIGHTS),
		      pname :: <GLenum>,
		      arg1 :: <float>, #rest rest) => ()

  // FIXME Add arg check in debug mode here.
  if (empty?(rest))
    glLightf($GL-LIGHT0 + light, pname, arg1);
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

  // FIXME Add arg check in debug mode here.
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

c-include("/usr/X11R6/include/GL/gl.h");

define constant <GLenum> = <integer>;

define constant <GLboolean> = <integer>;

define constant <GLbitfield> = <integer>;

define constant <GLbyte> = <integer>;

define constant <GLshort> = <integer>;

define constant <GLint> = <integer>;

define constant <GLubyte> = <integer>;

define constant <GLushort> = <integer>;

define constant <GLuint> = <integer>;

define constant <GLsizei> = <integer>;

define constant <GLfloat> = <single-float>;

define constant <GLclampf> = <single-float>;

define constant <GLdouble> = <double-float>;

define constant <GLclampd> = <double-float>;

define function glClearIndex
    (arg1 :: <GLfloat>)
 => ();
  call-out("glClearIndex", void:, float:, arg1);
  values();
end function glClearIndex;

define function glClearColor
    (arg1 :: <GLclampf>, arg2 :: <GLclampf>, arg3 :: <GLclampf>, arg4 :: <GLclampf>)
 => ();
  call-out("glClearColor", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glClearColor;

define function glClear
    (arg1 :: <GLbitfield>)
 => ();
  call-out("glClear", void:, unsigned-int:, arg1);
  values();
end function glClear;

define function glIndexMask
    (arg1 :: <GLuint>)
 => ();
  call-out("glIndexMask", void:, unsigned-int:, arg1);
  values();
end function glIndexMask;

define function glColorMask
    (arg1 :: <GLboolean>, arg2 :: <GLboolean>, arg3 :: <GLboolean>, arg4 :: <GLboolean>)
 => ();
  call-out("glColorMask", void:, unsigned-char:, arg1, unsigned-char:, arg2, unsigned-char:, arg3, unsigned-char:, arg4);
  values();
end function glColorMask;

define function glAlphaFunc-internal
    (arg1 :: <GLenum>, arg2 :: <GLclampf>)
 => ();
  call-out("glAlphaFunc", void:, unsigned-int:, arg1, float:, arg2);
  values();
end function glAlphaFunc-internal;

define function glBlendFunc-internal
    (arg1 :: <GLenum>, arg2 :: <GLenum>)
 => ();
  call-out("glBlendFunc", void:, unsigned-int:, arg1, unsigned-int:, arg2);
  values();
end function glBlendFunc-internal;

define function glLogicOp
    (arg1 :: <GLenum>)
 => ();
  call-out("glLogicOp", void:, unsigned-int:, arg1);
  values();
end function glLogicOp;

define function glCullFace
    (arg1 :: <GLenum>)
 => ();
  call-out("glCullFace", void:, unsigned-int:, arg1);
  values();
end function glCullFace;

define function glFrontFace
    (arg1 :: <GLenum>)
 => ();
  call-out("glFrontFace", void:, unsigned-int:, arg1);
  values();
end function glFrontFace;

define function glPointSize
    (arg1 :: <GLfloat>)
 => ();
  call-out("glPointSize", void:, float:, arg1);
  values();
end function glPointSize;

define function glLineWidth
    (arg1 :: <GLfloat>)
 => ();
  call-out("glLineWidth", void:, float:, arg1);
  values();
end function glLineWidth;

define function glLineStipple
    (arg1 :: <GLint>, arg2 :: <GLushort>)
 => ();
  call-out("glLineStipple", void:, int:, arg1, unsigned-short:, arg2);
  values();
end function glLineStipple;

define function glPolygonMode
    (arg1 :: <GLenum>, arg2 :: <GLenum>)
 => ();
  call-out("glPolygonMode", void:, unsigned-int:, arg1, unsigned-int:, arg2);
  values();
end function glPolygonMode;

define function glPolygonOffset
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>)
 => ();
  call-out("glPolygonOffset", void:, float:, arg1, float:, arg2);
  values();
end function glPolygonOffset;

define function glPolygonStipple
    (arg1 :: <byte-string>)
 => ();
  call-out("glPolygonStipple", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glPolygonStipple;

define function glGetPolygonStipple
    (arg1 :: <byte-string>)
 => ();
  call-out("glGetPolygonStipple", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glGetPolygonStipple;

define function glEdgeFlag
    (arg1 :: <GLboolean>)
 => ();
  call-out("glEdgeFlag", void:, unsigned-char:, arg1);
  values();
end function glEdgeFlag;

define function glScissor
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLsizei>)
 => ();
  call-out("glScissor", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glScissor;

define functional class <c-double-vector> (<c-vector>, <statically-typed-pointer>) end;

define sealed domain make (singleton(<c-double-vector>));

define inline method pointer-value
    (ptr :: <c-double-vector>, #key index = 0)
 => (result :: <GLdouble>);
  double-at(ptr, offset: index * 8);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLdouble>, ptr :: <c-double-vector>, #key index = 0)
 => (result :: <GLdouble>);
  double-at(ptr, offset: index * 8) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<c-double-vector>)) => (result :: <integer>);
  8;
end method content-size;

define function glClipPlane
    (arg1 :: <GLenum>, arg2 :: <c-double-vector>)
 => ();
  call-out("glClipPlane", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glClipPlane;

define function glGetClipPlane
    (arg1 :: <GLenum>, arg2 :: <c-double-vector>)
 => ();
  call-out("glGetClipPlane", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetClipPlane;

define function glDrawBuffer
    (arg1 :: <GLenum>)
 => ();
  call-out("glDrawBuffer", void:, unsigned-int:, arg1);
  values();
end function glDrawBuffer;

define function glReadBuffer
    (arg1 :: <GLenum>)
 => ();
  call-out("glReadBuffer", void:, unsigned-int:, arg1);
  values();
end function glReadBuffer;

define function glEnable
    (arg1 :: <GLenum>)
 => ();
  call-out("glEnable", void:, unsigned-int:, arg1);
  values();
end function glEnable;

define function glDisable
    (arg1 :: <GLenum>)
 => ();
  call-out("glDisable", void:, unsigned-int:, arg1);
  values();
end function glDisable;

define function glIsEnabled
    (arg1 :: <GLenum>)
 => (result :: <GLboolean>);
  let result-value
    = call-out("glIsEnabled", unsigned-char:, unsigned-int:, arg1);
  values(result-value);
end function glIsEnabled;

define function glEnableClientState
    (arg1 :: <GLenum>)
 => ();
  call-out("glEnableClientState", void:, unsigned-int:, arg1);
  values();
end function glEnableClientState;

define function glDisableClientState
    (arg1 :: <GLenum>)
 => ();
  call-out("glDisableClientState", void:, unsigned-int:, arg1);
  values();
end function glDisableClientState;

define functional class <GLboolean*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLboolean*>));

define inline method pointer-value
    (ptr :: <GLboolean*>, #key index = 0)
 => (result :: <GLboolean>);
  unsigned-byte-at(ptr, offset: index * 1);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLboolean>, ptr :: <GLboolean*>, #key index = 0)
 => (result :: <GLboolean>);
  unsigned-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLboolean*>)) => (result :: <integer>);
  1;
end method content-size;

define function glGetBooleanv
    (arg1 :: <GLenum>, arg2 :: <GLboolean*>)
 => ();
  call-out("glGetBooleanv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetBooleanv;

define function glGetDoublev
    (arg1 :: <GLenum>, arg2 :: <c-double-vector>)
 => ();
  call-out("glGetDoublev", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetDoublev;

define functional class <c-float-vector> (<c-vector>, <statically-typed-pointer>) end;

define sealed domain make (singleton(<c-float-vector>));

define inline method pointer-value
    (ptr :: <c-float-vector>, #key index = 0)
 => (result :: <GLfloat>);
  float-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLfloat>, ptr :: <c-float-vector>, #key index = 0)
 => (result :: <GLfloat>);
  float-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<c-float-vector>)) => (result :: <integer>);
  4;
end method content-size;

define function glGetFloatv
    (arg1 :: <GLenum>, arg2 :: <c-float-vector>)
 => ();
  call-out("glGetFloatv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetFloatv;

define functional class <c-integer-vector> (<c-vector>, <statically-typed-pointer>) end;

define sealed domain make (singleton(<c-integer-vector>));

define inline method pointer-value
    (ptr :: <c-integer-vector>, #key index = 0)
 => (result :: <GLint>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLint>, ptr :: <c-integer-vector>, #key index = 0)
 => (result :: <GLint>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<c-integer-vector>)) => (result :: <integer>);
  4;
end method content-size;

define function glGetIntegerv
    (arg1 :: <GLenum>, arg2 :: <c-integer-vector>)
 => ();
  call-out("glGetIntegerv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetIntegerv;

define function glPushAttrib
    (arg1 :: <GLbitfield>)
 => ();
  call-out("glPushAttrib", void:, unsigned-int:, arg1);
  values();
end function glPushAttrib;

define function glPopAttrib
    ()
 => ();
  call-out("glPopAttrib", void:);
  values();
end function glPopAttrib;

define function glPushClientAttrib
    (arg1 :: <GLbitfield>)
 => ();
  call-out("glPushClientAttrib", void:, unsigned-int:, arg1);
  values();
end function glPushClientAttrib;

define function glPopClientAttrib
    ()
 => ();
  call-out("glPopClientAttrib", void:);
  values();
end function glPopClientAttrib;

define function glRenderMode
    (arg1 :: <GLenum>)
 => (result :: <GLint>);
  let result-value
    = call-out("glRenderMode", int:, unsigned-int:, arg1);
  values(result-value);
end function glRenderMode;

define function glGetError
    ()
 => (result :: <GLenum>);
  let result-value
    = call-out("glGetError", unsigned-int:);
  values(result-value);
end function glGetError;

define function glGetString
    (arg1 :: <GLenum>)
 => (result :: <byte-string>);
  let result-value
    = call-out("glGetString", ptr:, unsigned-int:, arg1);
  let result-value = make(<c-string>, pointer: result-value);
  values(import-value(<byte-string>, result-value));
end function glGetString;

define function glFinish
    ()
 => ();
  call-out("glFinish", void:);
  values();
end function glFinish;

define function glFlush
    ()
 => ();
  call-out("glFlush", void:);
  values();
end function glFlush;

define function glHint
    (arg1 :: <GLenum>, arg2 :: <GLenum>)
 => ();
  call-out("glHint", void:, unsigned-int:, arg1, unsigned-int:, arg2);
  values();
end function glHint;

define function glClearDepth
    (arg1 :: <GLclampd>)
 => ();
  call-out("glClearDepth", void:, double:, arg1);
  values();
end function glClearDepth;

define function glDepthFunc
    (arg1 :: <GLenum>)
 => ();
  call-out("glDepthFunc", void:, unsigned-int:, arg1);
  values();
end function glDepthFunc;

define function glDepthMask
    (arg1 :: <GLboolean>)
 => ();
  call-out("glDepthMask", void:, unsigned-char:, arg1);
  values();
end function glDepthMask;

define function glDepthRange
    (arg1 :: <GLclampd>, arg2 :: <GLclampd>)
 => ();
  call-out("glDepthRange", void:, double:, arg1, double:, arg2);
  values();
end function glDepthRange;

define function glClearAccum
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glClearAccum", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glClearAccum;

define function glAccum-internal
    (arg1 :: <GLenum>, arg2 :: <GLfloat>)
 => ();
  call-out("glAccum", void:, unsigned-int:, arg1, float:, arg2);
  values();
end function glAccum-internal;

define function glMatrixMode
    (arg1 :: <GLenum>)
 => ();
  call-out("glMatrixMode", void:, unsigned-int:, arg1);
  values();
end function glMatrixMode;

define function glOrtho
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>, arg5 :: <GLdouble>, arg6 :: <GLdouble>)
 => ();
  call-out("glOrtho", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4, double:, arg5, double:, arg6);
  values();
end function glOrtho;

define function glFrustum
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>, arg5 :: <GLdouble>, arg6 :: <GLdouble>)
 => ();
  call-out("glFrustum", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4, double:, arg5, double:, arg6);
  values();
end function glFrustum;

define function glViewport
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLsizei>)
 => ();
  call-out("glViewport", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glViewport;

define function glPushMatrix
    ()
 => ();
  call-out("glPushMatrix", void:);
  values();
end function glPushMatrix;

define function glPopMatrix
    ()
 => ();
  call-out("glPopMatrix", void:);
  values();
end function glPopMatrix;

define function glLoadIdentity
    ()
 => ();
  call-out("glLoadIdentity", void:);
  values();
end function glLoadIdentity;

define function glLoadMatrixd
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glLoadMatrixd", void:, ptr:, (arg1).raw-value);
  values();
end function glLoadMatrixd;

define function glLoadMatrixf
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glLoadMatrixf", void:, ptr:, (arg1).raw-value);
  values();
end function glLoadMatrixf;

define function glMultMatrixd
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glMultMatrixd", void:, ptr:, (arg1).raw-value);
  values();
end function glMultMatrixd;

define function glMultMatrixf
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glMultMatrixf", void:, ptr:, (arg1).raw-value);
  values();
end function glMultMatrixf;

define function glRotated
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("glRotated", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function glRotated;

define function glRotatef
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glRotatef", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glRotatef;

define function glScaled
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glScaled", void:, double:, arg1, double:, arg2, double:, arg3);
  values();
end function glScaled;

define function glScalef
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glScalef", void:, float:, arg1, float:, arg2, float:, arg3);
  values();
end function glScalef;

define function glTranslated
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glTranslated", void:, double:, arg1, double:, arg2, double:, arg3);
  values();
end function glTranslated;

define function glTranslatef
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glTranslatef", void:, float:, arg1, float:, arg2, float:, arg3);
  values();
end function glTranslatef;

define function glIsList
    (arg1 :: <GLuint>)
 => (result :: <GLboolean>);
  let result-value
    = call-out("glIsList", unsigned-char:, unsigned-int:, arg1);
  values(result-value);
end function glIsList;

define function glDeleteLists
    (arg1 :: <GLuint>, arg2 :: <GLsizei>)
 => ();
  call-out("glDeleteLists", void:, unsigned-int:, arg1, int:, arg2);
  values();
end function glDeleteLists;

define function glGenLists
    (arg1 :: <GLsizei>)
 => (result :: <GLuint>);
  let result-value
    = call-out("glGenLists", unsigned-int:, int:, arg1);
  values(result-value);
end function glGenLists;

define function glNewList
    (arg1 :: <GLuint>, arg2 :: <GLenum>)
 => ();
  call-out("glNewList", void:, unsigned-int:, arg1, unsigned-int:, arg2);
  values();
end function glNewList;

define function glEndList
    ()
 => ();
  call-out("glEndList", void:);
  values();
end function glEndList;

define function glCallList
    (arg1 :: <GLuint>)
 => ();
  call-out("glCallList", void:, unsigned-int:, arg1);
  values();
end function glCallList;

define function glCallLists
    (arg1 :: <GLsizei>, arg2 :: <GLenum>, arg3 :: <statically-typed-pointer>)
 => ();
  call-out("glCallLists", void:, int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glCallLists;

define function glListBase
    (arg1 :: <GLuint>)
 => ();
  call-out("glListBase", void:, unsigned-int:, arg1);
  values();
end function glListBase;

define function glBegin-internal
    (arg1 :: <GLenum>)
 => ();
  call-out("glBegin", void:, unsigned-int:, arg1);
  values();
end function glBegin-internal;

define function glEnd
    ()
 => ();
  call-out("glEnd", void:);
  values();
end function glEnd;

define function glVertex2d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>)
 => ();
  call-out("glVertex2d", void:, double:, arg1, double:, arg2);
  values();
end function glVertex2d;

define function glVertex2f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>)
 => ();
  call-out("glVertex2f", void:, float:, arg1, float:, arg2);
  values();
end function glVertex2f;

define function glVertex2i
    (arg1 :: <GLint>, arg2 :: <GLint>)
 => ();
  call-out("glVertex2i", void:, int:, arg1, int:, arg2);
  values();
end function glVertex2i;

define function glVertex2s
    (arg1 :: <GLshort>, arg2 :: <GLshort>)
 => ();
  call-out("glVertex2s", void:, short:, arg1, short:, arg2);
  values();
end function glVertex2s;

define function glVertex3d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glVertex3d", void:, double:, arg1, double:, arg2, double:, arg3);
  values();
end function glVertex3d;

define function glVertex3f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glVertex3f", void:, float:, arg1, float:, arg2, float:, arg3);
  values();
end function glVertex3f;

define function glVertex3i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glVertex3i", void:, int:, arg1, int:, arg2, int:, arg3);
  values();
end function glVertex3i;

define function glVertex3s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>)
 => ();
  call-out("glVertex3s", void:, short:, arg1, short:, arg2, short:, arg3);
  values();
end function glVertex3s;

define function glVertex4d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("glVertex4d", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function glVertex4d;

define function glVertex4f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glVertex4f", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glVertex4f;

define function glVertex4i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glVertex4i", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glVertex4i;

define function glVertex4s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>, arg4 :: <GLshort>)
 => ();
  call-out("glVertex4s", void:, short:, arg1, short:, arg2, short:, arg3, short:, arg4);
  values();
end function glVertex4s;

define function glVertex2dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glVertex2dv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex2dv;

define function glVertex2fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glVertex2fv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex2fv;

define function glVertex2iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glVertex2iv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex2iv;

define functional class <GLshort*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLshort*>));

define inline method pointer-value
    (ptr :: <GLshort*>, #key index = 0)
 => (result :: <GLshort>);
  signed-short-at(ptr, offset: index * 2);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLshort>, ptr :: <GLshort*>, #key index = 0)
 => (result :: <GLshort>);
  signed-short-at(ptr, offset: index * 2) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLshort*>)) => (result :: <integer>);
  2;
end method content-size;

define function glVertex2sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glVertex2sv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex2sv;

define function glVertex3dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glVertex3dv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex3dv;

define function glVertex3fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glVertex3fv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex3fv;

define function glVertex3iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glVertex3iv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex3iv;

define function glVertex3sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glVertex3sv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex3sv;

define function glVertex4dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glVertex4dv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex4dv;

define function glVertex4fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glVertex4fv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex4fv;

define function glVertex4iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glVertex4iv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex4iv;

define function glVertex4sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glVertex4sv", void:, ptr:, (arg1).raw-value);
  values();
end function glVertex4sv;

define function glNormal3b
    (arg1 :: <GLbyte>, arg2 :: <GLbyte>, arg3 :: <GLbyte>)
 => ();
  call-out("glNormal3b", void:, char:, arg1, char:, arg2, char:, arg3);
  values();
end function glNormal3b;

define function glNormal3d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glNormal3d", void:, double:, arg1, double:, arg2, double:, arg3);
  values();
end function glNormal3d;

define function glNormal3f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glNormal3f", void:, float:, arg1, float:, arg2, float:, arg3);
  values();
end function glNormal3f;

define function glNormal3i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glNormal3i", void:, int:, arg1, int:, arg2, int:, arg3);
  values();
end function glNormal3i;

define function glNormal3s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>)
 => ();
  call-out("glNormal3s", void:, short:, arg1, short:, arg2, short:, arg3);
  values();
end function glNormal3s;

define functional class <GLbyte*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLbyte*>));

define inline method pointer-value
    (ptr :: <GLbyte*>, #key index = 0)
 => (result :: <GLbyte>);
  signed-byte-at(ptr, offset: index * 1);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLbyte>, ptr :: <GLbyte*>, #key index = 0)
 => (result :: <GLbyte>);
  signed-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLbyte*>)) => (result :: <integer>);
  1;
end method content-size;

define function glNormal3bv
    (arg1 :: <GLbyte*>)
 => ();
  call-out("glNormal3bv", void:, ptr:, (arg1).raw-value);
  values();
end function glNormal3bv;

define function glNormal3dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glNormal3dv", void:, ptr:, (arg1).raw-value);
  values();
end function glNormal3dv;

define function glNormal3fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glNormal3fv", void:, ptr:, (arg1).raw-value);
  values();
end function glNormal3fv;

define function glNormal3iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glNormal3iv", void:, ptr:, (arg1).raw-value);
  values();
end function glNormal3iv;

define function glNormal3sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glNormal3sv", void:, ptr:, (arg1).raw-value);
  values();
end function glNormal3sv;

define function glIndexd
    (arg1 :: <GLdouble>)
 => ();
  call-out("glIndexd", void:, double:, arg1);
  values();
end function glIndexd;

define function glIndexf
    (arg1 :: <GLfloat>)
 => ();
  call-out("glIndexf", void:, float:, arg1);
  values();
end function glIndexf;

define function glIndexi
    (arg1 :: <GLint>)
 => ();
  call-out("glIndexi", void:, int:, arg1);
  values();
end function glIndexi;

define function glIndexs
    (arg1 :: <GLshort>)
 => ();
  call-out("glIndexs", void:, short:, arg1);
  values();
end function glIndexs;

define function glIndexub
    (arg1 :: <GLubyte>)
 => ();
  call-out("glIndexub", void:, unsigned-char:, arg1);
  values();
end function glIndexub;

define function glIndexdv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glIndexdv", void:, ptr:, (arg1).raw-value);
  values();
end function glIndexdv;

define function glIndexfv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glIndexfv", void:, ptr:, (arg1).raw-value);
  values();
end function glIndexfv;

define function glIndexiv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glIndexiv", void:, ptr:, (arg1).raw-value);
  values();
end function glIndexiv;

define function glIndexsv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glIndexsv", void:, ptr:, (arg1).raw-value);
  values();
end function glIndexsv;

define function glIndexubv
    (arg1 :: <byte-string>)
 => ();
  call-out("glIndexubv", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glIndexubv;

define function glColor3b
    (arg1 :: <GLbyte>, arg2 :: <GLbyte>, arg3 :: <GLbyte>)
 => ();
  call-out("glColor3b", void:, char:, arg1, char:, arg2, char:, arg3);
  values();
end function glColor3b;

define function glColor3d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glColor3d", void:, double:, arg1, double:, arg2, double:, arg3);
  values();
end function glColor3d;

define function glColor3f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glColor3f", void:, float:, arg1, float:, arg2, float:, arg3);
  values();
end function glColor3f;

define function glColor3i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glColor3i", void:, int:, arg1, int:, arg2, int:, arg3);
  values();
end function glColor3i;

define function glColor3s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>)
 => ();
  call-out("glColor3s", void:, short:, arg1, short:, arg2, short:, arg3);
  values();
end function glColor3s;

define function glColor3ub
    (arg1 :: <GLubyte>, arg2 :: <GLubyte>, arg3 :: <GLubyte>)
 => ();
  call-out("glColor3ub", void:, unsigned-char:, arg1, unsigned-char:, arg2, unsigned-char:, arg3);
  values();
end function glColor3ub;

define function glColor3ui
    (arg1 :: <GLuint>, arg2 :: <GLuint>, arg3 :: <GLuint>)
 => ();
  call-out("glColor3ui", void:, unsigned-int:, arg1, unsigned-int:, arg2, unsigned-int:, arg3);
  values();
end function glColor3ui;

define function glColor3us
    (arg1 :: <GLushort>, arg2 :: <GLushort>, arg3 :: <GLushort>)
 => ();
  call-out("glColor3us", void:, unsigned-short:, arg1, unsigned-short:, arg2, unsigned-short:, arg3);
  values();
end function glColor3us;

define function glColor4b
    (arg1 :: <GLbyte>, arg2 :: <GLbyte>, arg3 :: <GLbyte>, arg4 :: <GLbyte>)
 => ();
  call-out("glColor4b", void:, char:, arg1, char:, arg2, char:, arg3, char:, arg4);
  values();
end function glColor4b;

define function glColor4d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("glColor4d", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function glColor4d;

define function glColor4f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glColor4f", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glColor4f;

define function glColor4i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glColor4i", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glColor4i;

define function glColor4s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>, arg4 :: <GLshort>)
 => ();
  call-out("glColor4s", void:, short:, arg1, short:, arg2, short:, arg3, short:, arg4);
  values();
end function glColor4s;

define function glColor4ub
    (arg1 :: <GLubyte>, arg2 :: <GLubyte>, arg3 :: <GLubyte>, arg4 :: <GLubyte>)
 => ();
  call-out("glColor4ub", void:, unsigned-char:, arg1, unsigned-char:, arg2, unsigned-char:, arg3, unsigned-char:, arg4);
  values();
end function glColor4ub;

define function glColor4ui
    (arg1 :: <GLuint>, arg2 :: <GLuint>, arg3 :: <GLuint>, arg4 :: <GLuint>)
 => ();
  call-out("glColor4ui", void:, unsigned-int:, arg1, unsigned-int:, arg2, unsigned-int:, arg3, unsigned-int:, arg4);
  values();
end function glColor4ui;

define function glColor4us
    (arg1 :: <GLushort>, arg2 :: <GLushort>, arg3 :: <GLushort>, arg4 :: <GLushort>)
 => ();
  call-out("glColor4us", void:, unsigned-short:, arg1, unsigned-short:, arg2, unsigned-short:, arg3, unsigned-short:, arg4);
  values();
end function glColor4us;

define function glColor3bv
    (arg1 :: <GLbyte*>)
 => ();
  call-out("glColor3bv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor3bv;

define function glColor3dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glColor3dv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor3dv;

define function glColor3fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glColor3fv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor3fv;

define function glColor3iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glColor3iv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor3iv;

define function glColor3sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glColor3sv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor3sv;

define function glColor3ubv
    (arg1 :: <byte-string>)
 => ();
  call-out("glColor3ubv", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glColor3ubv;

define functional class <GLuint*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLuint*>));

define inline method pointer-value
    (ptr :: <GLuint*>, #key index = 0)
 => (result :: <GLuint>);
  unsigned-long-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLuint>, ptr :: <GLuint*>, #key index = 0)
 => (result :: <GLuint>);
  unsigned-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLuint*>)) => (result :: <integer>);
  4;
end method content-size;

define function glColor3uiv
    (arg1 :: <GLuint*>)
 => ();
  call-out("glColor3uiv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor3uiv;

define functional class <GLushort*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLushort*>));

define inline method pointer-value
    (ptr :: <GLushort*>, #key index = 0)
 => (result :: <GLushort>);
  unsigned-short-at(ptr, offset: index * 2);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLushort>, ptr :: <GLushort*>, #key index = 0)
 => (result :: <GLushort>);
  unsigned-short-at(ptr, offset: index * 2) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLushort*>)) => (result :: <integer>);
  2;
end method content-size;

define function glColor3usv
    (arg1 :: <GLushort*>)
 => ();
  call-out("glColor3usv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor3usv;

define function glColor4bv
    (arg1 :: <GLbyte*>)
 => ();
  call-out("glColor4bv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor4bv;

define function glColor4dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glColor4dv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor4dv;

define function glColor4fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glColor4fv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor4fv;

define function glColor4iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glColor4iv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor4iv;

define function glColor4sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glColor4sv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor4sv;

define function glColor4ubv
    (arg1 :: <byte-string>)
 => ();
  call-out("glColor4ubv", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glColor4ubv;

define function glColor4uiv
    (arg1 :: <GLuint*>)
 => ();
  call-out("glColor4uiv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor4uiv;

define function glColor4usv
    (arg1 :: <GLushort*>)
 => ();
  call-out("glColor4usv", void:, ptr:, (arg1).raw-value);
  values();
end function glColor4usv;

define function glTexCoord1d
    (arg1 :: <GLdouble>)
 => ();
  call-out("glTexCoord1d", void:, double:, arg1);
  values();
end function glTexCoord1d;

define function glTexCoord1f
    (arg1 :: <GLfloat>)
 => ();
  call-out("glTexCoord1f", void:, float:, arg1);
  values();
end function glTexCoord1f;

define function glTexCoord1i
    (arg1 :: <GLint>)
 => ();
  call-out("glTexCoord1i", void:, int:, arg1);
  values();
end function glTexCoord1i;

define function glTexCoord1s
    (arg1 :: <GLshort>)
 => ();
  call-out("glTexCoord1s", void:, short:, arg1);
  values();
end function glTexCoord1s;

define function glTexCoord2d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>)
 => ();
  call-out("glTexCoord2d", void:, double:, arg1, double:, arg2);
  values();
end function glTexCoord2d;

define function glTexCoord2f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>)
 => ();
  call-out("glTexCoord2f", void:, float:, arg1, float:, arg2);
  values();
end function glTexCoord2f;

define function glTexCoord2i
    (arg1 :: <GLint>, arg2 :: <GLint>)
 => ();
  call-out("glTexCoord2i", void:, int:, arg1, int:, arg2);
  values();
end function glTexCoord2i;

define function glTexCoord2s
    (arg1 :: <GLshort>, arg2 :: <GLshort>)
 => ();
  call-out("glTexCoord2s", void:, short:, arg1, short:, arg2);
  values();
end function glTexCoord2s;

define function glTexCoord3d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glTexCoord3d", void:, double:, arg1, double:, arg2, double:, arg3);
  values();
end function glTexCoord3d;

define function glTexCoord3f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glTexCoord3f", void:, float:, arg1, float:, arg2, float:, arg3);
  values();
end function glTexCoord3f;

define function glTexCoord3i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glTexCoord3i", void:, int:, arg1, int:, arg2, int:, arg3);
  values();
end function glTexCoord3i;

define function glTexCoord3s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>)
 => ();
  call-out("glTexCoord3s", void:, short:, arg1, short:, arg2, short:, arg3);
  values();
end function glTexCoord3s;

define function glTexCoord4d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("glTexCoord4d", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function glTexCoord4d;

define function glTexCoord4f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glTexCoord4f", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glTexCoord4f;

define function glTexCoord4i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glTexCoord4i", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glTexCoord4i;

define function glTexCoord4s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>, arg4 :: <GLshort>)
 => ();
  call-out("glTexCoord4s", void:, short:, arg1, short:, arg2, short:, arg3, short:, arg4);
  values();
end function glTexCoord4s;

define function glTexCoord1dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glTexCoord1dv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord1dv;

define function glTexCoord1fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glTexCoord1fv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord1fv;

define function glTexCoord1iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glTexCoord1iv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord1iv;

define function glTexCoord1sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glTexCoord1sv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord1sv;

define function glTexCoord2dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glTexCoord2dv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord2dv;

define function glTexCoord2fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glTexCoord2fv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord2fv;

define function glTexCoord2iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glTexCoord2iv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord2iv;

define function glTexCoord2sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glTexCoord2sv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord2sv;

define function glTexCoord3dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glTexCoord3dv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord3dv;

define function glTexCoord3fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glTexCoord3fv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord3fv;

define function glTexCoord3iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glTexCoord3iv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord3iv;

define function glTexCoord3sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glTexCoord3sv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord3sv;

define function glTexCoord4dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glTexCoord4dv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord4dv;

define function glTexCoord4fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glTexCoord4fv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord4fv;

define function glTexCoord4iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glTexCoord4iv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord4iv;

define function glTexCoord4sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glTexCoord4sv", void:, ptr:, (arg1).raw-value);
  values();
end function glTexCoord4sv;

define function glRasterPos2d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>)
 => ();
  call-out("glRasterPos2d", void:, double:, arg1, double:, arg2);
  values();
end function glRasterPos2d;

define function glRasterPos2f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>)
 => ();
  call-out("glRasterPos2f", void:, float:, arg1, float:, arg2);
  values();
end function glRasterPos2f;

define function glRasterPos2i
    (arg1 :: <GLint>, arg2 :: <GLint>)
 => ();
  call-out("glRasterPos2i", void:, int:, arg1, int:, arg2);
  values();
end function glRasterPos2i;

define function glRasterPos2s
    (arg1 :: <GLshort>, arg2 :: <GLshort>)
 => ();
  call-out("glRasterPos2s", void:, short:, arg1, short:, arg2);
  values();
end function glRasterPos2s;

define function glRasterPos3d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glRasterPos3d", void:, double:, arg1, double:, arg2, double:, arg3);
  values();
end function glRasterPos3d;

define function glRasterPos3f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glRasterPos3f", void:, float:, arg1, float:, arg2, float:, arg3);
  values();
end function glRasterPos3f;

define function glRasterPos3i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glRasterPos3i", void:, int:, arg1, int:, arg2, int:, arg3);
  values();
end function glRasterPos3i;

define function glRasterPos3s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>)
 => ();
  call-out("glRasterPos3s", void:, short:, arg1, short:, arg2, short:, arg3);
  values();
end function glRasterPos3s;

define function glRasterPos4d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("glRasterPos4d", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function glRasterPos4d;

define function glRasterPos4f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glRasterPos4f", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glRasterPos4f;

define function glRasterPos4i
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glRasterPos4i", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glRasterPos4i;

define function glRasterPos4s
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>, arg4 :: <GLshort>)
 => ();
  call-out("glRasterPos4s", void:, short:, arg1, short:, arg2, short:, arg3, short:, arg4);
  values();
end function glRasterPos4s;

define function glRasterPos2dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glRasterPos2dv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos2dv;

define function glRasterPos2fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glRasterPos2fv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos2fv;

define function glRasterPos2iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glRasterPos2iv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos2iv;

define function glRasterPos2sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glRasterPos2sv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos2sv;

define function glRasterPos3dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glRasterPos3dv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos3dv;

define function glRasterPos3fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glRasterPos3fv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos3fv;

define function glRasterPos3iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glRasterPos3iv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos3iv;

define function glRasterPos3sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glRasterPos3sv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos3sv;

define function glRasterPos4dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glRasterPos4dv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos4dv;

define function glRasterPos4fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glRasterPos4fv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos4fv;

define function glRasterPos4iv
    (arg1 :: <c-integer-vector>)
 => ();
  call-out("glRasterPos4iv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos4iv;

define function glRasterPos4sv
    (arg1 :: <GLshort*>)
 => ();
  call-out("glRasterPos4sv", void:, ptr:, (arg1).raw-value);
  values();
end function glRasterPos4sv;

define function glRectd
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("glRectd", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function glRectd;

define function glRectf
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glRectf", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glRectf;

define function glRecti
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glRecti", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glRecti;

define function glRects
    (arg1 :: <GLshort>, arg2 :: <GLshort>, arg3 :: <GLshort>, arg4 :: <GLshort>)
 => ();
  call-out("glRects", void:, short:, arg1, short:, arg2, short:, arg3, short:, arg4);
  values();
end function glRects;

define function glRectdv
    (arg1 :: <c-double-vector>, arg2 :: <c-double-vector>)
 => ();
  call-out("glRectdv", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values();
end function glRectdv;

define function glRectfv
    (arg1 :: <c-float-vector>, arg2 :: <c-float-vector>)
 => ();
  call-out("glRectfv", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values();
end function glRectfv;

define function glRectiv
    (arg1 :: <c-integer-vector>, arg2 :: <c-integer-vector>)
 => ();
  call-out("glRectiv", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values();
end function glRectiv;

define function glRectsv
    (arg1 :: <GLshort*>, arg2 :: <GLshort*>)
 => ();
  call-out("glRectsv", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values();
end function glRectsv;

define function glVertexPointer
    (arg1 :: <GLint>, arg2 :: <GLenum>, arg3 :: <GLsizei>, arg4 :: <statically-typed-pointer>)
 => ();
  call-out("glVertexPointer", void:, int:, arg1, unsigned-int:, arg2, int:, arg3, ptr:, (arg4).raw-value);
  values();
end function glVertexPointer;

define function glNormalPointer
    (arg1 :: <GLenum>, arg2 :: <GLsizei>, arg3 :: <statically-typed-pointer>)
 => ();
  call-out("glNormalPointer", void:, unsigned-int:, arg1, int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glNormalPointer;

define function glColorPointer
    (arg1 :: <GLint>, arg2 :: <GLenum>, arg3 :: <GLsizei>, arg4 :: <statically-typed-pointer>)
 => ();
  call-out("glColorPointer", void:, int:, arg1, unsigned-int:, arg2, int:, arg3, ptr:, (arg4).raw-value);
  values();
end function glColorPointer;

define function glIndexPointer
    (arg1 :: <GLenum>, arg2 :: <GLsizei>, arg3 :: <statically-typed-pointer>)
 => ();
  call-out("glIndexPointer", void:, unsigned-int:, arg1, int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glIndexPointer;

define function glTexCoordPointer
    (arg1 :: <GLint>, arg2 :: <GLenum>, arg3 :: <GLsizei>, arg4 :: <statically-typed-pointer>)
 => ();
  call-out("glTexCoordPointer", void:, int:, arg1, unsigned-int:, arg2, int:, arg3, ptr:, (arg4).raw-value);
  values();
end function glTexCoordPointer;

define function glEdgeFlagPointer
    (arg1 :: <GLsizei>, arg2 :: <statically-typed-pointer>)
 => ();
  call-out("glEdgeFlagPointer", void:, int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glEdgeFlagPointer;

define functional class <GLvoid**> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLvoid**>));

define inline method pointer-value
    (ptr :: <GLvoid**>, #key index = 0)
 => (result :: <statically-typed-pointer>);
  pointer-at(ptr, offset: index * 4, class: <statically-typed-pointer>);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <statically-typed-pointer>, ptr :: <GLvoid**>, #key index = 0)
 => (result :: <statically-typed-pointer>);
  pointer-at(ptr, offset: index * 4, class: <statically-typed-pointer>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLvoid**>)) => (result :: <integer>);
  4;
end method content-size;

define function glGetPointerv
    (arg1 :: <GLenum>, arg2 :: <GLvoid**>)
 => ();
  call-out("glGetPointerv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetPointerv;

define function glArrayElement
    (arg1 :: <GLint>)
 => ();
  call-out("glArrayElement", void:, int:, arg1);
  values();
end function glArrayElement;

define function glDrawArrays
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLsizei>)
 => ();
  call-out("glDrawArrays", void:, unsigned-int:, arg1, int:, arg2, int:, arg3);
  values();
end function glDrawArrays;

define function glDrawElements
    (arg1 :: <GLenum>, arg2 :: <GLsizei>, arg3 :: <GLenum>, arg4 :: <statically-typed-pointer>)
 => ();
  call-out("glDrawElements", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, ptr:, (arg4).raw-value);
  values();
end function glDrawElements;

define function glInterleavedArrays
    (arg1 :: <GLenum>, arg2 :: <GLsizei>, arg3 :: <statically-typed-pointer>)
 => ();
  call-out("glInterleavedArrays", void:, unsigned-int:, arg1, int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glInterleavedArrays;

define function glShadeModel
    (arg1 :: <GLenum>)
 => ();
  call-out("glShadeModel", void:, unsigned-int:, arg1);
  values();
end function glShadeModel;

define function glLightf
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLfloat>)
 => ();
  call-out("glLightf", void:, unsigned-int:, arg1, unsigned-int:, arg2, float:, arg3);
  values();
end function glLightf;

define function glLighti
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>)
 => ();
  call-out("glLighti", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3);
  values();
end function glLighti;

define function glLightfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glLightfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glLightfv;

define function glLightiv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glLightiv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glLightiv;

define function glGetLightfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetLightfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetLightfv;

define function glGetLightiv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetLightiv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetLightiv;

define function glLightModelf
    (arg1 :: <GLenum>, arg2 :: <GLfloat>)
 => ();
  call-out("glLightModelf", void:, unsigned-int:, arg1, float:, arg2);
  values();
end function glLightModelf;

define function glLightModeli
    (arg1 :: <GLenum>, arg2 :: <GLint>)
 => ();
  call-out("glLightModeli", void:, unsigned-int:, arg1, int:, arg2);
  values();
end function glLightModeli;

define function glLightModelfv
    (arg1 :: <GLenum>, arg2 :: <c-float-vector>)
 => ();
  call-out("glLightModelfv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glLightModelfv;

define function glLightModeliv
    (arg1 :: <GLenum>, arg2 :: <c-integer-vector>)
 => ();
  call-out("glLightModeliv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glLightModeliv;

define function glMaterialf
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLfloat>)
 => ();
  call-out("glMaterialf", void:, unsigned-int:, arg1, unsigned-int:, arg2, float:, arg3);
  values();
end function glMaterialf;

define function glMateriali
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>)
 => ();
  call-out("glMateriali", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3);
  values();
end function glMateriali;

define function glMaterialfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glMaterialfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glMaterialfv;

define function glMaterialiv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glMaterialiv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glMaterialiv;

define function glGetMaterialfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetMaterialfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetMaterialfv;

define function glGetMaterialiv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetMaterialiv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetMaterialiv;

define function glColorMaterial-internal
    (arg1 :: <GLenum>, arg2 :: <GLenum>)
 => ();
  call-out("glColorMaterial", void:, unsigned-int:, arg1, unsigned-int:, arg2);
  values();
end function glColorMaterial-internal;

define function glPixelZoom
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>)
 => ();
  call-out("glPixelZoom", void:, float:, arg1, float:, arg2);
  values();
end function glPixelZoom;

define function glPixelStoref
    (arg1 :: <GLenum>, arg2 :: <GLfloat>)
 => ();
  call-out("glPixelStoref", void:, unsigned-int:, arg1, float:, arg2);
  values();
end function glPixelStoref;

define function glPixelStorei
    (arg1 :: <GLenum>, arg2 :: <GLint>)
 => ();
  call-out("glPixelStorei", void:, unsigned-int:, arg1, int:, arg2);
  values();
end function glPixelStorei;

define function glPixelTransferf
    (arg1 :: <GLenum>, arg2 :: <GLfloat>)
 => ();
  call-out("glPixelTransferf", void:, unsigned-int:, arg1, float:, arg2);
  values();
end function glPixelTransferf;

define function glPixelTransferi
    (arg1 :: <GLenum>, arg2 :: <GLint>)
 => ();
  call-out("glPixelTransferi", void:, unsigned-int:, arg1, int:, arg2);
  values();
end function glPixelTransferi;

define function glPixelMapfv
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <c-float-vector>)
 => ();
  call-out("glPixelMapfv", void:, unsigned-int:, arg1, int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glPixelMapfv;

define function glPixelMapuiv
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLuint*>)
 => ();
  call-out("glPixelMapuiv", void:, unsigned-int:, arg1, int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glPixelMapuiv;

define function glPixelMapusv
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLushort*>)
 => ();
  call-out("glPixelMapusv", void:, unsigned-int:, arg1, int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glPixelMapusv;

define function glGetPixelMapfv
    (arg1 :: <GLenum>, arg2 :: <c-float-vector>)
 => ();
  call-out("glGetPixelMapfv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetPixelMapfv;

define function glGetPixelMapuiv
    (arg1 :: <GLenum>, arg2 :: <GLuint*>)
 => ();
  call-out("glGetPixelMapuiv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetPixelMapuiv;

define function glGetPixelMapusv
    (arg1 :: <GLenum>, arg2 :: <GLushort*>)
 => ();
  call-out("glGetPixelMapusv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glGetPixelMapusv;

define function glBitmap
    (arg1 :: <GLsizei>, arg2 :: <GLsizei>, arg3 :: <GLfloat>, arg4 :: <GLfloat>, arg5 :: <GLfloat>, arg6 :: <GLfloat>, arg7 :: <byte-string>)
 => ();
  call-out("glBitmap", void:, int:, arg1, int:, arg2, float:, arg3, float:, arg4, float:, arg5, float:, arg6, ptr:, (export-value(<c-string>, arg7)).raw-value);
  values();
end function glBitmap;

define function glReadPixels
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLsizei>, arg5 :: <GLenum>, arg6 :: <GLenum>, arg7 :: <statically-typed-pointer>)
 => ();
  call-out("glReadPixels", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4, unsigned-int:, arg5, unsigned-int:, arg6, ptr:, (arg7).raw-value);
  values();
end function glReadPixels;

define function glDrawPixels
    (arg1 :: <GLsizei>, arg2 :: <GLsizei>, arg3 :: <GLenum>, arg4 :: <GLenum>, arg5 :: <statically-typed-pointer>)
 => ();
  call-out("glDrawPixels", void:, int:, arg1, int:, arg2, unsigned-int:, arg3, unsigned-int:, arg4, ptr:, (arg5).raw-value);
  values();
end function glDrawPixels;

define function glCopyPixels-internal
    (arg1 :: <GLint>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLsizei>, arg5 :: <GLenum>)
 => ();
  call-out("glCopyPixels", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4, unsigned-int:, arg5);
  values();
end function glCopyPixels-internal;

define function glStencilFunc
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLuint>)
 => ();
  call-out("glStencilFunc", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3);
  values();
end function glStencilFunc;

define function glStencilMask
    (arg1 :: <GLuint>)
 => ();
  call-out("glStencilMask", void:, unsigned-int:, arg1);
  values();
end function glStencilMask;

define function glStencilOp
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLenum>)
 => ();
  call-out("glStencilOp", void:, unsigned-int:, arg1, unsigned-int:, arg2, unsigned-int:, arg3);
  values();
end function glStencilOp;

define function glClearStencil
    (arg1 :: <GLint>)
 => ();
  call-out("glClearStencil", void:, int:, arg1);
  values();
end function glClearStencil;

define function glTexGend
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLdouble>)
 => ();
  call-out("glTexGend", void:, unsigned-int:, arg1, unsigned-int:, arg2, double:, arg3);
  values();
end function glTexGend;

define function glTexGenf
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLfloat>)
 => ();
  call-out("glTexGenf", void:, unsigned-int:, arg1, unsigned-int:, arg2, float:, arg3);
  values();
end function glTexGenf;

define function glTexGeni
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>)
 => ();
  call-out("glTexGeni", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3);
  values();
end function glTexGeni;

define function glTexGendv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-double-vector>)
 => ();
  call-out("glTexGendv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glTexGendv;

define function glTexGenfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glTexGenfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glTexGenfv;

define function glTexGeniv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glTexGeniv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glTexGeniv;

define function glGetTexGendv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-double-vector>)
 => ();
  call-out("glGetTexGendv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetTexGendv;

define function glGetTexGenfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetTexGenfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetTexGenfv;

define function glGetTexGeniv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetTexGeniv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetTexGeniv;

define function glTexEnvf
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLfloat>)
 => ();
  call-out("glTexEnvf", void:, unsigned-int:, arg1, unsigned-int:, arg2, float:, arg3);
  values();
end function glTexEnvf;

define function glTexEnvi
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>)
 => ();
  call-out("glTexEnvi", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3);
  values();
end function glTexEnvi;

define function glTexEnvfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glTexEnvfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glTexEnvfv;

define function glTexEnviv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glTexEnviv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glTexEnviv;

define function glGetTexEnvfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetTexEnvfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetTexEnvfv;

define function glGetTexEnviv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetTexEnviv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetTexEnviv;

define function glTexParameterf
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLfloat>)
 => ();
  call-out("glTexParameterf", void:, unsigned-int:, arg1, unsigned-int:, arg2, float:, arg3);
  values();
end function glTexParameterf;

define function glTexParameteri
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>)
 => ();
  call-out("glTexParameteri", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3);
  values();
end function glTexParameteri;

define function glTexParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glTexParameterfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glTexParameterfv;

define function glTexParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glTexParameteriv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glTexParameteriv;

define function glGetTexParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetTexParameterfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetTexParameterfv;

define function glGetTexParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetTexParameteriv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetTexParameteriv;

define function glGetTexLevelParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <c-float-vector>)
 => ();
  call-out("glGetTexLevelParameterfv", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, ptr:, (arg4).raw-value);
  values();
end function glGetTexLevelParameterfv;

define function glGetTexLevelParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <c-integer-vector>)
 => ();
  call-out("glGetTexLevelParameteriv", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, ptr:, (arg4).raw-value);
  values();
end function glGetTexLevelParameteriv;

define function glTexImage1D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLsizei>, arg5 :: <GLint>, arg6 :: <GLenum>, arg7 :: <GLenum>, arg8 :: <statically-typed-pointer>)
 => ();
  call-out("glTexImage1D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, unsigned-int:, arg6, unsigned-int:, arg7, ptr:, (arg8).raw-value);
  values();
end function glTexImage1D;

define function glTexImage2D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLsizei>, arg5 :: <GLsizei>, arg6 :: <GLint>, arg7 :: <GLenum>, arg8 :: <GLenum>, arg9 :: <statically-typed-pointer>)
 => ();
  call-out("glTexImage2D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6, unsigned-int:, arg7, unsigned-int:, arg8, ptr:, (arg9).raw-value);
  values();
end function glTexImage2D;

define function glGetTexImage
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <GLenum>, arg5 :: <statically-typed-pointer>)
 => ();
  call-out("glGetTexImage", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, unsigned-int:, arg4, ptr:, (arg5).raw-value);
  values();
end function glGetTexImage;

define function glGenTextures
    (arg1 :: <GLsizei>)
 => (arg2 :: <GLuint>);
  let arg2-ptr = make(<GLuint*>);
  call-out("glGenTextures", void:, int:, arg1, ptr: arg2-ptr.raw-value);
  let arg2-value = pointer-value(arg2-ptr);
destroy(arg2-ptr);
  values(arg2-value);
end function glGenTextures;

define function glDeleteTextures
    (arg1 :: <GLsizei>, arg2 :: <GLuint*>)
 => ();
  call-out("glDeleteTextures", void:, int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glDeleteTextures;

define function glBindTexture-internal
    (arg1 :: <GLenum>, arg2 :: <GLuint>)
 => ();
  call-out("glBindTexture", void:, unsigned-int:, arg1, unsigned-int:, arg2);
  values();
end function glBindTexture-internal;

define functional class <GLclampf*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLclampf*>));

define inline method pointer-value
    (ptr :: <GLclampf*>, #key index = 0)
 => (result :: <GLclampf>);
  float-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLclampf>, ptr :: <GLclampf*>, #key index = 0)
 => (result :: <GLclampf>);
  float-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLclampf*>)) => (result :: <integer>);
  4;
end method content-size;

define function glPrioritizeTextures
    (arg1 :: <GLsizei>, arg2 :: <GLuint*>, arg3 :: <GLclampf*>)
 => ();
  call-out("glPrioritizeTextures", void:, int:, arg1, ptr:, (arg2).raw-value, ptr:, (arg3).raw-value);
  values();
end function glPrioritizeTextures;

define function glAreTexturesResident
    (arg1 :: <GLsizei>, arg2 :: <GLuint*>, arg3 :: <GLboolean*>)
 => (result :: <GLboolean>);
  let result-value
    = call-out("glAreTexturesResident", unsigned-char:, int:, arg1, ptr:, (arg2).raw-value, ptr:, (arg3).raw-value);
  values(result-value);
end function glAreTexturesResident;

define function glIsTexture
    (arg1 :: <GLuint>)
 => (result :: <GLboolean>);
  let result-value
    = call-out("glIsTexture", unsigned-char:, unsigned-int:, arg1);
  values(result-value);
end function glIsTexture;

define function glTexSubImage1D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLsizei>, arg5 :: <GLenum>, arg6 :: <GLenum>, arg7 :: <statically-typed-pointer>)
 => ();
  call-out("glTexSubImage1D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, unsigned-int:, arg5, unsigned-int:, arg6, ptr:, (arg7).raw-value);
  values();
end function glTexSubImage1D;

define function glTexSubImage2D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLsizei>, arg6 :: <GLsizei>, arg7 :: <GLenum>, arg8 :: <GLenum>, arg9 :: <statically-typed-pointer>)
 => ();
  call-out("glTexSubImage2D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6, unsigned-int:, arg7, unsigned-int:, arg8, ptr:, (arg9).raw-value);
  values();
end function glTexSubImage2D;

define function glCopyTexImage1D-internal
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLsizei>, arg7 :: <GLint>)
 => ();
  call-out("glCopyTexImage1D", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7);
  values();
end function glCopyTexImage1D-internal;

define function glCopyTexImage2D-internal
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLsizei>, arg7 :: <GLsizei>, arg8 :: <GLint>)
 => ();
  call-out("glCopyTexImage2D", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7, int:, arg8);
  values();
end function glCopyTexImage2D-internal;

define function glCopyTexSubImage1D-internal
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLsizei>)
 => ();
  call-out("glCopyTexSubImage1D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6);
  values();
end function glCopyTexSubImage1D-internal;

define function glCopyTexSubImage2D-internal
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLint>, arg7 :: <GLsizei>, arg8 :: <GLsizei>)
 => ();
  call-out("glCopyTexSubImage2D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7, int:, arg8);
  values();
end function glCopyTexSubImage2D-internal;

define function glMap1d
    (arg1 :: <GLenum>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <c-double-vector>)
 => ();
  call-out("glMap1d", void:, unsigned-int:, arg1, double:, arg2, double:, arg3, int:, arg4, int:, arg5, ptr:, (arg6).raw-value);
  values();
end function glMap1d;

define function glMap1f
    (arg1 :: <GLenum>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <c-float-vector>)
 => ();
  call-out("glMap1f", void:, unsigned-int:, arg1, float:, arg2, float:, arg3, int:, arg4, int:, arg5, ptr:, (arg6).raw-value);
  values();
end function glMap1f;

define function glMap2d
    (arg1 :: <GLenum>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLdouble>, arg7 :: <GLdouble>, arg8 :: <GLint>, arg9 :: <GLint>, arg10 :: <c-double-vector>)
 => ();
  call-out("glMap2d", void:, unsigned-int:, arg1, double:, arg2, double:, arg3, int:, arg4, int:, arg5, double:, arg6, double:, arg7, int:, arg8, int:, arg9, ptr:, (arg10).raw-value);
  values();
end function glMap2d;

define function glMap2f
    (arg1 :: <GLenum>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLfloat>, arg7 :: <GLfloat>, arg8 :: <GLint>, arg9 :: <GLint>, arg10 :: <c-float-vector>)
 => ();
  call-out("glMap2f", void:, unsigned-int:, arg1, float:, arg2, float:, arg3, int:, arg4, int:, arg5, float:, arg6, float:, arg7, int:, arg8, int:, arg9, ptr:, (arg10).raw-value);
  values();
end function glMap2f;

define function glGetMapdv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-double-vector>)
 => ();
  call-out("glGetMapdv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetMapdv;

define function glGetMapfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetMapfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetMapfv;

define function glGetMapiv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetMapiv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetMapiv;

define function glEvalCoord1d
    (arg1 :: <GLdouble>)
 => ();
  call-out("glEvalCoord1d", void:, double:, arg1);
  values();
end function glEvalCoord1d;

define function glEvalCoord1f
    (arg1 :: <GLfloat>)
 => ();
  call-out("glEvalCoord1f", void:, float:, arg1);
  values();
end function glEvalCoord1f;

define function glEvalCoord1dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glEvalCoord1dv", void:, ptr:, (arg1).raw-value);
  values();
end function glEvalCoord1dv;

define function glEvalCoord1fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glEvalCoord1fv", void:, ptr:, (arg1).raw-value);
  values();
end function glEvalCoord1fv;

define function glEvalCoord2d
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>)
 => ();
  call-out("glEvalCoord2d", void:, double:, arg1, double:, arg2);
  values();
end function glEvalCoord2d;

define function glEvalCoord2f
    (arg1 :: <GLfloat>, arg2 :: <GLfloat>)
 => ();
  call-out("glEvalCoord2f", void:, float:, arg1, float:, arg2);
  values();
end function glEvalCoord2f;

define function glEvalCoord2dv
    (arg1 :: <c-double-vector>)
 => ();
  call-out("glEvalCoord2dv", void:, ptr:, (arg1).raw-value);
  values();
end function glEvalCoord2dv;

define function glEvalCoord2fv
    (arg1 :: <c-float-vector>)
 => ();
  call-out("glEvalCoord2fv", void:, ptr:, (arg1).raw-value);
  values();
end function glEvalCoord2fv;

define function glMapGrid1d
    (arg1 :: <GLint>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glMapGrid1d", void:, int:, arg1, double:, arg2, double:, arg3);
  values();
end function glMapGrid1d;

define function glMapGrid1f
    (arg1 :: <GLint>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glMapGrid1f", void:, int:, arg1, float:, arg2, float:, arg3);
  values();
end function glMapGrid1f;

define function glMapGrid2d
    (arg1 :: <GLint>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLint>, arg5 :: <GLdouble>, arg6 :: <GLdouble>)
 => ();
  call-out("glMapGrid2d", void:, int:, arg1, double:, arg2, double:, arg3, int:, arg4, double:, arg5, double:, arg6);
  values();
end function glMapGrid2d;

define function glMapGrid2f
    (arg1 :: <GLint>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLint>, arg5 :: <GLfloat>, arg6 :: <GLfloat>)
 => ();
  call-out("glMapGrid2f", void:, int:, arg1, float:, arg2, float:, arg3, int:, arg4, float:, arg5, float:, arg6);
  values();
end function glMapGrid2f;

define function glEvalPoint1
    (arg1 :: <GLint>)
 => ();
  call-out("glEvalPoint1", void:, int:, arg1);
  values();
end function glEvalPoint1;

define function glEvalPoint2
    (arg1 :: <GLint>, arg2 :: <GLint>)
 => ();
  call-out("glEvalPoint2", void:, int:, arg1, int:, arg2);
  values();
end function glEvalPoint2;

define function glEvalMesh1
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glEvalMesh1", void:, unsigned-int:, arg1, int:, arg2, int:, arg3);
  values();
end function glEvalMesh1;

define function glEvalMesh2
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLint>)
 => ();
  call-out("glEvalMesh2", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5);
  values();
end function glEvalMesh2;

define function glFogf
    (arg1 :: <GLenum>, arg2 :: <GLfloat>)
 => ();
  call-out("glFogf", void:, unsigned-int:, arg1, float:, arg2);
  values();
end function glFogf;

define function glFogi
    (arg1 :: <GLenum>, arg2 :: <GLint>)
 => ();
  call-out("glFogi", void:, unsigned-int:, arg1, int:, arg2);
  values();
end function glFogi;

define function glFogfv
    (arg1 :: <GLenum>, arg2 :: <c-float-vector>)
 => ();
  call-out("glFogfv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glFogfv;

define function glFogiv
    (arg1 :: <GLenum>, arg2 :: <c-integer-vector>)
 => ();
  call-out("glFogiv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glFogiv;

define function glFeedbackBuffer
    (arg1 :: <GLsizei>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glFeedbackBuffer", void:, int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glFeedbackBuffer;

define function glPassThrough
    (arg1 :: <GLfloat>)
 => ();
  call-out("glPassThrough", void:, float:, arg1);
  values();
end function glPassThrough;

define function glSelectBuffer
    (arg1 :: <GLsizei>, arg2 :: <GLuint*>)
 => ();
  call-out("glSelectBuffer", void:, int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glSelectBuffer;

define function glInitNames
    ()
 => ();
  call-out("glInitNames", void:);
  values();
end function glInitNames;

define function glLoadName
    (arg1 :: <GLuint>)
 => ();
  call-out("glLoadName", void:, unsigned-int:, arg1);
  values();
end function glLoadName;

define function glPushName
    (arg1 :: <GLuint>)
 => ();
  call-out("glPushName", void:, unsigned-int:, arg1);
  values();
end function glPushName;

define function glPopName
    ()
 => ();
  call-out("glPopName", void:);
  values();
end function glPopName;

define function glDrawRangeElements
    (arg1 :: <GLenum>, arg2 :: <GLuint>, arg3 :: <GLuint>, arg4 :: <GLsizei>, arg5 :: <GLenum>, arg6 :: <statically-typed-pointer>)
 => ();
  call-out("glDrawRangeElements", void:, unsigned-int:, arg1, unsigned-int:, arg2, unsigned-int:, arg3, int:, arg4, unsigned-int:, arg5, ptr:, (arg6).raw-value);
  values();
end function glDrawRangeElements;

define function glTexImage3D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <GLsizei>, arg5 :: <GLsizei>, arg6 :: <GLsizei>, arg7 :: <GLint>, arg8 :: <GLenum>, arg9 :: <GLenum>, arg10 :: <statically-typed-pointer>)
 => ();
  call-out("glTexImage3D", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7, unsigned-int:, arg8, unsigned-int:, arg9, ptr:, (arg10).raw-value);
  values();
end function glTexImage3D;

define function glTexSubImage3D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLsizei>, arg7 :: <GLsizei>, arg8 :: <GLsizei>, arg9 :: <GLenum>, arg10 :: <GLenum>, arg11 :: <statically-typed-pointer>)
 => ();
  call-out("glTexSubImage3D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7, int:, arg8, unsigned-int:, arg9, unsigned-int:, arg10, ptr:, (arg11).raw-value);
  values();
end function glTexSubImage3D;

define function glCopyTexSubImage3D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLint>, arg7 :: <GLint>, arg8 :: <GLsizei>, arg9 :: <GLsizei>)
 => ();
  call-out("glCopyTexSubImage3D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7, int:, arg8, int:, arg9);
  values();
end function glCopyTexSubImage3D;

define function glColorTable
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLsizei>, arg4 :: <GLenum>, arg5 :: <GLenum>, arg6 :: <statically-typed-pointer>)
 => ();
  call-out("glColorTable", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3, unsigned-int:, arg4, unsigned-int:, arg5, ptr:, (arg6).raw-value);
  values();
end function glColorTable;

define function glColorSubTable
    (arg1 :: <GLenum>, arg2 :: <GLsizei>, arg3 :: <GLsizei>, arg4 :: <GLenum>, arg5 :: <GLenum>, arg6 :: <statically-typed-pointer>)
 => ();
  call-out("glColorSubTable", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, unsigned-int:, arg4, unsigned-int:, arg5, ptr:, (arg6).raw-value);
  values();
end function glColorSubTable;

define function glColorTableParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glColorTableParameteriv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glColorTableParameteriv;

define function glColorTableParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glColorTableParameterfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glColorTableParameterfv;

define function glCopyColorSubTable
    (arg1 :: <GLenum>, arg2 :: <GLsizei>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLsizei>)
 => ();
  call-out("glCopyColorSubTable", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5);
  values();
end function glCopyColorSubTable;

define function glCopyColorTable
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLsizei>)
 => ();
  call-out("glCopyColorTable", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3, int:, arg4, int:, arg5);
  values();
end function glCopyColorTable;

define function glGetColorTable
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLenum>, arg4 :: <statically-typed-pointer>)
 => ();
  call-out("glGetColorTable", void:, unsigned-int:, arg1, unsigned-int:, arg2, unsigned-int:, arg3, ptr:, (arg4).raw-value);
  values();
end function glGetColorTable;

define function glGetColorTableParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetColorTableParameterfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetColorTableParameterfv;

define function glGetColorTableParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetColorTableParameteriv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetColorTableParameteriv;

define function glBlendEquation
    (arg1 :: <GLenum>)
 => ();
  call-out("glBlendEquation", void:, unsigned-int:, arg1);
  values();
end function glBlendEquation;

define function glBlendColor
    (arg1 :: <GLclampf>, arg2 :: <GLclampf>, arg3 :: <GLclampf>, arg4 :: <GLclampf>)
 => ();
  call-out("glBlendColor", void:, float:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glBlendColor;

define function glHistogram
    (arg1 :: <GLenum>, arg2 :: <GLsizei>, arg3 :: <GLenum>, arg4 :: <GLboolean>)
 => ();
  call-out("glHistogram", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, unsigned-char:, arg4);
  values();
end function glHistogram;

define function glResetHistogram
    (arg1 :: <GLenum>)
 => ();
  call-out("glResetHistogram", void:, unsigned-int:, arg1);
  values();
end function glResetHistogram;

define function glGetHistogram
    (arg1 :: <GLenum>, arg2 :: <GLboolean>, arg3 :: <GLenum>, arg4 :: <GLenum>, arg5 :: <statically-typed-pointer>)
 => ();
  call-out("glGetHistogram", void:, unsigned-int:, arg1, unsigned-char:, arg2, unsigned-int:, arg3, unsigned-int:, arg4, ptr:, (arg5).raw-value);
  values();
end function glGetHistogram;

define function glGetHistogramParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetHistogramParameterfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetHistogramParameterfv;

define function glGetHistogramParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetHistogramParameteriv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetHistogramParameteriv;

define function glMinmax
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLboolean>)
 => ();
  call-out("glMinmax", void:, unsigned-int:, arg1, unsigned-int:, arg2, unsigned-char:, arg3);
  values();
end function glMinmax;

define function glResetMinmax
    (arg1 :: <GLenum>)
 => ();
  call-out("glResetMinmax", void:, unsigned-int:, arg1);
  values();
end function glResetMinmax;

define function glGetMinmax
    (arg1 :: <GLenum>, arg2 :: <GLboolean>, arg3 :: <GLenum>, arg4 :: <GLenum>, arg5 :: <statically-typed-pointer>)
 => ();
  call-out("glGetMinmax", void:, unsigned-int:, arg1, unsigned-char:, arg2, unsigned-int:, arg3, unsigned-int:, arg4, ptr:, (arg5).raw-value);
  values();
end function glGetMinmax;

define function glGetMinmaxParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetMinmaxParameterfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetMinmaxParameterfv;

define function glGetMinmaxParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetMinmaxParameteriv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetMinmaxParameteriv;

define function glConvolutionFilter1D
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLsizei>, arg4 :: <GLenum>, arg5 :: <GLenum>, arg6 :: <statically-typed-pointer>)
 => ();
  call-out("glConvolutionFilter1D", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3, unsigned-int:, arg4, unsigned-int:, arg5, ptr:, (arg6).raw-value);
  values();
end function glConvolutionFilter1D;

define function glConvolutionFilter2D
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLsizei>, arg4 :: <GLsizei>, arg5 :: <GLenum>, arg6 :: <GLenum>, arg7 :: <statically-typed-pointer>)
 => ();
  call-out("glConvolutionFilter2D", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3, int:, arg4, unsigned-int:, arg5, unsigned-int:, arg6, ptr:, (arg7).raw-value);
  values();
end function glConvolutionFilter2D;

define function glConvolutionParameterf
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLfloat>)
 => ();
  call-out("glConvolutionParameterf", void:, unsigned-int:, arg1, unsigned-int:, arg2, float:, arg3);
  values();
end function glConvolutionParameterf;

define function glConvolutionParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glConvolutionParameterfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glConvolutionParameterfv;

define function glConvolutionParameteri
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>)
 => ();
  call-out("glConvolutionParameteri", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3);
  values();
end function glConvolutionParameteri;

define function glConvolutionParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glConvolutionParameteriv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glConvolutionParameteriv;

define function glCopyConvolutionFilter1D
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLsizei>)
 => ();
  call-out("glCopyConvolutionFilter1D", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3, int:, arg4, int:, arg5);
  values();
end function glCopyConvolutionFilter1D;

define function glCopyConvolutionFilter2D
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLsizei>, arg6 :: <GLsizei>)
 => ();
  call-out("glCopyConvolutionFilter2D", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6);
  values();
end function glCopyConvolutionFilter2D;

define function glGetConvolutionFilter
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLenum>, arg4 :: <statically-typed-pointer>)
 => ();
  call-out("glGetConvolutionFilter", void:, unsigned-int:, arg1, unsigned-int:, arg2, unsigned-int:, arg3, ptr:, (arg4).raw-value);
  values();
end function glGetConvolutionFilter;

define function glGetConvolutionParameterfv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-float-vector>)
 => ();
  call-out("glGetConvolutionParameterfv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetConvolutionParameterfv;

define function glGetConvolutionParameteriv
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <c-integer-vector>)
 => ();
  call-out("glGetConvolutionParameteriv", void:, unsigned-int:, arg1, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetConvolutionParameteriv;

define function glSeparableFilter2D
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLsizei>, arg4 :: <GLsizei>, arg5 :: <GLenum>, arg6 :: <GLenum>, arg7 :: <statically-typed-pointer>, arg8 :: <statically-typed-pointer>)
 => ();
  call-out("glSeparableFilter2D", void:, unsigned-int:, arg1, unsigned-int:, arg2, int:, arg3, int:, arg4, unsigned-int:, arg5, unsigned-int:, arg6, ptr:, (arg7).raw-value, ptr:, (arg8).raw-value);
  values();
end function glSeparableFilter2D;

define function glGetSeparableFilter
    (arg1 :: <GLenum>, arg2 :: <GLenum>, arg3 :: <GLenum>, arg4 :: <statically-typed-pointer>, arg5 :: <statically-typed-pointer>, arg6 :: <statically-typed-pointer>)
 => ();
  call-out("glGetSeparableFilter", void:, unsigned-int:, arg1, unsigned-int:, arg2, unsigned-int:, arg3, ptr:, (arg4).raw-value, ptr:, (arg5).raw-value, ptr:, (arg6).raw-value);
  values();
end function glGetSeparableFilter;

define function glActiveTexture
    (arg1 :: <GLenum>)
 => ();
  call-out("glActiveTexture", void:, unsigned-int:, arg1);
  values();
end function glActiveTexture;

define function glClientActiveTexture
    (arg1 :: <GLenum>)
 => ();
  call-out("glClientActiveTexture", void:, unsigned-int:, arg1);
  values();
end function glClientActiveTexture;

define function glCompressedTexImage1D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <GLsizei>, arg5 :: <GLint>, arg6 :: <GLsizei>, arg7 :: <statically-typed-pointer>)
 => ();
  call-out("glCompressedTexImage1D", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, int:, arg4, int:, arg5, int:, arg6, ptr:, (arg7).raw-value);
  values();
end function glCompressedTexImage1D;

define function glCompressedTexImage2D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <GLsizei>, arg5 :: <GLsizei>, arg6 :: <GLint>, arg7 :: <GLsizei>, arg8 :: <statically-typed-pointer>)
 => ();
  call-out("glCompressedTexImage2D", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7, ptr:, (arg8).raw-value);
  values();
end function glCompressedTexImage2D;

define function glCompressedTexImage3D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLenum>, arg4 :: <GLsizei>, arg5 :: <GLsizei>, arg6 :: <GLsizei>, arg7 :: <GLint>, arg8 :: <GLsizei>, arg9 :: <statically-typed-pointer>)
 => ();
  call-out("glCompressedTexImage3D", void:, unsigned-int:, arg1, int:, arg2, unsigned-int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7, int:, arg8, ptr:, (arg9).raw-value);
  values();
end function glCompressedTexImage3D;

define function glCompressedTexSubImage1D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLsizei>, arg5 :: <GLenum>, arg6 :: <GLsizei>, arg7 :: <statically-typed-pointer>)
 => ();
  call-out("glCompressedTexSubImage1D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, unsigned-int:, arg5, int:, arg6, ptr:, (arg7).raw-value);
  values();
end function glCompressedTexSubImage1D;

define function glCompressedTexSubImage2D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLsizei>, arg6 :: <GLsizei>, arg7 :: <GLenum>, arg8 :: <GLsizei>, arg9 :: <statically-typed-pointer>)
 => ();
  call-out("glCompressedTexSubImage2D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6, unsigned-int:, arg7, int:, arg8, ptr:, (arg9).raw-value);
  values();
end function glCompressedTexSubImage2D;

define function glCompressedTexSubImage3D
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLsizei>, arg7 :: <GLsizei>, arg8 :: <GLsizei>, arg9 :: <GLenum>, arg10 :: <GLsizei>, arg11 :: <statically-typed-pointer>)
 => ();
  call-out("glCompressedTexSubImage3D", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, int:, arg6, int:, arg7, int:, arg8, unsigned-int:, arg9, int:, arg10, ptr:, (arg11).raw-value);
  values();
end function glCompressedTexSubImage3D;

define function glGetCompressedTexImage
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <statically-typed-pointer>)
 => ();
  call-out("glGetCompressedTexImage", void:, unsigned-int:, arg1, int:, arg2, ptr:, (arg3).raw-value);
  values();
end function glGetCompressedTexImage;

define function glMultiTexCoord1d
    (arg1 :: <GLenum>, arg2 :: <GLdouble>)
 => ();
  call-out("glMultiTexCoord1d", void:, unsigned-int:, arg1, double:, arg2);
  values();
end function glMultiTexCoord1d;

define function glMultiTexCoord1dv
    (arg1 :: <GLenum>, arg2 :: <c-double-vector>)
 => ();
  call-out("glMultiTexCoord1dv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord1dv;

define function glMultiTexCoord1f
    (arg1 :: <GLenum>, arg2 :: <GLfloat>)
 => ();
  call-out("glMultiTexCoord1f", void:, unsigned-int:, arg1, float:, arg2);
  values();
end function glMultiTexCoord1f;

define function glMultiTexCoord1fv
    (arg1 :: <GLenum>, arg2 :: <c-float-vector>)
 => ();
  call-out("glMultiTexCoord1fv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord1fv;

define function glMultiTexCoord1i
    (arg1 :: <GLenum>, arg2 :: <GLint>)
 => ();
  call-out("glMultiTexCoord1i", void:, unsigned-int:, arg1, int:, arg2);
  values();
end function glMultiTexCoord1i;

define function glMultiTexCoord1iv
    (arg1 :: <GLenum>, arg2 :: <c-integer-vector>)
 => ();
  call-out("glMultiTexCoord1iv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord1iv;

define function glMultiTexCoord1s
    (arg1 :: <GLenum>, arg2 :: <GLshort>)
 => ();
  call-out("glMultiTexCoord1s", void:, unsigned-int:, arg1, short:, arg2);
  values();
end function glMultiTexCoord1s;

define function glMultiTexCoord1sv
    (arg1 :: <GLenum>, arg2 :: <GLshort*>)
 => ();
  call-out("glMultiTexCoord1sv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord1sv;

define function glMultiTexCoord2d
    (arg1 :: <GLenum>, arg2 :: <GLdouble>, arg3 :: <GLdouble>)
 => ();
  call-out("glMultiTexCoord2d", void:, unsigned-int:, arg1, double:, arg2, double:, arg3);
  values();
end function glMultiTexCoord2d;

define function glMultiTexCoord2dv
    (arg1 :: <GLenum>, arg2 :: <c-double-vector>)
 => ();
  call-out("glMultiTexCoord2dv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord2dv;

define function glMultiTexCoord2f
    (arg1 :: <GLenum>, arg2 :: <GLfloat>, arg3 :: <GLfloat>)
 => ();
  call-out("glMultiTexCoord2f", void:, unsigned-int:, arg1, float:, arg2, float:, arg3);
  values();
end function glMultiTexCoord2f;

define function glMultiTexCoord2fv
    (arg1 :: <GLenum>, arg2 :: <c-float-vector>)
 => ();
  call-out("glMultiTexCoord2fv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord2fv;

define function glMultiTexCoord2i
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glMultiTexCoord2i", void:, unsigned-int:, arg1, int:, arg2, int:, arg3);
  values();
end function glMultiTexCoord2i;

define function glMultiTexCoord2iv
    (arg1 :: <GLenum>, arg2 :: <c-integer-vector>)
 => ();
  call-out("glMultiTexCoord2iv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord2iv;

define function glMultiTexCoord2s
    (arg1 :: <GLenum>, arg2 :: <GLshort>, arg3 :: <GLshort>)
 => ();
  call-out("glMultiTexCoord2s", void:, unsigned-int:, arg1, short:, arg2, short:, arg3);
  values();
end function glMultiTexCoord2s;

define function glMultiTexCoord2sv
    (arg1 :: <GLenum>, arg2 :: <GLshort*>)
 => ();
  call-out("glMultiTexCoord2sv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord2sv;

define function glMultiTexCoord3d
    (arg1 :: <GLenum>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("glMultiTexCoord3d", void:, unsigned-int:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function glMultiTexCoord3d;

define function glMultiTexCoord3dv
    (arg1 :: <GLenum>, arg2 :: <c-double-vector>)
 => ();
  call-out("glMultiTexCoord3dv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord3dv;

define function glMultiTexCoord3f
    (arg1 :: <GLenum>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glMultiTexCoord3f", void:, unsigned-int:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glMultiTexCoord3f;

define function glMultiTexCoord3fv
    (arg1 :: <GLenum>, arg2 :: <c-float-vector>)
 => ();
  call-out("glMultiTexCoord3fv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord3fv;

define function glMultiTexCoord3i
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glMultiTexCoord3i", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glMultiTexCoord3i;

define function glMultiTexCoord3iv
    (arg1 :: <GLenum>, arg2 :: <c-integer-vector>)
 => ();
  call-out("glMultiTexCoord3iv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord3iv;

define function glMultiTexCoord3s
    (arg1 :: <GLenum>, arg2 :: <GLshort>, arg3 :: <GLshort>, arg4 :: <GLshort>)
 => ();
  call-out("glMultiTexCoord3s", void:, unsigned-int:, arg1, short:, arg2, short:, arg3, short:, arg4);
  values();
end function glMultiTexCoord3s;

define function glMultiTexCoord3sv
    (arg1 :: <GLenum>, arg2 :: <GLshort*>)
 => ();
  call-out("glMultiTexCoord3sv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord3sv;

define function glMultiTexCoord4d
    (arg1 :: <GLenum>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>, arg5 :: <GLdouble>)
 => ();
  call-out("glMultiTexCoord4d", void:, unsigned-int:, arg1, double:, arg2, double:, arg3, double:, arg4, double:, arg5);
  values();
end function glMultiTexCoord4d;

define function glMultiTexCoord4dv
    (arg1 :: <GLenum>, arg2 :: <c-double-vector>)
 => ();
  call-out("glMultiTexCoord4dv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord4dv;

define function glMultiTexCoord4f
    (arg1 :: <GLenum>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>, arg5 :: <GLfloat>)
 => ();
  call-out("glMultiTexCoord4f", void:, unsigned-int:, arg1, float:, arg2, float:, arg3, float:, arg4, float:, arg5);
  values();
end function glMultiTexCoord4f;

define function glMultiTexCoord4fv
    (arg1 :: <GLenum>, arg2 :: <c-float-vector>)
 => ();
  call-out("glMultiTexCoord4fv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord4fv;

define function glMultiTexCoord4i
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLint>, arg4 :: <GLint>, arg5 :: <GLint>)
 => ();
  call-out("glMultiTexCoord4i", void:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5);
  values();
end function glMultiTexCoord4i;

define function glMultiTexCoord4iv
    (arg1 :: <GLenum>, arg2 :: <c-integer-vector>)
 => ();
  call-out("glMultiTexCoord4iv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord4iv;

define function glMultiTexCoord4s
    (arg1 :: <GLenum>, arg2 :: <GLshort>, arg3 :: <GLshort>, arg4 :: <GLshort>, arg5 :: <GLshort>)
 => ();
  call-out("glMultiTexCoord4s", void:, unsigned-int:, arg1, short:, arg2, short:, arg3, short:, arg4, short:, arg5);
  values();
end function glMultiTexCoord4s;

define function glMultiTexCoord4sv
    (arg1 :: <GLenum>, arg2 :: <GLshort*>)
 => ();
  call-out("glMultiTexCoord4sv", void:, unsigned-int:, arg1, ptr:, (arg2).raw-value);
  values();
end function glMultiTexCoord4sv;

define functional class <GLdouble<@16>> (<c-double-vector>, <c-vector>) end class;

define sealed domain make (singleton(<GLdouble<@16>>));

define inline method content-size (value == <GLdouble<@16>>)  => (result :: <integer>);
  16;
end method content-size;

define function glLoadTransposeMatrixd
    (arg1 :: <GLdouble<@16>>)
 => ();
  call-out("glLoadTransposeMatrixd", void:, ptr:, (arg1).raw-value);
  values();
end function glLoadTransposeMatrixd;

define functional class <GLfloat<@16>> (<c-float-vector>, <c-vector>) end class;

define sealed domain make (singleton(<GLfloat<@16>>));

define inline method content-size (value == <GLfloat<@16>>)  => (result :: <integer>);
  16;
end method content-size;

define function glLoadTransposeMatrixf
    (arg1 :: <GLfloat<@16>>)
 => ();
  call-out("glLoadTransposeMatrixf", void:, ptr:, (arg1).raw-value);
  values();
end function glLoadTransposeMatrixf;

define function glMultTransposeMatrixd
    (arg1 :: <GLdouble<@16>>)
 => ();
  call-out("glMultTransposeMatrixd", void:, ptr:, (arg1).raw-value);
  values();
end function glMultTransposeMatrixd;

define function glMultTransposeMatrixf
    (arg1 :: <GLfloat<@16>>)
 => ();
  call-out("glMultTransposeMatrixf", void:, ptr:, (arg1).raw-value);
  values();
end function glMultTransposeMatrixf;

define function glSampleCoverage
    (arg1 :: <GLclampf>, arg2 :: <GLboolean>)
 => ();
  call-out("glSampleCoverage", void:, float:, arg1, unsigned-char:, arg2);
  values();
end function glSampleCoverage;

define constant $GL-VERSION-1-1 = 1;

define constant $GL-VERSION-1-2 = 1;

define constant $GL-VERSION-1-3 = 1;

define constant $GL-ARB-imaging = 1;

define constant $GL-FALSE = 0;

define constant $GL-TRUE = 1;

define constant $GL-BYTE = 5120;

define constant $GL-UNSIGNED-BYTE = 5121;

define constant $GL-SHORT = 5122;

define constant $GL-UNSIGNED-SHORT = 5123;

define constant $GL-INT = 5124;

define constant $GL-UNSIGNED-INT = 5125;

define constant $GL-FLOAT = 5126;

define constant $GL-DOUBLE = 5130;

define constant $GL-2-BYTES = 5127;

define constant $GL-3-BYTES = 5128;

define constant $GL-4-BYTES = 5129;

define constant $GL-POINTS = 0;

define constant $GL-LINES = 1;

define constant $GL-LINE-LOOP = 2;

define constant $GL-LINE-STRIP = 3;

define constant $GL-TRIANGLES = 4;

define constant $GL-TRIANGLE-STRIP = 5;

define constant $GL-TRIANGLE-FAN = 6;

define constant $GL-QUADS = 7;

define constant $GL-QUAD-STRIP = 8;

define constant $GL-POLYGON = 9;

define constant $GL-VERTEX-ARRAY = 32884;

define constant $GL-NORMAL-ARRAY = 32885;

define constant $GL-COLOR-ARRAY = 32886;

define constant $GL-INDEX-ARRAY = 32887;

define constant $GL-TEXTURE-COORD-ARRAY = 32888;

define constant $GL-EDGE-FLAG-ARRAY = 32889;

define constant $GL-VERTEX-ARRAY-SIZE = 32890;

define constant $GL-VERTEX-ARRAY-TYPE = 32891;

define constant $GL-VERTEX-ARRAY-STRIDE = 32892;

define constant $GL-NORMAL-ARRAY-TYPE = 32894;

define constant $GL-NORMAL-ARRAY-STRIDE = 32895;

define constant $GL-COLOR-ARRAY-SIZE = 32897;

define constant $GL-COLOR-ARRAY-TYPE = 32898;

define constant $GL-COLOR-ARRAY-STRIDE = 32899;

define constant $GL-INDEX-ARRAY-TYPE = 32901;

define constant $GL-INDEX-ARRAY-STRIDE = 32902;

define constant $GL-TEXTURE-COORD-ARRAY-SIZE = 32904;

define constant $GL-TEXTURE-COORD-ARRAY-TYPE = 32905;

define constant $GL-TEXTURE-COORD-ARRAY-STRIDE = 32906;

define constant $GL-EDGE-FLAG-ARRAY-STRIDE = 32908;

define constant $GL-VERTEX-ARRAY-POINTER = 32910;

define constant $GL-NORMAL-ARRAY-POINTER = 32911;

define constant $GL-COLOR-ARRAY-POINTER = 32912;

define constant $GL-INDEX-ARRAY-POINTER = 32913;

define constant $GL-TEXTURE-COORD-ARRAY-POINTER = 32914;

define constant $GL-EDGE-FLAG-ARRAY-POINTER = 32915;

define constant $GL-V2F = 10784;

define constant $GL-V3F = 10785;

define constant $GL-C4UB-V2F = 10786;

define constant $GL-C4UB-V3F = 10787;

define constant $GL-C3F-V3F = 10788;

define constant $GL-N3F-V3F = 10789;

define constant $GL-C4F-N3F-V3F = 10790;

define constant $GL-T2F-V3F = 10791;

define constant $GL-T4F-V4F = 10792;

define constant $GL-T2F-C4UB-V3F = 10793;

define constant $GL-T2F-C3F-V3F = 10794;

define constant $GL-T2F-N3F-V3F = 10795;

define constant $GL-T2F-C4F-N3F-V3F = 10796;

define constant $GL-T4F-C4F-N3F-V4F = 10797;

define constant $GL-MATRIX-MODE = 2976;

define constant $GL-MODELVIEW = 5888;

define constant $GL-PROJECTION = 5889;

define constant $GL-TEXTURE = 5890;

define constant $GL-POINT-SMOOTH = 2832;

define constant $GL-POINT-SIZE = 2833;

define constant $GL-POINT-SIZE-GRANULARITY = 2835;

define constant $GL-POINT-SIZE-RANGE = 2834;

define constant $GL-LINE-SMOOTH = 2848;

define constant $GL-LINE-STIPPLE = 2852;

define constant $GL-LINE-STIPPLE-PATTERN = 2853;

define constant $GL-LINE-STIPPLE-REPEAT = 2854;

define constant $GL-LINE-WIDTH = 2849;

define constant $GL-LINE-WIDTH-GRANULARITY = 2851;

define constant $GL-LINE-WIDTH-RANGE = 2850;

define constant $GL-POINT = 6912;

define constant $GL-LINE = 6913;

define constant $GL-FILL = 6914;

define constant $GL-CW = 2304;

define constant $GL-CCW = 2305;

define constant $GL-FRONT = 1028;

define constant $GL-BACK = 1029;

define constant $GL-POLYGON-MODE = 2880;

define constant $GL-POLYGON-SMOOTH = 2881;

define constant $GL-POLYGON-STIPPLE = 2882;

define constant $GL-EDGE-FLAG = 2883;

define constant $GL-CULL-FACE = 2884;

define constant $GL-CULL-FACE-MODE = 2885;

define constant $GL-FRONT-FACE = 2886;

define constant $GL-POLYGON-OFFSET-FACTOR = 32824;

define constant $GL-POLYGON-OFFSET-UNITS = 10752;

define constant $GL-POLYGON-OFFSET-POINT = 10753;

define constant $GL-POLYGON-OFFSET-LINE = 10754;

define constant $GL-POLYGON-OFFSET-FILL = 32823;

define constant $GL-COMPILE = 4864;

define constant $GL-COMPILE-AND-EXECUTE = 4865;

define constant $GL-LIST-BASE = 2866;

define constant $GL-LIST-INDEX = 2867;

define constant $GL-LIST-MODE = 2864;

define constant $GL-NEVER = 512;

define constant $GL-LESS = 513;

define constant $GL-EQUAL = 514;

define constant $GL-LEQUAL = 515;

define constant $GL-GREATER = 516;

define constant $GL-NOTEQUAL = 517;

define constant $GL-GEQUAL = 518;

define constant $GL-ALWAYS = 519;

define constant $GL-DEPTH-TEST = 2929;

define constant $GL-DEPTH-BITS = 3414;

define constant $GL-DEPTH-CLEAR-VALUE = 2931;

define constant $GL-DEPTH-FUNC = 2932;

define constant $GL-DEPTH-RANGE = 2928;

define constant $GL-DEPTH-WRITEMASK = 2930;

define constant $GL-DEPTH-COMPONENT = 6402;

define constant $GL-LIGHTING = 2896;

define constant $GL-LIGHT0 = 16384;

define constant $GL-LIGHT1 = 16385;

define constant $GL-LIGHT2 = 16386;

define constant $GL-LIGHT3 = 16387;

define constant $GL-LIGHT4 = 16388;

define constant $GL-LIGHT5 = 16389;

define constant $GL-LIGHT6 = 16390;

define constant $GL-LIGHT7 = 16391;

define constant $GL-SPOT-EXPONENT = 4613;

define constant $GL-SPOT-CUTOFF = 4614;

define constant $GL-CONSTANT-ATTENUATION = 4615;

define constant $GL-LINEAR-ATTENUATION = 4616;

define constant $GL-QUADRATIC-ATTENUATION = 4617;

define constant $GL-AMBIENT = 4608;

define constant $GL-DIFFUSE = 4609;

define constant $GL-SPECULAR = 4610;

define constant $GL-SHININESS = 5633;

define constant $GL-EMISSION = 5632;

define constant $GL-POSITION = 4611;

define constant $GL-SPOT-DIRECTION = 4612;

define constant $GL-AMBIENT-AND-DIFFUSE = 5634;

define constant $GL-COLOR-INDEXES = 5635;

define constant $GL-LIGHT-MODEL-TWO-SIDE = 2898;

define constant $GL-LIGHT-MODEL-LOCAL-VIEWER = 2897;

define constant $GL-LIGHT-MODEL-AMBIENT = 2899;

define constant $GL-FRONT-AND-BACK = 1032;

define constant $GL-SHADE-MODEL = 2900;

define constant $GL-FLAT = 7424;

define constant $GL-SMOOTH = 7425;

define constant $GL-COLOR-MATERIAL = 2903;

define constant $GL-COLOR-MATERIAL-FACE = 2901;

define constant $GL-COLOR-MATERIAL-PARAMETER = 2902;

define constant $GL-NORMALIZE = 2977;

define constant $GL-CLIP-PLANE0 = 12288;

define constant $GL-CLIP-PLANE1 = 12289;

define constant $GL-CLIP-PLANE2 = 12290;

define constant $GL-CLIP-PLANE3 = 12291;

define constant $GL-CLIP-PLANE4 = 12292;

define constant $GL-CLIP-PLANE5 = 12293;

define constant $GL-ACCUM-RED-BITS = 3416;

define constant $GL-ACCUM-GREEN-BITS = 3417;

define constant $GL-ACCUM-BLUE-BITS = 3418;

define constant $GL-ACCUM-ALPHA-BITS = 3419;

define constant $GL-ACCUM-CLEAR-VALUE = 2944;

define constant $GL-ACCUM = 256;

define constant $GL-ADD = 260;

define constant $GL-LOAD = 257;

define constant $GL-MULT = 259;

define constant $GL-RETURN = 258;

define constant $GL-ALPHA-TEST = 3008;

define constant $GL-ALPHA-TEST-REF = 3010;

define constant $GL-ALPHA-TEST-FUNC = 3009;

define constant $GL-BLEND = 3042;

define constant $GL-BLEND-SRC = 3041;

define constant $GL-BLEND-DST = 3040;

define constant $GL-ZERO = 0;

define constant $GL-ONE = 1;

define constant $GL-SRC-COLOR = 768;

define constant $GL-ONE-MINUS-SRC-COLOR = 769;

define constant $GL-SRC-ALPHA = 770;

define constant $GL-ONE-MINUS-SRC-ALPHA = 771;

define constant $GL-DST-ALPHA = 772;

define constant $GL-ONE-MINUS-DST-ALPHA = 773;

define constant $GL-DST-COLOR = 774;

define constant $GL-ONE-MINUS-DST-COLOR = 775;

define constant $GL-SRC-ALPHA-SATURATE = 776;

define constant $GL-CONSTANT-COLOR = 32769;

define constant $GL-ONE-MINUS-CONSTANT-COLOR = 32770;

define constant $GL-CONSTANT-ALPHA = 32771;

define constant $GL-ONE-MINUS-CONSTANT-ALPHA = 32772;

define constant $GL-FEEDBACK = 7169;

define constant $GL-RENDER = 7168;

define constant $GL-SELECT = 7170;

define constant $GL-2D = 1536;

define constant $GL-3D = 1537;

define constant $GL-3D-COLOR = 1538;

define constant $GL-3D-COLOR-TEXTURE = 1539;

define constant $GL-4D-COLOR-TEXTURE = 1540;

define constant $GL-POINT-TOKEN = 1793;

define constant $GL-LINE-TOKEN = 1794;

define constant $GL-LINE-RESET-TOKEN = 1799;

define constant $GL-POLYGON-TOKEN = 1795;

define constant $GL-BITMAP-TOKEN = 1796;

define constant $GL-DRAW-PIXEL-TOKEN = 1797;

define constant $GL-COPY-PIXEL-TOKEN = 1798;

define constant $GL-PASS-THROUGH-TOKEN = 1792;

define constant $GL-FEEDBACK-BUFFER-POINTER = 3568;

define constant $GL-FEEDBACK-BUFFER-SIZE = 3569;

define constant $GL-FEEDBACK-BUFFER-TYPE = 3570;

define constant $GL-SELECTION-BUFFER-POINTER = 3571;

define constant $GL-SELECTION-BUFFER-SIZE = 3572;

define constant $GL-FOG = 2912;

define constant $GL-FOG-MODE = 2917;

define constant $GL-FOG-DENSITY = 2914;

define constant $GL-FOG-COLOR = 2918;

define constant $GL-FOG-INDEX = 2913;

define constant $GL-FOG-START = 2915;

define constant $GL-FOG-END = 2916;

define constant $GL-LINEAR = 9729;

define constant $GL-EXP = 2048;

define constant $GL-EXP2 = 2049;

define constant $GL-LOGIC-OP = 3057;

define constant $GL-INDEX-LOGIC-OP = 3057;

define constant $GL-COLOR-LOGIC-OP = 3058;

define constant $GL-LOGIC-OP-MODE = 3056;

define constant $GL-CLEAR = 5376;

define constant $GL-SET = 5391;

define constant $GL-COPY = 5379;

define constant $GL-COPY-INVERTED = 5388;

define constant $GL-NOOP = 5381;

define constant $GL-INVERT = 5386;

define constant $GL-AND = 5377;

define constant $GL-NAND = 5390;

define constant $GL-OR = 5383;

define constant $GL-NOR = 5384;

define constant $GL-XOR = 5382;

define constant $GL-EQUIV = 5385;

define constant $GL-AND-REVERSE = 5378;

define constant $GL-AND-INVERTED = 5380;

define constant $GL-OR-REVERSE = 5387;

define constant $GL-OR-INVERTED = 5389;

define constant $GL-STENCIL-TEST = 2960;

define constant $GL-STENCIL-WRITEMASK = 2968;

define constant $GL-STENCIL-BITS = 3415;

define constant $GL-STENCIL-FUNC = 2962;

define constant $GL-STENCIL-VALUE-MASK = 2963;

define constant $GL-STENCIL-REF = 2967;

define constant $GL-STENCIL-FAIL = 2964;

define constant $GL-STENCIL-PASS-DEPTH-PASS = 2966;

define constant $GL-STENCIL-PASS-DEPTH-FAIL = 2965;

define constant $GL-STENCIL-CLEAR-VALUE = 2961;

define constant $GL-STENCIL-INDEX = 6401;

define constant $GL-KEEP = 7680;

define constant $GL-REPLACE = 7681;

define constant $GL-INCR = 7682;

define constant $GL-DECR = 7683;

define constant $GL-NONE = 0;

define constant $GL-LEFT = 1030;

define constant $GL-RIGHT = 1031;

define constant $GL-FRONT-LEFT = 1024;

define constant $GL-FRONT-RIGHT = 1025;

define constant $GL-BACK-LEFT = 1026;

define constant $GL-BACK-RIGHT = 1027;

define constant $GL-AUX0 = 1033;

define constant $GL-AUX1 = 1034;

define constant $GL-AUX2 = 1035;

define constant $GL-AUX3 = 1036;

define constant $GL-COLOR-INDEX = 6400;

define constant $GL-RED = 6403;

define constant $GL-GREEN = 6404;

define constant $GL-BLUE = 6405;

define constant $GL-ALPHA = 6406;

define constant $GL-LUMINANCE = 6409;

define constant $GL-LUMINANCE-ALPHA = 6410;

define constant $GL-ALPHA-BITS = 3413;

define constant $GL-RED-BITS = 3410;

define constant $GL-GREEN-BITS = 3411;

define constant $GL-BLUE-BITS = 3412;

define constant $GL-INDEX-BITS = 3409;

define constant $GL-SUBPIXEL-BITS = 3408;

define constant $GL-AUX-BUFFERS = 3072;

define constant $GL-READ-BUFFER = 3074;

define constant $GL-DRAW-BUFFER = 3073;

define constant $GL-DOUBLEBUFFER = 3122;

define constant $GL-STEREO = 3123;

define constant $GL-BITMAP = 6656;

define constant $GL-COLOR = 6144;

define constant $GL-DEPTH = 6145;

define constant $GL-STENCIL = 6146;

define constant $GL-DITHER = 3024;

define constant $GL-RGB = 6407;

define constant $GL-RGBA = 6408;

define constant $GL-MAX-LIST-NESTING = 2865;

define constant $GL-MAX-ATTRIB-STACK-DEPTH = 3381;

define constant $GL-MAX-MODELVIEW-STACK-DEPTH = 3382;

define constant $GL-MAX-NAME-STACK-DEPTH = 3383;

define constant $GL-MAX-PROJECTION-STACK-DEPTH = 3384;

define constant $GL-MAX-TEXTURE-STACK-DEPTH = 3385;

define constant $GL-MAX-EVAL-ORDER = 3376;

define constant $GL-MAX-LIGHTS = 3377;

define constant $GL-MAX-CLIP-PLANES = 3378;

define constant $GL-MAX-TEXTURE-SIZE = 3379;

define constant $GL-MAX-PIXEL-MAP-TABLE = 3380;

define constant $GL-MAX-VIEWPORT-DIMS = 3386;

define constant $GL-MAX-CLIENT-ATTRIB-STACK-DEPTH = 3387;

define constant $GL-ATTRIB-STACK-DEPTH = 2992;

define constant $GL-CLIENT-ATTRIB-STACK-DEPTH = 2993;

define constant $GL-COLOR-CLEAR-VALUE = 3106;

define constant $GL-COLOR-WRITEMASK = 3107;

define constant $GL-CURRENT-INDEX = 2817;

define constant $GL-CURRENT-COLOR = 2816;

define constant $GL-CURRENT-NORMAL = 2818;

define constant $GL-CURRENT-RASTER-COLOR = 2820;

define constant $GL-CURRENT-RASTER-DISTANCE = 2825;

define constant $GL-CURRENT-RASTER-INDEX = 2821;

define constant $GL-CURRENT-RASTER-POSITION = 2823;

define constant $GL-CURRENT-RASTER-TEXTURE-COORDS = 2822;

define constant $GL-CURRENT-RASTER-POSITION-VALID = 2824;

define constant $GL-CURRENT-TEXTURE-COORDS = 2819;

define constant $GL-INDEX-CLEAR-VALUE = 3104;

define constant $GL-INDEX-MODE = 3120;

define constant $GL-INDEX-WRITEMASK = 3105;

define constant $GL-MODELVIEW-MATRIX = 2982;

define constant $GL-MODELVIEW-STACK-DEPTH = 2979;

define constant $GL-NAME-STACK-DEPTH = 3440;

define constant $GL-PROJECTION-MATRIX = 2983;

define constant $GL-PROJECTION-STACK-DEPTH = 2980;

define constant $GL-RENDER-MODE = 3136;

define constant $GL-RGBA-MODE = 3121;

define constant $GL-TEXTURE-MATRIX = 2984;

define constant $GL-TEXTURE-STACK-DEPTH = 2981;

define constant $GL-VIEWPORT = 2978;

define constant $GL-AUTO-NORMAL = 3456;

define constant $GL-MAP1-COLOR-4 = 3472;

define constant $GL-MAP1-GRID-DOMAIN = 3536;

define constant $GL-MAP1-GRID-SEGMENTS = 3537;

define constant $GL-MAP1-INDEX = 3473;

define constant $GL-MAP1-NORMAL = 3474;

define constant $GL-MAP1-TEXTURE-COORD-1 = 3475;

define constant $GL-MAP1-TEXTURE-COORD-2 = 3476;

define constant $GL-MAP1-TEXTURE-COORD-3 = 3477;

define constant $GL-MAP1-TEXTURE-COORD-4 = 3478;

define constant $GL-MAP1-VERTEX-3 = 3479;

define constant $GL-MAP1-VERTEX-4 = 3480;

define constant $GL-MAP2-COLOR-4 = 3504;

define constant $GL-MAP2-GRID-DOMAIN = 3538;

define constant $GL-MAP2-GRID-SEGMENTS = 3539;

define constant $GL-MAP2-INDEX = 3505;

define constant $GL-MAP2-NORMAL = 3506;

define constant $GL-MAP2-TEXTURE-COORD-1 = 3507;

define constant $GL-MAP2-TEXTURE-COORD-2 = 3508;

define constant $GL-MAP2-TEXTURE-COORD-3 = 3509;

define constant $GL-MAP2-TEXTURE-COORD-4 = 3510;

define constant $GL-MAP2-VERTEX-3 = 3511;

define constant $GL-MAP2-VERTEX-4 = 3512;

define constant $GL-COEFF = 2560;

define constant $GL-DOMAIN = 2562;

define constant $GL-ORDER = 2561;

define constant $GL-FOG-HINT = 3156;

define constant $GL-LINE-SMOOTH-HINT = 3154;

define constant $GL-PERSPECTIVE-CORRECTION-HINT = 3152;

define constant $GL-POINT-SMOOTH-HINT = 3153;

define constant $GL-POLYGON-SMOOTH-HINT = 3155;

define constant $GL-DONT-CARE = 4352;

define constant $GL-FASTEST = 4353;

define constant $GL-NICEST = 4354;

define constant $GL-SCISSOR-TEST = 3089;

define constant $GL-SCISSOR-BOX = 3088;

define constant $GL-MAP-COLOR = 3344;

define constant $GL-MAP-STENCIL = 3345;

define constant $GL-INDEX-SHIFT = 3346;

define constant $GL-INDEX-OFFSET = 3347;

define constant $GL-RED-SCALE = 3348;

define constant $GL-RED-BIAS = 3349;

define constant $GL-GREEN-SCALE = 3352;

define constant $GL-GREEN-BIAS = 3353;

define constant $GL-BLUE-SCALE = 3354;

define constant $GL-BLUE-BIAS = 3355;

define constant $GL-ALPHA-SCALE = 3356;

define constant $GL-ALPHA-BIAS = 3357;

define constant $GL-DEPTH-SCALE = 3358;

define constant $GL-DEPTH-BIAS = 3359;

define constant $GL-PIXEL-MAP-S-TO-S-SIZE = 3249;

define constant $GL-PIXEL-MAP-I-TO-I-SIZE = 3248;

define constant $GL-PIXEL-MAP-I-TO-R-SIZE = 3250;

define constant $GL-PIXEL-MAP-I-TO-G-SIZE = 3251;

define constant $GL-PIXEL-MAP-I-TO-B-SIZE = 3252;

define constant $GL-PIXEL-MAP-I-TO-A-SIZE = 3253;

define constant $GL-PIXEL-MAP-R-TO-R-SIZE = 3254;

define constant $GL-PIXEL-MAP-G-TO-G-SIZE = 3255;

define constant $GL-PIXEL-MAP-B-TO-B-SIZE = 3256;

define constant $GL-PIXEL-MAP-A-TO-A-SIZE = 3257;

define constant $GL-PIXEL-MAP-S-TO-S = 3185;

define constant $GL-PIXEL-MAP-I-TO-I = 3184;

define constant $GL-PIXEL-MAP-I-TO-R = 3186;

define constant $GL-PIXEL-MAP-I-TO-G = 3187;

define constant $GL-PIXEL-MAP-I-TO-B = 3188;

define constant $GL-PIXEL-MAP-I-TO-A = 3189;

define constant $GL-PIXEL-MAP-R-TO-R = 3190;

define constant $GL-PIXEL-MAP-G-TO-G = 3191;

define constant $GL-PIXEL-MAP-B-TO-B = 3192;

define constant $GL-PIXEL-MAP-A-TO-A = 3193;

define constant $GL-PACK-ALIGNMENT = 3333;

define constant $GL-PACK-LSB-FIRST = 3329;

define constant $GL-PACK-ROW-LENGTH = 3330;

define constant $GL-PACK-SKIP-PIXELS = 3332;

define constant $GL-PACK-SKIP-ROWS = 3331;

define constant $GL-PACK-SWAP-BYTES = 3328;

define constant $GL-UNPACK-ALIGNMENT = 3317;

define constant $GL-UNPACK-LSB-FIRST = 3313;

define constant $GL-UNPACK-ROW-LENGTH = 3314;

define constant $GL-UNPACK-SKIP-PIXELS = 3316;

define constant $GL-UNPACK-SKIP-ROWS = 3315;

define constant $GL-UNPACK-SWAP-BYTES = 3312;

define constant $GL-ZOOM-X = 3350;

define constant $GL-ZOOM-Y = 3351;

define constant $GL-TEXTURE-ENV = 8960;

define constant $GL-TEXTURE-ENV-MODE = 8704;

define constant $GL-TEXTURE-1D = 3552;

define constant $GL-TEXTURE-2D = 3553;

define constant $GL-TEXTURE-WRAP-S = 10242;

define constant $GL-TEXTURE-WRAP-T = 10243;

define constant $GL-TEXTURE-MAG-FILTER = 10240;

define constant $GL-TEXTURE-MIN-FILTER = 10241;

define constant $GL-TEXTURE-ENV-COLOR = 8705;

define constant $GL-TEXTURE-GEN-S = 3168;

define constant $GL-TEXTURE-GEN-T = 3169;

define constant $GL-TEXTURE-GEN-MODE = 9472;

define constant $GL-TEXTURE-BORDER-COLOR = 4100;

define constant $GL-TEXTURE-WIDTH = 4096;

define constant $GL-TEXTURE-HEIGHT = 4097;

define constant $GL-TEXTURE-BORDER = 4101;

define constant $GL-TEXTURE-COMPONENTS = 4099;

define constant $GL-TEXTURE-RED-SIZE = 32860;

define constant $GL-TEXTURE-GREEN-SIZE = 32861;

define constant $GL-TEXTURE-BLUE-SIZE = 32862;

define constant $GL-TEXTURE-ALPHA-SIZE = 32863;

define constant $GL-TEXTURE-LUMINANCE-SIZE = 32864;

define constant $GL-TEXTURE-INTENSITY-SIZE = 32865;

define constant $GL-NEAREST-MIPMAP-NEAREST = 9984;

define constant $GL-NEAREST-MIPMAP-LINEAR = 9986;

define constant $GL-LINEAR-MIPMAP-NEAREST = 9985;

define constant $GL-LINEAR-MIPMAP-LINEAR = 9987;

define constant $GL-OBJECT-LINEAR = 9217;

define constant $GL-OBJECT-PLANE = 9473;

define constant $GL-EYE-LINEAR = 9216;

define constant $GL-EYE-PLANE = 9474;

define constant $GL-SPHERE-MAP = 9218;

define constant $GL-DECAL = 8449;

define constant $GL-MODULATE = 8448;

define constant $GL-NEAREST = 9728;

define constant $GL-REPEAT = 10497;

define constant $GL-CLAMP = 10496;

define constant $GL-S = 8192;

define constant $GL-T = 8193;

define constant $GL-R = 8194;

define constant $GL-Q = 8195;

define constant $GL-TEXTURE-GEN-R = 3170;

define constant $GL-TEXTURE-GEN-Q = 3171;

define constant $GL-VENDOR = 7936;

define constant $GL-RENDERER = 7937;

define constant $GL-VERSION = 7938;

define constant $GL-EXTENSIONS = 7939;

define constant $GL-NO-ERROR = 0;

define constant $GL-INVALID-VALUE = 1281;

define constant $GL-INVALID-ENUM = 1280;

define constant $GL-INVALID-OPERATION = 1282;

define constant $GL-STACK-OVERFLOW = 1283;

define constant $GL-STACK-UNDERFLOW = 1284;

define constant $GL-OUT-OF-MEMORY = 1285;

define constant $GL-CURRENT-BIT = 1;

define constant $GL-POINT-BIT = 2;

define constant $GL-LINE-BIT = 4;

define constant $GL-POLYGON-BIT = 8;

define constant $GL-POLYGON-STIPPLE-BIT = 16;

define constant $GL-PIXEL-MODE-BIT = 32;

define constant $GL-LIGHTING-BIT = 64;

define constant $GL-FOG-BIT = 128;

define constant $GL-DEPTH-BUFFER-BIT = 256;

define constant $GL-ACCUM-BUFFER-BIT = 512;

define constant $GL-STENCIL-BUFFER-BIT = 1024;

define constant $GL-VIEWPORT-BIT = 2048;

define constant $GL-TRANSFORM-BIT = 4096;

define constant $GL-ENABLE-BIT = 8192;

define constant $GL-COLOR-BUFFER-BIT = 16384;

define constant $GL-HINT-BIT = 32768;

define constant $GL-EVAL-BIT = 65536;

define constant $GL-LIST-BIT = 131072;

define constant $GL-TEXTURE-BIT = 262144;

define constant $GL-SCISSOR-BIT = 524288;

define constant $GL-ALL-ATTRIB-BITS = 1048575;

define constant $GL-PROXY-TEXTURE-1D = 32867;

define constant $GL-PROXY-TEXTURE-2D = 32868;

define constant $GL-TEXTURE-PRIORITY = 32870;

define constant $GL-TEXTURE-RESIDENT = 32871;

define constant $GL-TEXTURE-BINDING-1D = 32872;

define constant $GL-TEXTURE-BINDING-2D = 32873;

define constant $GL-TEXTURE-INTERNAL-FORMAT = 4099;

define constant $GL-ALPHA4 = 32827;

define constant $GL-ALPHA8 = 32828;

define constant $GL-ALPHA12 = 32829;

define constant $GL-ALPHA16 = 32830;

define constant $GL-LUMINANCE4 = 32831;

define constant $GL-LUMINANCE8 = 32832;

define constant $GL-LUMINANCE12 = 32833;

define constant $GL-LUMINANCE16 = 32834;

define constant $GL-LUMINANCE4-ALPHA4 = 32835;

define constant $GL-LUMINANCE6-ALPHA2 = 32836;

define constant $GL-LUMINANCE8-ALPHA8 = 32837;

define constant $GL-LUMINANCE12-ALPHA4 = 32838;

define constant $GL-LUMINANCE12-ALPHA12 = 32839;

define constant $GL-LUMINANCE16-ALPHA16 = 32840;

define constant $GL-INTENSITY = 32841;

define constant $GL-INTENSITY4 = 32842;

define constant $GL-INTENSITY8 = 32843;

define constant $GL-INTENSITY12 = 32844;

define constant $GL-INTENSITY16 = 32845;

define constant $GL-R3-G3-B2 = 10768;

define constant $GL-RGB4 = 32847;

define constant $GL-RGB5 = 32848;

define constant $GL-RGB8 = 32849;

define constant $GL-RGB10 = 32850;

define constant $GL-RGB12 = 32851;

define constant $GL-RGB16 = 32852;

define constant $GL-RGBA2 = 32853;

define constant $GL-RGBA4 = 32854;

define constant $GL-RGB5-A1 = 32855;

define constant $GL-RGBA8 = 32856;

define constant $GL-RGB10-A2 = 32857;

define constant $GL-RGBA12 = 32858;

define constant $GL-RGBA16 = 32859;

define constant $GL-CLIENT-PIXEL-STORE-BIT = 1;

define constant $GL-CLIENT-VERTEX-ARRAY-BIT = 2;

define constant $GL-ALL-CLIENT-ATTRIB-BITS = #e4294967295;

define constant $GL-CLIENT-ALL-ATTRIB-BITS = #e4294967295;

define constant $GL-RESCALE-NORMAL = 32826;

define constant $GL-CLAMP-TO-EDGE = 33071;

define constant $GL-MAX-ELEMENTS-VERTICES = 33000;

define constant $GL-MAX-ELEMENTS-INDICES = 33001;

define constant $GL-BGR = 32992;

define constant $GL-BGRA = 32993;

define constant $GL-UNSIGNED-BYTE-3-3-2 = 32818;

define constant $GL-UNSIGNED-BYTE-2-3-3-REV = 33634;

define constant $GL-UNSIGNED-SHORT-5-6-5 = 33635;

define constant $GL-UNSIGNED-SHORT-5-6-5-REV = 33636;

define constant $GL-UNSIGNED-SHORT-4-4-4-4 = 32819;

define constant $GL-UNSIGNED-SHORT-4-4-4-4-REV = 33637;

define constant $GL-UNSIGNED-SHORT-5-5-5-1 = 32820;

define constant $GL-UNSIGNED-SHORT-1-5-5-5-REV = 33638;

define constant $GL-UNSIGNED-INT-8-8-8-8 = 32821;

define constant $GL-UNSIGNED-INT-8-8-8-8-REV = 33639;

define constant $GL-UNSIGNED-INT-10-10-10-2 = 32822;

define constant $GL-UNSIGNED-INT-2-10-10-10-REV = 33640;

define constant $GL-LIGHT-MODEL-COLOR-CONTROL = 33272;

define constant $GL-SINGLE-COLOR = 33273;

define constant $GL-SEPARATE-SPECULAR-COLOR = 33274;

define constant $GL-TEXTURE-MIN-LOD = 33082;

define constant $GL-TEXTURE-MAX-LOD = 33083;

define constant $GL-TEXTURE-BASE-LEVEL = 33084;

define constant $GL-TEXTURE-MAX-LEVEL = 33085;

define constant $GL-SMOOTH-POINT-SIZE-RANGE = 2834;

define constant $GL-SMOOTH-POINT-SIZE-GRANULARITY = 2835;

define constant $GL-SMOOTH-LINE-WIDTH-RANGE = 2850;

define constant $GL-SMOOTH-LINE-WIDTH-GRANULARITY = 2851;

define constant $GL-ALIASED-POINT-SIZE-RANGE = 33901;

define constant $GL-ALIASED-LINE-WIDTH-RANGE = 33902;

define constant $GL-PACK-SKIP-IMAGES = 32875;

define constant $GL-PACK-IMAGE-HEIGHT = 32876;

define constant $GL-UNPACK-SKIP-IMAGES = 32877;

define constant $GL-UNPACK-IMAGE-HEIGHT = 32878;

define constant $GL-TEXTURE-3D = 32879;

define constant $GL-PROXY-TEXTURE-3D = 32880;

define constant $GL-TEXTURE-DEPTH = 32881;

define constant $GL-TEXTURE-WRAP-R = 32882;

define constant $GL-MAX-3D-TEXTURE-SIZE = 32883;

define constant $GL-TEXTURE-BINDING-3D = 32874;

define constant $GL-COLOR-TABLE = 32976;

define constant $GL-POST-CONVOLUTION-COLOR-TABLE = 32977;

define constant $GL-POST-COLOR-MATRIX-COLOR-TABLE = 32978;

define constant $GL-PROXY-COLOR-TABLE = 32979;

define constant $GL-PROXY-POST-CONVOLUTION-COLOR-TABLE = 32980;

define constant $GL-PROXY-POST-COLOR-MATRIX-COLOR-TABLE = 32981;

define constant $GL-COLOR-TABLE-SCALE = 32982;

define constant $GL-COLOR-TABLE-BIAS = 32983;

define constant $GL-COLOR-TABLE-FORMAT = 32984;

define constant $GL-COLOR-TABLE-WIDTH = 32985;

define constant $GL-COLOR-TABLE-RED-SIZE = 32986;

define constant $GL-COLOR-TABLE-GREEN-SIZE = 32987;

define constant $GL-COLOR-TABLE-BLUE-SIZE = 32988;

define constant $GL-COLOR-TABLE-ALPHA-SIZE = 32989;

define constant $GL-COLOR-TABLE-LUMINANCE-SIZE = 32990;

define constant $GL-COLOR-TABLE-INTENSITY-SIZE = 32991;

define constant $GL-CONVOLUTION-1D = 32784;

define constant $GL-CONVOLUTION-2D = 32785;

define constant $GL-SEPARABLE-2D = 32786;

define constant $GL-CONVOLUTION-BORDER-MODE = 32787;

define constant $GL-CONVOLUTION-FILTER-SCALE = 32788;

define constant $GL-CONVOLUTION-FILTER-BIAS = 32789;

define constant $GL-REDUCE = 32790;

define constant $GL-CONVOLUTION-FORMAT = 32791;

define constant $GL-CONVOLUTION-WIDTH = 32792;

define constant $GL-CONVOLUTION-HEIGHT = 32793;

define constant $GL-MAX-CONVOLUTION-WIDTH = 32794;

define constant $GL-MAX-CONVOLUTION-HEIGHT = 32795;

define constant $GL-POST-CONVOLUTION-RED-SCALE = 32796;

define constant $GL-POST-CONVOLUTION-GREEN-SCALE = 32797;

define constant $GL-POST-CONVOLUTION-BLUE-SCALE = 32798;

define constant $GL-POST-CONVOLUTION-ALPHA-SCALE = 32799;

define constant $GL-POST-CONVOLUTION-RED-BIAS = 32800;

define constant $GL-POST-CONVOLUTION-GREEN-BIAS = 32801;

define constant $GL-POST-CONVOLUTION-BLUE-BIAS = 32802;

define constant $GL-POST-CONVOLUTION-ALPHA-BIAS = 32803;

define constant $GL-CONSTANT-BORDER = 33105;

define constant $GL-REPLICATE-BORDER = 33107;

define constant $GL-CONVOLUTION-BORDER-COLOR = 33108;

define constant $GL-COLOR-MATRIX = 32945;

define constant $GL-COLOR-MATRIX-STACK-DEPTH = 32946;

define constant $GL-MAX-COLOR-MATRIX-STACK-DEPTH = 32947;

define constant $GL-POST-COLOR-MATRIX-RED-SCALE = 32948;

define constant $GL-POST-COLOR-MATRIX-GREEN-SCALE = 32949;

define constant $GL-POST-COLOR-MATRIX-BLUE-SCALE = 32950;

define constant $GL-POST-COLOR-MATRIX-ALPHA-SCALE = 32951;

define constant $GL-POST-COLOR-MATRIX-RED-BIAS = 32952;

define constant $GL-POST-COLOR-MATRIX-GREEN-BIAS = 32953;

define constant $GL-POST-COLOR-MATRIX-BLUE-BIAS = 32954;

define constant $GL-POST-COLOR-MATRIX-ALPHA-BIAS = 32955;

define constant $GL-HISTOGRAM = 32804;

define constant $GL-PROXY-HISTOGRAM = 32805;

define constant $GL-HISTOGRAM-WIDTH = 32806;

define constant $GL-HISTOGRAM-FORMAT = 32807;

define constant $GL-HISTOGRAM-RED-SIZE = 32808;

define constant $GL-HISTOGRAM-GREEN-SIZE = 32809;

define constant $GL-HISTOGRAM-BLUE-SIZE = 32810;

define constant $GL-HISTOGRAM-ALPHA-SIZE = 32811;

define constant $GL-HISTOGRAM-LUMINANCE-SIZE = 32812;

define constant $GL-HISTOGRAM-SINK = 32813;

define constant $GL-MINMAX = 32814;

define constant $GL-MINMAX-FORMAT = 32815;

define constant $GL-MINMAX-SINK = 32816;

define constant $GL-TABLE-TOO-LARGE = 32817;

define constant $GL-BLEND-EQUATION = 32777;

define constant $GL-MIN = 32775;

define constant $GL-MAX = 32776;

define constant $GL-FUNC-ADD = 32774;

define constant $GL-FUNC-SUBTRACT = 32778;

define constant $GL-FUNC-REVERSE-SUBTRACT = 32779;

define constant $GL-BLEND-COLOR = 32773;

define constant $GL-TEXTURE0 = 33984;

define constant $GL-TEXTURE1 = 33985;

define constant $GL-TEXTURE2 = 33986;

define constant $GL-TEXTURE3 = 33987;

define constant $GL-TEXTURE4 = 33988;

define constant $GL-TEXTURE5 = 33989;

define constant $GL-TEXTURE6 = 33990;

define constant $GL-TEXTURE7 = 33991;

define constant $GL-TEXTURE8 = 33992;

define constant $GL-TEXTURE9 = 33993;

define constant $GL-TEXTURE10 = 33994;

define constant $GL-TEXTURE11 = 33995;

define constant $GL-TEXTURE12 = 33996;

define constant $GL-TEXTURE13 = 33997;

define constant $GL-TEXTURE14 = 33998;

define constant $GL-TEXTURE15 = 33999;

define constant $GL-TEXTURE16 = 34000;

define constant $GL-TEXTURE17 = 34001;

define constant $GL-TEXTURE18 = 34002;

define constant $GL-TEXTURE19 = 34003;

define constant $GL-TEXTURE20 = 34004;

define constant $GL-TEXTURE21 = 34005;

define constant $GL-TEXTURE22 = 34006;

define constant $GL-TEXTURE23 = 34007;

define constant $GL-TEXTURE24 = 34008;

define constant $GL-TEXTURE25 = 34009;

define constant $GL-TEXTURE26 = 34010;

define constant $GL-TEXTURE27 = 34011;

define constant $GL-TEXTURE28 = 34012;

define constant $GL-TEXTURE29 = 34013;

define constant $GL-TEXTURE30 = 34014;

define constant $GL-TEXTURE31 = 34015;

define constant $GL-ACTIVE-TEXTURE = 34016;

define constant $GL-CLIENT-ACTIVE-TEXTURE = 34017;

define constant $GL-MAX-TEXTURE-UNITS = 34018;

define constant $GL-NORMAL-MAP = 34065;

define constant $GL-REFLECTION-MAP = 34066;

define constant $GL-TEXTURE-CUBE-MAP = 34067;

define constant $GL-TEXTURE-BINDING-CUBE-MAP = 34068;

define constant $GL-TEXTURE-CUBE-MAP-POSITIVE-X = 34069;

define constant $GL-TEXTURE-CUBE-MAP-NEGATIVE-X = 34070;

define constant $GL-TEXTURE-CUBE-MAP-POSITIVE-Y = 34071;

define constant $GL-TEXTURE-CUBE-MAP-NEGATIVE-Y = 34072;

define constant $GL-TEXTURE-CUBE-MAP-POSITIVE-Z = 34073;

define constant $GL-TEXTURE-CUBE-MAP-NEGATIVE-Z = 34074;

define constant $GL-PROXY-TEXTURE-CUBE-MAP = 34075;

define constant $GL-MAX-CUBE-MAP-TEXTURE-SIZE = 34076;

define constant $GL-COMPRESSED-ALPHA = 34025;

define constant $GL-COMPRESSED-LUMINANCE = 34026;

define constant $GL-COMPRESSED-LUMINANCE-ALPHA = 34027;

define constant $GL-COMPRESSED-INTENSITY = 34028;

define constant $GL-COMPRESSED-RGB = 34029;

define constant $GL-COMPRESSED-RGBA = 34030;

define constant $GL-TEXTURE-COMPRESSION-HINT = 34031;

define constant $GL-TEXTURE-COMPRESSED-IMAGE-SIZE = 34464;

define constant $GL-TEXTURE-COMPRESSED = 34465;

define constant $GL-NUM-COMPRESSED-TEXTURE-FORMATS = 34466;

define constant $GL-COMPRESSED-TEXTURE-FORMATS = 34467;

define constant $GL-MULTISAMPLE = 32925;

define constant $GL-SAMPLE-ALPHA-TO-COVERAGE = 32926;

define constant $GL-SAMPLE-ALPHA-TO-ONE = 32927;

define constant $GL-SAMPLE-COVERAGE = 32928;

define constant $GL-SAMPLE-BUFFERS = 32936;

define constant $GL-SAMPLES = 32937;

define constant $GL-SAMPLE-COVERAGE-VALUE = 32938;

define constant $GL-SAMPLE-COVERAGE-INVERT = 32939;

define constant $GL-MULTISAMPLE-BIT = 536870912;

define constant $GL-TRANSPOSE-MODELVIEW-MATRIX = 34019;

define constant $GL-TRANSPOSE-PROJECTION-MATRIX = 34020;

define constant $GL-TRANSPOSE-TEXTURE-MATRIX = 34021;

define constant $GL-TRANSPOSE-COLOR-MATRIX = 34022;

define constant $GL-COMBINE = 34160;

define constant $GL-COMBINE-RGB = 34161;

define constant $GL-COMBINE-ALPHA = 34162;

define constant $GL-SOURCE0-RGB = 34176;

define constant $GL-SOURCE1-RGB = 34177;

define constant $GL-SOURCE2-RGB = 34178;

define constant $GL-SOURCE0-ALPHA = 34184;

define constant $GL-SOURCE1-ALPHA = 34185;

define constant $GL-SOURCE2-ALPHA = 34186;

define constant $GL-OPERAND0-RGB = 34192;

define constant $GL-OPERAND1-RGB = 34193;

define constant $GL-OPERAND2-RGB = 34194;

define constant $GL-OPERAND0-ALPHA = 34200;

define constant $GL-OPERAND1-ALPHA = 34201;

define constant $GL-OPERAND2-ALPHA = 34202;

define constant $GL-RGB-SCALE = 34163;

define constant $GL-ADD-SIGNED = 34164;

define constant $GL-INTERPOLATE = 34165;

define constant $GL-SUBTRACT = 34023;

define constant $GL-CONSTANT = 34166;

define constant $GL-PRIMARY-COLOR = 34167;

define constant $GL-PREVIOUS = 34168;

define constant $GL-DOT3-RGB = 34478;

define constant $GL-DOT3-RGBA = 34479;

define constant $GL-CLAMP-TO-BORDER = 33069;

define constant $GL-MESA-trace = 1;

define constant $GL-TRACE-ALL-BITS-MESA = 65535;

define constant $GL-TRACE-OPERATIONS-BIT-MESA = 1;

define constant $GL-TRACE-PRIMITIVES-BIT-MESA = 2;

define constant $GL-TRACE-ARRAYS-BIT-MESA = 4;

define constant $GL-TRACE-TEXTURES-BIT-MESA = 8;

define constant $GL-TRACE-PIXELS-BIT-MESA = 16;

define constant $GL-TRACE-ERRORS-BIT-MESA = 32;

define constant $GL-TRACE-MASK-MESA = 34645;

define constant $GL-TRACE-NAME-MESA = 34646;

define constant $GL-MESA-packed-depth-stencil = 1;

define constant $GL-DEPTH-STENCIL-MESA = 34640;

define constant $GL-UNSIGNED-INT-24-8-MESA = 34641;

define constant $GL-UNSIGNED-INT-8-24-REV-MESA = 34642;

define constant $GL-UNSIGNED-SHORT-15-1-MESA = 34643;

define constant $GL-UNSIGNED-SHORT-1-15-REV-MESA = 34644;

define constant $GL-MESA-ycbcr-texture = 1;

define constant $GL-YCBCR-MESA = 34647;

define constant $GL-UNSIGNED-SHORT-8-8-MESA = 34234;

define constant $GL-UNSIGNED-SHORT-8-8-REV-MESA = 34235;

define constant $GL-MESA-pack-invert = 1;

define constant $GL-PACK-INVERT-MESA = 34648;

define constant $GL-APPLE-client-storage = 1;

define constant $GL-UNPACK-CLIENT-STORAGE-APPLE = 34226;

define constant $GL-APPLE-ycbcr-422 = 1;

define constant $GL-YCBCR-422-APPLE = 34233;

define constant $GL-UNSIGNED-SHORT-8-8-APPLE = 34234;

define constant $GL-UNSIGNED-SHORT-8-8-REV-APPLE = 34235;


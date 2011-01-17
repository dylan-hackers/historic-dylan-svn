module: pdf
synopsis: Stream-based PDF 
author: 
copyright: 

// State management

define method push-state( stream :: <pdf-stream> )
=>()
    write-line( stream, "q" );

    values();
end method push-state;

define method pop-state( stream :: <pdf-stream> )
=>()
    write-line( stream, "Q" );

    values();
end method pop-state;

// PDF Reference Manual 1.3 gives 5 decimal places as the approximate accuracy limit for reals.
define constant $pdf-real-decimal-place-limit :: <integer> = 6;

define method concatenate-matrix( stream :: <pdf-stream>, a :: <float>, b :: <float>, c :: <float>,
                               d :: <float>, e :: <float>, f :: <float> )
=>()
    let a$ = as( <string>, round-float( a, $pdf-real-decimal-place-limit ) );
    let b$ = as( <string>, round-float( b, $pdf-real-decimal-place-limit ) );
    let c$ = as( <string>, round-float( c, $pdf-real-decimal-place-limit ) );
    let d$ = as( <string>, round-float( d, $pdf-real-decimal-place-limit ) );
    let e$ = as( <string>, round-float( e, $pdf-real-decimal-place-limit ) );
    let f$ = as( <string>, round-float( f, $pdf-real-decimal-place-limit ) );
    
    format( stream, "%s %s %s %s %s %s cm\n", a$, b$, c$, d$, e$, f$ );

    values();
end method concatenate-matrix;

define method line-width( stream :: <pdf-stream>, width :: <float> )
=>()
    format( stream, "%s w\n", as( <string>, width ) );
    
    values();
end method line-width;

define method line-cap( stream :: <pdf-stream>, cap :: <integer> )
=>()
    format( stream, "%d J\n", cap );

    values();
end method line-cap;

define method line-join( stream :: <pdf-stream>, join :: <integer> )
=>()
    format( stream, "%d j\n", join );

    values();
end method line-join;

define method miter-limit( stream :: <pdf-stream>, limit :: <float> )
=>()
    format( stream, "%s M\n", as( <string>, limit ) );

    values();
end method miter-limit;

define method dash-pattern( stream :: <pdf-stream>, dash :: <pdf-array>, phase :: <integer> )
=>()
    write-pdf-object( stream, dash, 0 );
    format( stream, " %d d\n", phase );

    values();
end method dash-pattern;

define method flatness-tolerance( stream :: <pdf-stream>, tolerance :: <float> )
=>()
    format( stream, "%s i\n", as( <string>, tolerance ) );

    values();
end method flatness-tolerance;

define method transform-matrix( stream :: <pdf-stream>, a :: <float>, b :: <float>, c :: <float>,
                               d :: <float>, e :: <float>, f :: <float> )
=>()
    concatenate-matrix( stream, a, b, c, d, e, f );

    values();
end method transform-matrix;

define method translate-matrix( stream :: <pdf-stream>, x :: <float>, y :: <float> )
=> ()
    concatenate-matrix( stream, 1.0, 0.0, 0.0, 1.0, x, y );

    values;
end method translate-matrix;

define method scale-matrix( stream :: <pdf-stream>, x :: <float>, y :: <float> )
=> ()
    concatenate-matrix( stream, x, 0.0, 0.0, y, 0.0, 0.0 );

    values;
end method scale-matrix;

define method rotate-matrix( stream :: <pdf-stream>, degrees :: <float> )
=> ()
    let degrees-clockwise = modulo( (- degrees), 360.0 );
    let ct = cos( degrees-clockwise * 0.0174532925 );
    let st = sin( degrees-clockwise * 0.0174532925 );
    
    concatenate-matrix( stream, ct, st, - st, ct, 0.0, 0.0 );

    values;
end method rotate-matrix;

define method skew-matrix( stream :: <pdf-stream>, alpha :: <float>, beta :: <float> )
=> ()
    concatenate-matrix( stream, 1.0, tan( alpha ), tan( beta ), 1.0, 0.0, 0.0 );

    values;
end method skew-matrix;


// Paths

define method move-to( stream :: <pdf-stream>, x :: <float>, y :: <float> )
=> ()
    
    format( stream, "%s %s m\n", 
                as( <string>, x ), 
                as( <string>, y ) );

    values();
end method move-to;

define method append-line( stream :: <pdf-stream>, x :: <float>, y :: <float> )
=> ()
    
    format( stream, "%s %s l\n", 
                as( <string>, x ), 
                as( <string>, y ) );

    values();
end method append-line;

define method append-bezier-path( stream :: <pdf-stream>, x1 :: <float>, y1 :: <float>, 
                                x2 :: <float>, y2 :: <float>, x3 :: <float>, y3 :: <float> )
=> ()
    
    format( stream, "%s %s %s %s %s %s c\n", 
                as( <string>, x1 ), 
                as( <string>, y1 ),
                as( <string>, x2 ), 
                as( <string>, y2 ),
                as( <string>, x3 ), 
                as( <string>, y3 )
                 );

    values();
end method append-bezier-path;

// Find better names for these two!

define method append-bezier-path2( stream :: <pdf-stream>,
                                x2 :: <float>, y2 :: <float>, x3 :: <float>, y3 :: <float> )
=> ()
    
    format( stream, "%s %s %s %s v\n", 
                as( <string>, x2 ), 
                as( <string>, y2 ),
                as( <string>, x3 ), 
                as( <string>, y3 )
                 );

    values();
end method append-bezier-path2;

define method append-bezier-path3( stream :: <pdf-stream>, x1 :: <float>, y1 :: <float>, 
                                x2 :: <float>, y2 :: <float> )
=> ()
    
    format( stream, "%s %s %s %s y\n", 
                as( <string>, x1 ), 
                as( <string>, y1 ),
                as( <string>, x2 ), 
                as( <string>, y2 )
                 );

    values();
end method append-bezier-path3;

define method close-subpath( stream :: <pdf-stream> )
=>()
    write-line( stream, "h" );

    values();
end method close-subpath;

define method rectangle-path( stream :: <pdf-stream>, #key x :: <float>, y :: <float>, 
                                width :: <float>, height :: <float> )
=> ()
    
    format( stream, "%s %s %s %s re\n", 
                as( <string>, x ), 
                as( <string>, y ),
                as( <string>, width ), 
                as( <string>, height )
                 );

    values();
end method rectangle-path;


// Path painting

define method stroke-path( stream :: <pdf-stream> )
=>()
    write-line( stream, "S" );

    values();
end method stroke-path;

define method close-and-stroke-path( stream :: <pdf-stream> )
=>()
    write-line( stream, "s" );

    values();
end method close-and-stroke-path;

define method fill-path( stream :: <pdf-stream> )
=>()
    write-line( stream, "f" );

    values();
end method fill-path;

define method fill-path-even-odd-winding( stream :: <pdf-stream> )
=>()
    write-line( stream, "f*" );

    values();
end method fill-path-even-odd-winding;


// Colour

define method gray-fill( stream :: <pdf-stream>, g :: <float> )
=> ()
    
    format( stream, "%s g\n", 
                as( <string>, g ) );

    values();
end method gray-fill;

define method gray-stroke( stream :: <pdf-stream>, g :: <float> )
=> ()
    
    format( stream, "%s G\n", 
                as( <string>, g ) );

    values();
end method gray-stroke;

define method rgb-fill( stream :: <pdf-stream>, r :: <float>, g :: <float>, b :: <float> )
=> ()
    
    format( stream, "%s %s %s rg\n", 
                as( <string>, r ), 
                as( <string>, g ), 
                as( <string>, b ) );

    values();
end method rgb-fill;

define method rgb-stroke( stream :: <pdf-stream>, r :: <float>, g :: <float>, b :: <float> )
=> ()
    
    format( stream, "%s %s %s RG\n", 
                as( <string>, r ), 
                as( <string>, g ), 
                as( <string>, b ) );

    values();
end method rgb-stroke;

define method cmyk-fill( stream :: <pdf-stream>, c :: <float>, m :: <float>, y :: <float>, k :: <float> )
=> ()
    
    format( stream, "%s %s %s %s k\n", 
                as( <string>, c ), 
                as( <string>, m ), 
                as( <string>, y ), 
                as( <string>, k ) );

    values();
end method cmyk-fill;

define method cmyk-stroke( stream :: <pdf-stream>, c :: <float>, m :: <float>, y :: <float>, k :: <float> )
=> ()
    
    format( stream, "%s %s %s %s K\n", 
                as( <string>, c ), 
                as( <string>, m ), 
                as( <string>, y ), 
                as( <string>, k ) );

    values();
end method cmyk-stroke;

define function read-colour-file( filename :: <string> = "pcl.colours" )
=> ( colours :: <map> )
	let colours :: <table> = make(<table>);
	let line :: <string> = "";
	with-open-file(file = filename)
	  while(line := read-line(file, on-end-of-stream: #f))
		unless( line[ 0 ] = '.' )
			let character :: <character> = line[ 0 ];
			let colours :: type-union( <colour>, <vector) = parse-colours( line );
		end unless
	  end while;
	end;
end function read-colour-file;

define function parse-colours( source :: <string> )
=> ( type-union( <colour>, <vector) )
	let source-s 
	let candidate :: <string> = "";
	while()

end function parse-colours;

// Text Objects

define method text-begin( stream :: <pdf-stream> )
=>()
    write-line( stream, "BT" );

    values();
end method text-begin;

define method text-end( stream :: <pdf-stream> )
=>()
    write-line( stream, "ET" );

    values();
end method text-end;

define method text-font( stream :: <pdf-stream>, font :: <pdf-name>, size :: <float> )
=> ()
    format( stream, "/%s %s Tf\n", as( <pdf-name>, font ),
                as( <string>, size ) );

    values();
end method text-font;

define method text-position( stream :: <pdf-stream>, x :: <float>, y :: <float> )
=> ()
    format( stream, "%s %s Td\n", 
                as( <string>, x ), 
                as( <string>, y ) );

    values();
end method text-position;

define method text-character-spacing( stream :: <pdf-stream>, spacing :: <float> )
=> ()
    format( stream, "%s Tc\n", as( <string>, spacing ) );

    values();
end method text-character-spacing;

define method text-leading( stream :: <pdf-stream>, leading :: <float> )
=> ()
    format( stream, "%s TL\n", as( <string>, leading ) );

    values();
end method text-leading;

define method text-word-spacing( stream :: <pdf-stream>, spacing :: <float> )
=> ()
    format( stream, "%s Tw\n", as( <string>, spacing ) );

    values();
end method text-word-spacing;

define method text-scale( stream :: <pdf-stream>, scale :: <float> )
=> ()
    format( stream, "%s Tz\n", as( <string>, scale ) );

    values();
end method text-scale;

define method text-rise( stream :: <pdf-stream>, rise :: <float> )
=> ()
    format( stream, "%s Ts\n", as( <string>, rise ) );

    values();
end method text-rise;

define method text-rendering-mode( stream :: <pdf-stream>, mode :: <integer> )
=> ()
    format( stream, "%d Tr\n", mode );

    values();
end method text-rendering-mode;

define method text-show( stream :: <pdf-stream>, line :: <string> )
=> ()
    format( stream, "(%s) Tj\n", line );

    values();
end method text-show;

define method text-show-next-line( stream :: <pdf-stream>, line :: <string> )
=> ()
    
    format( stream, "(%s) '\n", line );

    values();
end method text-show-next-line;

define method text-next-line( stream :: <pdf-stream> )
=>()
    write-line( stream, "T*" );

    values();
end method text-next-line;


// Utilities

define constant $arc-factor :: <float> = 0.552284749;

define method circle-path( stream :: <stream>, x :: <float>, y :: <float>, radius :: <float>  ) 
=> ()
    move-to( stream, x + radius, y);
    append-bezier-path( stream, x + radius, y + radius * $arc-factor, x + radius * $arc-factor, y + radius, x, y + radius);
    append-bezier-path( stream, x - radius * $arc-factor, y + radius, x - radius, y + radius * $arc-factor, x - radius, y);
    append-bezier-path( stream, x - radius, y - radius * $arc-factor, x - radius * $arc-factor, y - radius, x, y - radius);
    append-bezier-path( stream, x + radius * $arc-factor, y - radius, x + radius, y - radius * $arc-factor, x + radius, y);
    close-subpath( stream );
    
    values();
end method circle-path;



////////////////////////////////////////////////////////////////////////////////
// round-float
// Rounds a float to n decimal places
// Thanks to P. T. Withington for the code.
////////////////////////////////////////////////////////////////////////////////

define method round-float( number :: <float>, n :: <integer> )
=> ( float :: <float> )
    as( <float>,  round/( number, (10.0 ^ - n) ) ) / as( <float>, (10.0 ^ n) );
end method round-float;

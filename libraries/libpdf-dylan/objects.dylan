module: pdf
synopsis: Simple PDF object classes and methods.
author: Rob Myers. dylan@robmyers.org
copyright: Standard Gwydion Dylan.


// <pdf-object>

define constant <pdf-object> = <object>;/*type-union(  <pdf-boolean>,
                                            <pdf-integer>, 
                                            <pdf-real>,
                                            <pdf-string>,
                                            <pdf-name>,
                                            <pdf-null>
                                            <pdf-indirect-object>,
                                            <pdf-array>,
                                            <pdf-dictionary>,
                                            <pdf-stream> );*/


// Simple mappings

define constant <pdf-boolean> = <boolean>;
define constant <pdf-integer> = <integer>;
define constant <pdf-real> = <float>;
define constant <pdf-string> = <string>;

// WARNING: Declare as(<pdf-name>, "hello"), not #"hello", to maintain case!
define constant <pdf-name> = <symbol>;


// <pdf-null>

define class <pdf-null> (<object>)
end class <pdf-null>;


// <pdf-indirect-object>
// WARNING - This assumes that indirect objects must be declared at top-level. Is this true?

define class <pdf-indirect-object> (<pdf-object>)
    slot object-number :: <integer> = 0;
    slot generation-number :: <integer> = 0;
    slot value :: false-or( <pdf-object> ) = #f; 
end class <pdf-indirect-object>;

define method initialize( indirect :: <pdf-indirect-object>, #key document :: <pdf-document>, 
                    value :: <pdf-object>, generation :: <integer> = 0, #all-keys )
=> ( indirect :: <pdf-indirect-object> )
    next-method();
    
    indirect.object-number := document.object-number;
    document.object-number := document.object-number + 1;
    
    indirect.generation-number := generation;
    
    indirect.value := value;
    
    indirect;
end method initialize;


// as <pdf-real> -> <string>

define method as( type == <string>, object :: <pdf-real> )
=> ( result :: <string> )
    let object-string-stream :: <string-stream> = make( <string-stream>, direction: #"output" );
    write-pdf-object( object-string-stream, object, 0 );
    object-string-stream.stream-contents;
end method as;


// write-pdf-object

define generic write-pdf-object( stream :: <stream>, object :: <pdf-object>, depth :: <integer> )
=> ();

define method write-pdf-object( stream :: <stream>, object :: <pdf-boolean>, depth :: <integer> ) 
=> ()
    if( value )
        write( stream, "true" );
    else
        write( stream, "false" );
    end if;
    values();
end method write-pdf-object;

define method write-pdf-object( stream :: <stream>, object :: <pdf-integer>, depth :: <integer> ) 
=> ()
    format( stream, "%d", object );

    values();
end method write-pdf-object;

define method write-pdf-object( stream :: <stream>, object :: <pdf-real>, depth :: <integer> ) 
=> ()
    let float-string = format-to-string( "%=", object );
    float-string := copy-sequence( float-string, start: 0, end: float-string.size - 2 );
    write( stream, float-string );

    values();
end method write-pdf-object;

define method write-pdf-object( stream :: <stream>, object :: <pdf-string>, depth :: <integer> ) 
=> ()
    format( stream, "(%s)", object );

    values();
end method write-pdf-object;

define method write-pdf-object( stream :: <stream>, object :: <pdf-name>, depth :: <integer> ) 
=> ()
    format( stream, "/%s", object );

    values();
end method write-pdf-object;

define method write-pdf-object( stream :: <stream>, object :: <pdf-null>, depth :: <integer> ) 
=> ()
    write( stream, "null" );

    values();
end method write-pdf-object;

define method write-pdf-object( stream :: <stream>, indirect-object :: <pdf-indirect-object>, depth :: <integer> )
=> ()
    if( depth > 0 )
        format( stream, "%d %d R", indirect-object.object-number, indirect-object.generation-number );
    else
        format( stream, "%d %d obj\n", indirect-object.object-number, indirect-object.generation-number );
        write-pdf-object( stream, indirect-object.value, depth + 1 );
        write-line( stream, "endobj" );
    end if;
    
    values();
end method write-pdf-object;
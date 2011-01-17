module: pdf
synopsis: PDF array class and methods.
author: Rob Myers. dylan@robmyers.org
copyright: Standard Gwydion Dylan.


// <pdf-array>

define constant <pdf-array> = <stretchy-vector>;

define method write-pdf-object( stream :: <stream>, array :: <pdf-array>, depth :: <integer> )
=> ()
    write( stream, "[" );
    for( item in array )
        write-pdf-object( stream, item, depth + 1 );
        write( stream, " " );
    end for;
    write( stream, "]" );
    
    values();
end method write-pdf-object;


// make-pdf-rectangle
// FIXME Use reals rather than integers?

define method make-pdf-rectangle( #key top :: <pdf-real>, left :: <pdf-real>, bottom :: <pdf-real>, right :: <pdf-real> )
=> ( result :: <pdf-array> )
    let rectangle :: <pdf-array> = make( <pdf-array> );
    
    add!( rectangle, left );
    add!( rectangle, bottom );
    add!( rectangle, right );
    add!( rectangle, top );

    rectangle;
end method make-pdf-rectangle;
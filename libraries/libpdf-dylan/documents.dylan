module: pdf
synopsis: PDF document class and methods
author: Rob Myers. dylan@robmyers.org
copyright: Standard Gwydion Dylan. 


// <pdf-document>

define class <pdf-document> (<object>)
    slot catalog :: <pdf-indirect-object>; 
    slot outline-tree-root :: <pdf-indirect-object>;
    slot page-tree-root :: <pdf-indirect-object>;
    slot body :: <stretchy-vector> = make( <stretchy-vector> );
    slot proc-set :: <pdf-indirect-object>;
    slot object-number :: <integer> = 1;
end class <pdf-document>;


// initialize <pdf-document>

define method initialize( document :: <pdf-document>, #key, #all-keys )
=> ( result :: <pdf-document> )
    next-method();
    
    document.catalog := make-pdf-catalog( document );
    document.outline-tree-root := make-pdf-outline-tree( document );
    document.catalog.value[ "Outlines" ] := document.outline-tree-root;
    document.page-tree-root := make-pdf-page-tree( document );
    document.catalog.value[ "Pages" ] := document.page-tree-root;
    
    let backwards-compatibility-proc-set :: <pdf-array> = make( <pdf-array> );
    add!( backwards-compatibility-proc-set, as( <pdf-name>, "PDF" ) );
    document.proc-set := make( <pdf-indirect-object>, value: backwards-compatibility-proc-set, document: document );
    
    document;
end method initialize;


// write-pdf-document

define method write-pdf-document( stream :: <stream>, document :: <pdf-document> )
=> ()
    // The list of indirect object references, to be built whilst writing the body
    let xrefs :: <stretchy-vector> = make( <stretchy-vector> );
    // Add the free entry for object 0
    add!( xrefs, "0000000000 65535 f " );
    
    // Write the pdf document header
    write-line( stream, $pdf-header );

    // Write the catalog
    write-pdf-document-object( document.catalog, stream, xrefs );
    
    // Write the outline tree root
    write-pdf-document-object( document.outline-tree-root, stream, xrefs );
    // Write the page tree root
    write-pdf-document-object( document.page-tree-root, stream, xrefs );
    // Write the compatibility procset
    write-pdf-document-object( document.proc-set, stream, xrefs );
    
    // Write the main document body
    for( object in document.body )
        write-pdf-document-object( object, stream, xrefs );
    end for;
    
    // Write the cross-reference table
    let startxref :: <integer> = stream.stream-position;
    format( stream, "xref\n%d %d\n", 0, xrefs.size );
    for( xref in xrefs )
        write-line( stream, xref );
    end for;
    values();
    
    // Write trailer.
    format( stream, "trailer\n<< /Size %d\n/Root %d %d R\n>>\nstartxref\n%d\n%%%%EOF",
            document.object-number + 1, 
            document.catalog.object-number,
            document.catalog.generation-number,
            startxref );

    values();
end method write-pdf-document;


// write-pdf-document-object

define method write-pdf-document-object( object :: <pdf-object>, stream :: <stream>, xrefs :: <stretchy-vector> )
=> ()
        if( instance?( object, <pdf-indirect-object> ) )
            add!( xrefs, format-to-string( "%s %s n ", // Trailing space mandated by PDF standard 
                            zero-padded-integer( stream.stream-position, 10 ),
                            zero-padded-integer( object.generation-number, 5 ) ) );
        end if;
        write-pdf-object( stream, object, 0 );
    values();
end method write-pdf-document-object;


// zero-padded-integer

define method zero-padded-integer( int :: <integer>, length :: <integer> )
=> ( string :: <string> )
    let string :: <string> = format-to-string( "%d", int );
    
    if( string.size < length )
        let shortfall = (length - string.size) - 1;
        for( i from 0 to shortfall )
            string := concatenate!( "0", string );
        end for;
    end if;
    
    string;
end method zero-padded-integer;
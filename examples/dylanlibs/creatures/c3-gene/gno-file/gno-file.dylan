Module:    gno-file
Synopsis:  Routines for reading Creatures 2/3 .gno files.
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.
License:   See License.txt

define method gno-file-version () => (full-name :: <byte-string>)
  "1.1.1";
end method gno-file-version;

define class <gno-documentation> (<object>)
    slot gno-type, init-keyword: type:;
    slot gno-subtype, init-keyword: subtype:;
    slot gno-number, init-keyword: number:;
    slot gno-description, init-keyword: description:;
    slot gno-comment, init-keyword: comment:;
end class <gno-documentation>;

// C3 has additional documentation
define class <gno-extra-documentation> (<object>)
    slot gno-type, init-keyword: type:;
    slot gno-subtype, init-keyword: subtype:;
    slot gno-sequence, init-keyword: sequence:;
    slot gno-number, init-keyword: number:;
    slot gno-comments, init-keyword: comments:;
end class;

define method load-gno-file ( filename, #key version ) => (gno)
  with-open-file( fs = filename, element-type: <byte> )
    do-load-gno-file( fs, make(<stretchy-vector>), version )
  end with-open-file;
end method load-gno-file;

define method read-16-bits( fs, #key endian = #"little" )
  let total-size1 = read-element( fs );
  let total-size2 = read-element( fs );
  if(endian == #"little")
    total-size1 + total-size2 * 256;
  else
    total-size2 + total-size1 * 256;
  end;    
end method read-16-bits;

define method do-load-gno-file ( fs, sequence, version )
    let data1 = read( fs, 2 ); // Don't know what these are
    let total-size = read-16-bits( fs );
    let abort = #f;
    let index = 0;
    until(abort)
      let entry = make(<gno-documentation>);
      entry.gno-type := read-16-bits( fs );
      entry.gno-subtype := read-16-bits( fs );
      entry.gno-number := read-16-bits( fs );
      read-16-bits( fs ); // skip
      let description-size = read-16-bits( fs );
      entry.gno-description := as(<string>, read( fs, description-size ));
      let comment-size = read-16-bits( fs );
      if(comment-size > 0)
        entry.gno-comment := as(<string>, read( fs, comment-size ) );
      else
        entry.gno-comment := "";
      end if;
      
      sequence := add!( sequence, entry );
      index := index + 1;
      if( index = total-size)
        abort := #t;
      end if;
    end until;
    sequence;
end method do-load-gno-file;

define method do-load-gno-file ( fs, sequence, version == #"creatures3" )
    let data1 = read( fs, 2); // Don't know what these are
    let total-size = read-16-bits( fs );
    for(index from 0 below total-size)
      let gene-type = read-16-bits( fs );
      let gene-subtype = read-16-bits( fs );
      let gene-sequence = read-16-bits( fs );
      let gene-number = read-16-bits( fs );
      let descriptions = make(<stretchy-vector>);
      for(entry from 0 below 19)
        let len = read-16-bits( fs );
        if(zero?(len))
          descriptions := add!(descriptions, "");
        else
          let comment = as(<string>, read( fs, len ));
          descriptions := add!(descriptions, comment);
        end if;
      end for;
      sequence := add!(sequence, make(<gno-extra-documentation>,
                                      type: gene-type,
                                      subtype: gene-subtype,
                                      sequence: gene-sequence,
                                      number: gene-number,
                                      comments: descriptions));
    end for;
    let next-position = (1801 - total-size) * 46 + fs.stream-position;
    fs.stream-position := next-position;
    next-method(fs, sequence, version);
end method do-load-gno-file;

define function write-16-bits( fs, number )
    let (byte2, byte1) = truncate/( number, 256 );
    write-element( fs, byte1 );
    write-element( fs, byte2 );
end function write-16-bits;

define method save-gno-file ( gno, filename, #key version )
    with-open-file ( fs = filename, element-type: <byte>, direction: #"output" )
      let normal = choose(method(x) instance?(x, <gno-documentation>) end, gno);
      let extra = choose(method(x) instance?(x, <gno-extra-documentation>) end, gno);
      when(extra.size > 0)
        write-16-bits( fs, 2 );
        write-16-bits( fs, extra.size );
        for(e in extra)
          write-16-bits( fs, e.gno-type );
          write-16-bits( fs, e.gno-subtype );
          write-16-bits( fs, e.gno-sequence );
          write-16-bits( fs, e.gno-number );
          for(d in e.gno-comments)
            let len = d.size;
            write-16-bits( fs, len );
            unless(zero?(len))
              write( fs, d );
            end;
          end;
        end for;
        let zero-fill = make(<vector>, size: (1801 - extra.size) * 46, fill: 0);
        write( fs, zero-fill );
      end when;

      write-16-bits( fs, 2 );
      write-16-bits( fs, normal.size );
      for( g in normal )
        write-16-bits( fs, g.gno-type );
        write-16-bits( fs, g.gno-subtype );
        write-16-bits( fs, g.gno-number );
        write-16-bits( fs, 0);
        write-16-bits( fs, g.gno-description.size );
        write( fs, g.gno-description );
        write-16-bits( fs, g.gno-comment.size );
        if(g.gno-comment.size > 0)
          write( fs, g.gno-comment );
        end if;
      end for;
    end with-open-file;
end method save-gno-file;



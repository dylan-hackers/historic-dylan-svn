Module:    print-implementation
Author:    Jonathan Bachrach, Eliot Miranda
copyright: 1996 The Harlequin Group Limited. All rights reserved.

//// <UNBOUND>

define method print (object :: <unbound>,
                     #key stream = *standard-output*)
  write(stream,"UNBOUND");
  object
end method;

//// <MULTIDIMENSIONAL-ARRAY>
//// A native thingy

define method print-array-value (array :: <multidimensional-array>, stream :: <stream>)
  begin
    let rank :: <integer> = array.dimensions.size;
    local method print-helper (axis, #rest indices)
            if (axis = rank)
              // format(stream, "%=", array[reverse!(indices)])
              print(array[reverse!(indices)], stream: stream);
            else
              write-element(stream, '(');
              for (first? = #t then #f, i :: <integer> from 0,
                   until: i >= array.dimensions[#()])
                unless (first?)
                  write-element(stream, ' ');
                end unless;
                apply(print-helper, axis + 1, i, indices);
              finally
                axis;
              end for;
              write-element(stream, ')');
            end if;
          end method;
    print-helper(0)
  end;
  values()
end method;

define method print (array :: <multidimensional-array>,
                     #key stream = *standard-output*, verbose?)
  print-standard-object-header(stream, array);
  // format(stream, "DIMENSIONS: %=", array.dimensions);
  write(stream, "DIMENSIONS: ");
  print(array.dimensions, stream);
  if (verbose?)
    write(stream, " VALUE: ");
    print-array-value(array, stream)
  end if;
  print-standard-object-trailer(stream, array)
end method;

//// <ISLAND-DEQUE>

define method print (deque :: <island-queue>, #key stream, verbose?)
//  format
//    (stream, "[<island-queue> FIRST: %d LAST: %d DATA: %d]", deque.first-index,
//     deque.last-index, deque.data);
  format
    (stream, "[<island-queue> FIRST: %d LAST: %d DATA: %=]", deque.first-index,
     deque.last-index, deque.data);
  deque
end method;

//// <SIMPLE-CONDITION>

define method print (condition :: <simple-condition>,
                     #key stream = *standard-output*, verbose?)
  // (if verbose?
  //    (next-method) )
  apply(format, stream,
        condition.condition-format-string,
        condition.condition-format-arguments)
end method;

/*
;;;; <TYPE-ERROR>

(define-method print
    ((condition <type-error>) #key (stream *standard-output*) (verbose? #F))
  ;; (if verbose?
  ;;    (next-method) )
  (format stream "%= is not an instance of %="
	  (type-error-value condition)
	  (type-error-type condition)))
*/

// eof

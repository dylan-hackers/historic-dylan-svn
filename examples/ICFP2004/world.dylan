module: ants


define function dump-world-state(world :: <array>) => ()
  for(xx from 0 below *world*.dimensions[0])
    for(yy from 0 below *world*.dimensions[1])
      let cell-string = format-to-string("cell (%d, %d): ", xx, yy);

      // Add the cell contents.
      
      format-out("%s\n", cell-string);
    end for;
  end for;  
end function dump-world-state;

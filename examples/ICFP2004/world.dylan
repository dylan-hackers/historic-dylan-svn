module: ants


define function dump-world-state(world :: <array>) => ()
  for(yy from 0 below *world*.dimensions[1])
    for(xx from 0 below *world*.dimensions[0])
      let cell-string = format-to-string("cell (%d, %d):", xx, yy);

      // Add the cell contents.
      let cell = *world*[xx, yy];
      if (cell.rocky)
        cell-string := format-to-string("%s rock", cell-string);
      else
        
      end if;
      
      format-out("%s\n", cell-string);
    end for;
  end for;  
end function dump-world-state;

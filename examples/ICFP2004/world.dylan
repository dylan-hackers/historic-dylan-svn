module: ants

define class <world> (<object>)
  constant slot world-x :: <integer>, required-init-keyword: x:;
  constant slot world-y :: <integer>, required-init-keyword: y:;
  constant slot cells :: <simple-object-vector>, required-init-keyword: cells:;
end;

define variable *world* :: <world> = make(<world>, x:0, y:0, cells: #[]);


define function dump-world-state(*world* :: <world>) => ()
  for(yy from 0 below *world*.world-y)
    for(xx from 0 below *world*.world-x)
      let p = make-position(xx, yy);
      let ss = format-to-string("cell (%d, %d): ", xx, yy);

      // Add the cell contents.
      if (is-rocky(p))
        ss := format-to-string("%srock", ss);
      else
        if (food-at(p) > 0)
          ss := format-to-string("%s%d food; ", ss, food-at(p));
        end if;

        if (anthill-at(p, #"red"))
          ss := format-to-string("%sred hill; ", ss);
        end if;

        if (anthill-at(p, #"black"))
          ss := format-to-string("%sblack hill; ", ss);
        end if;

        if (check-any-marker-at(p, #"red"))
          ss := format-to-string("%sred marks: ", ss);
          for (i from 0 below 6)
            if (check-marker-at(p, #"red", i))
              ss := format-to-string("%s%d", ss, i);
            end if;
          end for;
          ss := format-to-string("%s; ", ss);
        end if;

        if (check-any-marker-at(p, #"black"))
          ss := format-to-string("%sblack marks: ", ss);
          for (i from 0 below 6)
            if (check-marker-at(p, #"black", i))
              ss := format-to-string("%s%d", ss, i);
            end if;
          end for;
          ss := format-to-string("%s; ", ss);
        end if;

        if (ant-at(p))
          let a = ant-at(p);
          ss := format-to-string("%s%s ant of id %d, dir %d, food %d, state %d, resting %d",
                                 ss, as(<string>, a.color), a.id, a.direction,
                                 if (a.has-food) 1 else 0 end if,
                                 a.state, a.resting);
        end if;
      end if;
      
      format-out("%s\n", ss);
    end for;
  end for;  
end function dump-world-state;


define function dump-world-summary(*world* :: <world>,
                                   red-brain :: <string>,
                                   black-brain :: <string>,
                                   world :: <string>) => ()
  let reds = 0;
  let blacks = 0;
  for(yy from 0 below *world*.world-y)
    for(xx from 0 below *world*.world-x)
      let p = make-position(xx, yy);

      if (anthill-at(p, #"red"))
        reds := reds + food-at(p);
      end if;

      if (anthill-at(p, #"black"))
        blacks := blacks + food-at(p);
      end if;
      
    end for;
  end for;

  format-out("Summary for your ants on world %s:\n", world);
  format-out("Reds (%s) scored: %d.\n", red-brain, reds);
  format-out("Blacks (%s) scored: %d.\n", black-brain, blacks);
end function dump-world-summary;

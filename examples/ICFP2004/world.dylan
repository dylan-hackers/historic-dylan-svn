module: ants


define variable *world* :: <array> = make(<array>, dimensions: #[0, 0]);


define function dump-world-state(world :: <array>) => ()
  for(yy from 0 below *world*.dimensions[1])
    for(xx from 0 below *world*.dimensions[0])
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

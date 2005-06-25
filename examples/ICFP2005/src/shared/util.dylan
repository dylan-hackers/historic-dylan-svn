module: world

define method find-player (name :: <string>, world :: <world>) => (location)
  block(return)
    for (player in world.world-players)
      if (player.player-name = name)
        return(player);
      end if;
    end for;
  end block;
end;

define method find-possible-locations
    (current-location :: <node>, #key wanted-type = "foot")
 => (result :: <stretchy-object-vector>)
  if (wanted-type = "foot")
    current-location.moves-by-foot;
  elseif (wanted-type = "car")
    current-location.moves-by-car;
  end if;
end;

define function dbg(#rest args)
  apply(format, *standard-error*, args);
  force-output(*standard-error*);
end;

define function send(#rest args)
  apply(format, *standard-output*, args);
  force-output(*standard-output*);
  dbg("Send: %s\n", args);
end;

define method regexp-match(big :: <string>, regex :: <string>) => (#rest results);
  let (#rest marks) = regexp-position(big, regex);
  let result = make(<stretchy-vector>);

  if(marks[0])
    for(i from 0 below marks.size by 2)
      if(marks[i] & marks[i + 1])
        result := add!(result, copy-sequence(big, 
                                             start: marks[i], 
                                             end: marks[i + 1]))
      else
        result := add!(result, #f)
      end
    end
  end;
  apply(values, result)
end;

define function re (stream, #rest regexen)
  let regex = reduce1(method(x, y) concatenate(x, ws-re, y) end,
                      regexen);
  let line = read-line(stream);
  dbg("line: %s\n", line);
  let (match, #rest substrings) = regexp-match(line, regex);
  //dbg("RE: %= %= %=\n", regex, line, match);
  unless (match) signal(make(<parse-error>)) end;
  apply(values, substrings)
end;



define function collect (stream, type, keywords, regexps)
 => (res :: <vec>);
  let res = make(<stretchy-vector>);
  block()
    while(#t)
      let (#rest substrings) = apply(re, stream, regexps);
      add!(res,
           apply(make, type,
                 intermingle(keywords, substrings)));
    end while;
  exception (condition :: <parse-error>)
  end;
  as(<vec>, res);
end;

// kudos to Intercal
define function intermingle (#rest sequences)
  apply(concatenate,
        apply(map,
              method(#rest elements) elements end,
              sequences));
end;



define constant <vec> = <simple-object-vector>;
define constant <string> = <byte-string>;

define macro lock-down
  { lock-down ?classes end } => { ?classes }
  classes:
    { } => { }
    { ?:name, ... } =>
    { define sealed domain make(singleton(?name));
      define sealed domain initialize(?name);
      ... }
end lock-down;


module: brain-compiler2

define generic unparse(i :: <instruction>)
 => text :: <byte-string>;

define function sense-condition-as-string(m)
 => text :: <byte-string>;
  select (m)
    Friend: => "Friend";
    Foe: => "Foe";
    FriendWithFood: => "FriendWithFood";
    FoeWithFood: => "FoeWithFood";
    Food: => "Food";
    Rock: => "Rock";
    Marker0: => "Marker 0";
    Marker1: => "Marker 1";
    Marker2: => "Marker 2";
    Marker3: => "Marker 3";
    Marker4: => "Marker 4";
    Marker5: => "Marker 5";
    FoeMarker: => "FoeMarker";
    Home: => "Home";
    FoeHome: => "FoeHome";
  end;
end;

define function sense-direction-as-string(d)
 => text :: <byte-string>;
  select (d)
    Here: => "Here";
    Ahead: => "Ahead";
    LeftAhead: => "LeftAhead";
    RightAhead: => "RightAhead";
   end;
end;

define method unparse(s :: <sense>)
 => text :: <byte-string>;
  format-to-string("Sense %s %d %d %s",
                   s.sense-direction.sense-direction-as-string,
                   s.state-true,
                   s.state-false,
                   s.cond.sense-condition-as-string);
end;


define method unparse(m :: <mark>)
 => text :: <byte-string>;
  format-to-string("Mark %d %d", m.marker, m.state);
end;

define method unparse(u :: <unmark>)
 => text :: <byte-string>;
  format-to-string("Unmark %d %d", u.marker, u.state);
end;

define method unparse(p :: <pickup>)
 => text :: <byte-string>;
  format-to-string("PickUp %d %d", p.state-success, p.state-failure);
end;

define method unparse(d :: <drop>)
 => text :: <byte-string>;
  format-to-string("Drop %d", d.state);
end;

define method unparse(t :: <turn>)
 => text :: <byte-string>;
  format-to-string("Turn %s %d", if (t.left-or-right == #"left") "Left" else "Right" end, t.state);
end;

define method unparse(m :: <move>)
 => text :: <byte-string>;
  format-to-string("Move %d %d", m.state-success, m.state-failure);
end;

define method unparse(f :: <flip>)
 => text :: <byte-string>;
  format-to-string("Flip %d %d %d", f.probability, f.state-success, f.state-failure);
end;



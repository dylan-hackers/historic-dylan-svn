module: ants


begin
  /*
    let test-machine = read-state-machine(*standard-input*);
    do(method(x) format-out("%s\n", unparse(x)) end, test-machine);
  force-output(*standard-output*);
  let testmap = read-map(*standard-input*);


*/
/*
  with-open-file(world-stream = application-arguments()[0])
    *world* := read-map(world-stream)
  end with-open-file;

  // Test world output.
  dump-world-state(*world*);
*/
  apply(play-game, application-arguments());


end;

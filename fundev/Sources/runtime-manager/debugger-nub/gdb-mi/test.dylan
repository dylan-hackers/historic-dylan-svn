module: gdb-access

let command = break-list();
format-out("%=\n", command);

let (to-stream, from-stream) = piped-exec("gdb -i mi");

while(#t)
  iterate loop()
    let line = read-line(from-stream);
    write-line(*standard-output*, line);
    if(line ~= "(gdb) ")
      loop();
    end if;
  end iterate;
  force-output(*standard-output*);

  let line = read-line(*standard-input*);
  write-line(to-stream, line);
  force-output(to-stream);
end while;

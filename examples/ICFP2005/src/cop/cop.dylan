module: cop
synopsis: 
author: 
copyright: 

define function main(name, arguments)
  let skelet = 
    with-open-file(fs = "world-skeleton",
                   direction: #"input",
                   element-type: <character>)
      read-world-skeleton(fs);
    end;
  let world = with-open-file(fs = "world",
                             direction: #"input",
                             element-type: <character>)
                read-world(fs, skelet);
              end;
  for (i in world.players)
    format-out("%=\n", i);
  end;
 // exit-application(0);
end function main;

main(application-name(), application-arguments());

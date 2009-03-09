module: dfmc-visualization
author: Hannes Mehnert
copyright: 2009, all rights reversed
synopsis: Dylan side of graphical visualization of DFM control flow graphs

define function report-progress (i1 :: <integer>, i2 :: <integer>,
                                 #key heading-label, item-label)
  //if (item-label[0] = 'D' & item-label[1] = 'F' & item-label[2] = 'M')
  //  format-out("%s %s\n", heading-label, item-label);
  //end;
end;

define function write-data (vis :: <dfmc-graph-visualization>, key :: <symbol>, #rest arguments)
  write-to-visualizer(vis, apply(list, key, vis.dfm-index, arguments));
end;

define function trace-computations (vis :: <dfmc-graph-visualization>, key :: <symbol>, id :: <integer>, comp-or-id, comp2 :: <integer>, #key label)
  select (key by \==)
    #"add-temporary-user", #"remove-temporary-user" =>
      write-data(vis, key, id, comp-or-id);
    #"add-temporary" =>
      begin
        let str = make(<string-stream>, direction: #"output");
        print-object(comp-or-id, str);
        write-data(vis, key, id, str.stream-contents, comp2);
      end;
    #"temporary-generator" =>
      write-data(vis, key, id, comp-or-id, comp2);
    #"remove-temporary" =>
      write-data(vis, key, id);        
    #"remove-edge", #"insert-edge" =>
      write-data(vis, key, id, comp-or-id, label);
    #"change-edge" =>
      write-data(vis, key, id, comp-or-id, comp2, label);
    #"new-computation" =>
      write-data(vis, key, output-computation-sexp(comp-or-id));
    #"remove-computation" =>
      write-data(vis, key, id);
    #"change-entry-point" =>
      write-data(vis, key, id, comp-or-id);
    #"set-loop-call-loop" =>
      write-data(vis, key, id, comp-or-id, #"no");
    otherwise => ;
  end;
end;

define function visualize (vis :: <dfmc-graph-visualization>, key :: <symbol>, object :: <object>)
  select (key by \==)
    #"file-changed" => vis.report-enabled? := (object = "scratch-source");
    #"dfm-switch" => vis.dfm-report-enabled? := (object == 4);
    #"dfm-header" =>
        write-data(vis, key, object);
    #"optimizing" =>
      begin
        vis.dfm-report-enabled? := (object == 4);
        write-data(vis, #"relayouted");
      end;
    //#"finished" =>
    //  vis.dfm-report-enabled? := #f;
    #"beginning" =>
      write-data(vis, key, object);
    #"relayouted" =>
      write-data(vis, key);
    #"highlight-queue" =>
      write-data(vis, key, object);
    #"highlight" =>
      if (instance?(object, <integer>))
        write-data(vis, key, object);
      end;
    otherwise => ;
  end;
end;

define function trace-types (vis :: <dfmc-graph-visualization>, key :: <symbol>, #rest args);
  apply(write-data, vis, key, args);
end;

define function visualizing-compiler (vis :: <dfmc-graph-visualization>, project)
  let lib = project.project-current-compilation-context;
  vis.dfm-index := vis.dfm-index + 1;
  block()
    dynamic-bind(*progress-library* = lib,
                 *dump-dfm-method* = curry(visualize, vis),
                 *computation-tracer* = curry(trace-computations, vis),
                 *typist-visualize* = curry(trace-types, vis))
      with-progress-reporting(project, report-progress, visualization-callback: curry(visualize, vis))
        compile-library-from-definitions(lib, force?: #t, skip-link?: #t,
                                         compile-if-built?: #t, skip-heaping?: #t);
      end;
    end;
  exception (e :: <abort-compilation>)
  end
end;


Module:    graph-viewer
Synopsis:  DUIM GL pane for viewing graphs


define function random-position(variance :: <integer>) => (random-vector :: <simple-double-float-vector>);
  let offset = as(<double-float>, variance - 1) / 2.0d0;
  v2(as(<double-float>, random(variance) - offset),
     as(<double-float>, random(variance) - offset))
end;

define sealed class <node-layout> (<object>)
  slot node :: <node>, required-init-keyword: node:;
  slot node-position :: <simple-double-float-vector> 
    = random-position(200);
  slot new-position :: <simple-double-float-vector>;
  slot velocity :: <simple-double-float-vector> = v2(0.0d0, 0.0d0);
end;

define sealed class <graph-viewer-gl-pane> (<gl-pane>) 
  slot graph :: <graph>, required-init-keyword: graph:;
  slot layouts :: <table> = make(<table>);
  slot pane-latitude = 0.0d0;
  slot pane-longitude = 0.0d0;
  slot pane-last-x = #f;
  slot pane-last-y = #f;
  slot dragged-layout = #f;
end class;

define method initialize (pane :: <graph-viewer-gl-pane>, #key)
  next-method();
  recompute-layout(pane);
end;

define method recompute-layout(pane :: <graph-viewer-gl-pane>) => ();
  pane.layouts := make(<table>);
  for(node in pane.graph.nodes)
    pane.layouts[node] := make(<node-layout>, node: node) 
  end;
  pane.layouts[pane.graph.nodes[0]].node-position := v2(0.0d0, 0.0d0);
end;

define method set-projection-matrix (pane :: <graph-viewer-gl-pane>) => ()
  if (bounding-box?(pane.sheet-region))
    let (left, top, width*, height*) = pane.sheet-region.box-edges;
    let width = as(<double-float>, width*);
    let height = as(<double-float>, height*);

    glMatrixMode($GL-PROJECTION);
    glLoadIdentity();
    glOrtho(- width / 2.0d0, width / 2.0d0,
            - height / 2.0d0, height / 2.0d0,
            - 10.0d0, 10.0d0);
    glMatrixMode($GL-MODELVIEW);
  else
    error("Can't grok shaped GL windows")
  end;
end;  

define method handle-initialize-scene 
    (pane :: <graph-viewer-gl-pane>, medium :: <medium>) => ();
  next-method();

  glClearColor(1.0s0, 1.0s0, 1.0s0, 1.0s0);
  glClearDepth(1.0d0);

  glEnable($GL-DEPTH-TEST);
/*
  glEnable($GL-POINT-SMOOTH);
  glEnable($GL-LINE-SMOOTH);
 */
  glPointSize(3.0s0);
  
  set-projection-matrix(pane);

  let frame = sheet-frame(pane);
  frame.timer-thread |
    (frame.timer-thread 
      := make(<thread>,
              function: method ()
                          while(#t)
                            sleep(1.0 / 60.0);                     
                            with-lock (frame.lock)
                              call-in-frame(frame, update-graph-layout, pane);
                              wait-for(frame.update-notification)
                            end
                          end
                        end));
end method;

define inline-only function v2(x :: <double-float>, y :: <double-float>) => (v2 :: <simple-double-float-vector>);
  let v2 = make(<simple-double-float-vector>, size: 2, fill: 0.0d0);
  v2[0] := x;
  v2[1] := y;
  v2
end;

define method spring-force (distance :: <simple-double-float-vector>,
                            spring-length :: <double-float>,
                            spring-constant :: <double-float>)
 => (force :: <simple-double-float-vector>);
  block()
    (distance - (normalize(distance) * spring-length)) * spring-constant
  exception (e :: <error>)
    v2(0.0d0, 0.0d0)
  end;
end;

define method repelling-force (distance :: <simple-double-float-vector>,
                               spring-constant :: <double-float>)
 => (force :: <simple-double-float-vector>);
  block()
    let distance-squared :: <double-float> = distance * distance;
    if (distance-squared < 10.0d0)
      distance-squared := 10.0d0
    end;
    let direction :: <simple-double-float-vector> = normalize(distance);
    (1.0d0 / distance-squared) * spring-constant * direction
  exception (e :: <error>)
    v2(0.0d0, 0.0d0)
  end
end;

define constant $spring-force :: <double-float> = 1.0d0;
define constant $spring-length :: <double-float> = 50.0d0;
define constant $repelling-force :: <double-float> = -1.0d5;
define constant $friction :: <double-float> = 0.93d0;
define constant $mass :: <double-float> = 100.0d0;
define constant $center-force :: <double-float> = -5.0d-2;

define method update-graph-layout (pane :: <graph-viewer-gl-pane>) => ();
  repaint-sheet(pane, $everywhere);
  local method local-node-position(node :: <node>) => (position :: <simple-double-float-vector>)
          pane.layouts[node].node-position
        end;
  local method node-distance (node1 :: <node>, node2 :: <node>) => (position :: <simple-double-float-vector>)
          node2.local-node-position - node1.local-node-position
        end;

  for(layout :: <node-layout> in pane.layouts)
    layout.new-position := layout.node-position;
    if (layout ~= pane.dragged-layout)

      let force1 :: <simple-double-float-vector> = v2(0.0d0, 0.0d0);
      for (node* :: <node> in pane.graph.nodes)
        if(node* ~== layout.node)
          force1 := force1 + repelling-force(node-distance(layout.node, node*),
                                             $repelling-force)
        end
      end;

      let force2 :: <simple-double-float-vector> = v2(0.0d0, 0.0d0);
      for (node* :: <node> in layout.node.neighbours)
        force2 := force2 + spring-force(node-distance(layout.node, node*),
                                        $spring-length,
                                        $spring-force)
      end;

      layout.velocity 
        := (layout.velocity 
              + ((force1 + force2 + layout.node-position * $center-force) / $mass)) 
                * $friction;

      layout.new-position := layout.node-position + layout.velocity; // divide by time!
    end;
  end;
  map(method(x) x.node-position := x.new-position end, pane.layouts);
  let frame = sheet-frame(pane);
  with-lock (frame.lock)
    release(frame.update-notification)
  end
end;

define method relayout-children (pane :: <graph-viewer-gl-pane>) => ()
  next-method();
  pane.pane-gl-initialized? := #f // XXX leaking contexts live a sieve
//  set-projection-matrix(pane);
end method;

define method convert-to-world-coordinates (pane :: <graph-viewer-gl-pane>,
                                            x :: <integer>,
                                            y :: <integer>) => (v2 :: <simple-double-float-vector>);
  let (left, top, width*, height*) = pane.sheet-region.box-edges;
  let width = as(<double-float>, width*);
  let height = as(<double-float>, height*);
  let latitude = pane-latitude(pane);
  let longitude = pane-longitude(pane);

  v2(x - width / 2.0d0 + longitude,
     - y + height / 2.0d0 - latitude)
end;
  
define method handle-repaint-scene 
    (pane :: <graph-viewer-gl-pane>, medium :: <medium>) => ()
  // Draw scene...
  local method vertex-for-node (node :: <node>)
          let layout = pane.layouts[node];
          glVertex2d(layout.node-position[0], layout.node-position[1]);
        end;
  local method vertices-for-edge (edge :: <edge>)
          vertex-for-node(edge.source);
          vertex-for-node(edge.target);
        end;

  let latitude = pane-latitude(pane);
  let longitude = pane-longitude(pane);

  glClear(logior($GL-COLOR-BUFFER-BIT, $GL-DEPTH-BUFFER-BIT));
  glPushMatrix();
  glTranslated(- longitude, latitude, 0.0d0);
  glColor3d(0.0d0, 0.0d0, 1.0d0);

  glBegin($GL-POINTS);
  do(vertex-for-node, pane.graph.nodes);
  glEnd();

  glBegin($GL-LINES);
  do(vertices-for-edge, pane.graph.edges);
  glEnd();
  glPopMatrix();
end method;

define method handle-event (pane :: <graph-viewer-gl-pane>, event :: <pointer-drag-event>) => ()
  next-method();
  let x = event-x(event);
  let y = event-y(event);

  let world-position = convert-to-world-coordinates(pane, x, y);

  if (pane.dragged-layout)
    pane.dragged-layout.node-position := world-position
  else
    let near-node 
      = find-element(pane.layouts, 
                     method(layout)
                       distance(world-position, layout.node-position) < 6.0d0
                     end);

    if (near-node)
      pane.dragged-layout := near-node;
      near-node.node-position := world-position
    else

      let last-x = pane-last-x(pane);
      let last-y = pane-last-y(pane);
      if (last-x)
        let dx = x - last-x;
        pane-longitude(pane) := pane-longitude(pane) - as(<double-float>, dx);
      end;
      if (last-y)
        let dy = y - last-y;
        pane-latitude(pane) := pane-latitude(pane) - as(<double-float>, dy);
      end;
      pane-last-x(pane) := x;
      pane-last-y(pane) := y;
    end;
  end;
  repaint-sheet(pane, $everywhere);
end method;

define method handle-event 
    (pane :: <graph-viewer-gl-pane>, event :: <button-release-event>) => ()
  next-method();
  pane-last-x(pane) := #f;
  pane-last-y(pane) := #f;
  dragged-layout(pane) := #f;
end method;

define method handle-event 
    (pane :: <graph-viewer-gl-pane>, event :: <pointer-exit-event>) => ()
  next-method();
  pane-last-x(pane) := #f;
  pane-last-y(pane) := #f;
  dragged-layout(pane) := #f;
end method;

define constant $graph = make(<graph>);

define variable *oh-this-sucks* = #f;

define frame <demo-frame> (<simple-frame>)
  constant slot lock :: <lock>, init-value: make(<lock>);
  slot update-notification :: false-or(<notification>), init-value: #f;
  slot timer-thread :: false-or(<thread>) = #f;
  pane file-menu (frame)
    make(<menu>,
         label: "&File",
         children:
           vector(make(<menu-button>,
                       label: "New Graph",
                       activate-callback:
                         method (sheet)
                           $graph.nodes := make(<stretchy-vector>);
                           $graph.edges := make(<stretchy-vector>);
                           create-random-graph(40, random(80));
                           recompute-layout(*oh-this-sucks*)
                         end),
                  make(<menu-button>,
                       label: "New Bipartite Graph",
                       activate-callback:
                         method (sheet)
                           $graph.nodes := make(<stretchy-vector>);
                           $graph.edges := make(<stretchy-vector>);
                           create-random-bipartite-graph(80, random(120));
                           recompute-layout(*oh-this-sucks*)
                         end),
                  make(<menu-button>,
                       label: "Recompute",
                       activate-callback:
                         method (sheet)
                           recompute-layout(*oh-this-sucks*)
                         end),
                  make(<menu-button>,
                       label: "Close",
                       activate-callback:
                         method (sheet)
                           exit-frame(sheet-frame(sheet))
                         end)));
  menu-bar (frame)
    make(<menu-bar>,
         children: vector(file-menu(frame)));
  layout (frame)
    vertically (spacing: 2)
      with-border (style: #"inset")
        *oh-this-sucks* := make(<graph-viewer-gl-pane>,
                                graph: $graph,
                                width: 350, height: 300)
      end;
    end;
  status-bar (frame)
    make(<status-bar>);
  keyword title: = "GL Graph Viewer";
end frame;

define method initialize (frame :: <demo-frame>, #key)
  next-method();
  frame.update-notification := make(<notification>, lock: frame.lock)
end method initialize;

define method choose-random-node (graph :: <graph>) => (node :: <node>);
  graph.nodes[random(graph.nodes.size)]
end;

define function create-random-graph(nodes :: <integer>, edges :: <integer>);
  for(i from 0 below nodes)
    create-node($graph)
  end;

  for(i from 0 below edges)
    let node1 = choose-random-node($graph);
    let node2 = choose-random-node($graph);
    unless (node1 = node2)
      create-edge($graph, node1, node2);
    end;
  end;
end;

define method choose-random-bipartite-node (graph :: <graph>,
                                            lower-half? :: <boolean>) => (node :: <node>);
  if (lower-half?)
    graph.nodes[random(ash(graph.nodes.size, -1))]
  else  
    graph.nodes[random(ash(graph.nodes.size, -1)) + ash(graph.nodes.size, -1)]
  end
end;

define function create-random-bipartite-graph(node-count :: <integer>, edges :: <integer>);
  for(i from 0 below node-count)
    create-node($graph)
  end;
  
  for(i from 0 below edges)
    let node1 = choose-random-bipartite-node($graph, #f);
    let node2 = choose-random-bipartite-node($graph, #f);
    unless (node1 = node2)
      create-edge($graph, node1, node2);
    end;
  end;

  for(i from 0 below edges)
    let node1 = choose-random-bipartite-node($graph, #t);
    let node2 = choose-random-bipartite-node($graph, #t);
    unless (node1 = node2)
      create-edge($graph, node1, node2);
    end;
  end;

  create-edge($graph, $graph.nodes[0], $graph.nodes[node-count - 1]);
end;


define method main () => ()
/*
  let node1 = create-node($graph);
  let node2 = create-node($graph);
  let node3 = create-node($graph);
  let node4 = create-node($graph);
  create-edge($graph, node1, node2);
  create-edge($graph, node2, node3);
  create-edge($graph, node3, node1);
  create-edge($graph, node1, node4);
*/

  create-random-graph(30, 30);

  start-frame(make(<demo-frame>));
end method main;

begin
  main();
end;

// eof


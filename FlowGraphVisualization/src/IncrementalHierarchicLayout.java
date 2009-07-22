import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedList;

import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import y.base.DataMap;
import y.base.EdgeCursor;
import y.base.Node;
import y.base.NodeCursor;
import y.base.NodeMap;
import y.layout.hierarchic.ConstraintLayerer;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.TopologicalLayerer;
import y.layout.hierarchic.ConstraintLayerer.ConstraintFactory;
import y.layout.hierarchic.incremental.IncrementalHintsFactory;
import y.layout.hierarchic.incremental.OldLayererWrapper;
import y.layout.hierarchic.incremental.SwimLaneDescriptor;
import y.util.Maps;
import y.view.Arrow;
import y.view.BridgeCalculator;
import y.view.DefaultGraph2DRenderer;
import y.view.EdgeRealizer;
import y.view.GenericEdgeRealizer;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.LineType;

public class IncrementalHierarchicLayout
{
	protected IncrementalHierarchicLayouter hierarchicLayouter;
	protected IncrementalHierarchicLayouter typeLayouter;
	protected HashMap<Integer, Node> int_node_map = new HashMap<Integer, Node>();
	protected HashMap<Node, Node> tv_temp_map = new HashMap<Node, Node>();
	
	protected Graph2D graph;
	private Graph2DView view;
	protected Graph2D typegraph;
	private Graph2DView typeview;
	private DemoBase demobase;
	protected ConstraintFactory scf;
	protected ConstraintFactory typescf;
	
	public Node highlight = null;
	public ArrayList<Integer> opt_queue;


	public boolean changed = false;
	public boolean typechanged = false;
	public int numChanges = 0;
	protected Hashtable<Integer, JLabel> sliderLabels = new Hashtable<Integer, JLabel>();
	protected int lastEntry = 0;
	private int lastChangeCount = 0;
	public ArrayList<ArrayList> changes = new ArrayList<ArrayList>();

	protected int lastslidervalue = 0;
	protected boolean graphfinished = false;
	protected boolean graphinprocessofbeingfinished = false;
	
	protected final String graph_id;
	
	public Node selection;
	
	public boolean isok = true;

	private NodeMap swimLane;
	protected DataMap hintMap;
	protected IncrementalHintsFactory hintsFactory;
	private ArrayList<Node> topnodes;
	
	protected IncrementalHintsFactory typeHintsFactory;
	protected DataMap typeHintMap;
	
	
	public IncrementalHierarchicLayout(DemoBase db, String id)
	{
		graph_id = id;
		graph = new Graph2D();
		typegraph = new Graph2D();
		view = db.view;
		typeview = db.typeview;
		demobase = db;
		
		// enable bridges for PolyLineEdgeRealizer
		BridgeCalculator bridgeCalculator = new BridgeCalculator();
		bridgeCalculator.setCrossingMode( BridgeCalculator.CROSSING_MODE_HORIZONTAL_CROSSES_VERTICAL );
		((DefaultGraph2DRenderer) view.getGraph2DRenderer()).setBridgeCalculator(bridgeCalculator );

		((DefaultGraph2DRenderer) typeview.getGraph2DRenderer()).setBridgeCalculator(bridgeCalculator );

		// create and configure the layout algorithm
		hierarchicLayouter = new IncrementalHierarchicLayouter();
		hierarchicLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_FROM_SCRATCH);
		
	    hierarchicLayouter.getEdgeLayoutDescriptor().setMinimumFirstSegmentLength(0);
	    hierarchicLayouter.getEdgeLayoutDescriptor().setMinimumLastSegmentLength(0);
	    //hierarchicLayouter.getEdgeLayoutDescriptor().setOrthogonallyRouted(true);
	    hierarchicLayouter.getEdgeLayoutDescriptor().setMinimumDistance(2.0d);

	    hierarchicLayouter.getNodeLayoutDescriptor().setLayerAlignment(0.0d);
	    hierarchicLayouter.setMinimumLayerDistance(3.0d);

		
		ConstraintLayerer cl = new ConstraintLayerer();
		TopologicalLayerer tl = new TopologicalLayerer();
		cl.setCoreLayerer(tl);
		hierarchicLayouter.setFromScratchLayerer(new OldLayererWrapper(cl));

		
		
		typeLayouter = new IncrementalHierarchicLayouter();
		typeLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_FROM_SCRATCH);
	    typeLayouter.getEdgeLayoutDescriptor().setMinimumFirstSegmentLength(0);
	    typeLayouter.getEdgeLayoutDescriptor().setMinimumLastSegmentLength(0);
	    typeLayouter.getEdgeLayoutDescriptor().setMinimumDistance(2.0d);

	    typeLayouter.getNodeLayoutDescriptor().setLayerAlignment(0.0d);
	    typeLayouter.setMinimumLayerDistance(3.0d);

		
		ConstraintLayerer c2 = new ConstraintLayerer();
		TopologicalLayerer t2 = new TopologicalLayerer();
		c2.setCoreLayerer(t2);
		typeLayouter.setFromScratchLayerer(new OldLayererWrapper(c2));
		
		sliderLabels.put(0, new JLabel("initial DFM models"));
		changes.add(new ArrayList());
		initGraph();
	}
	
	public void initGraph () {
		// make it look nice
		EdgeRealizer defaultER = graph.getDefaultEdgeRealizer();
		defaultER.setArrow(Arrow.STANDARD);
		defaultER.setLineType(LineType.LINE_2);
		EdgeRealizer edefaultER = typegraph.getDefaultEdgeRealizer();
		edefaultER.setArrow(Arrow.STANDARD);
		edefaultER.setLineType(LineType.LINE_2);
		graph.setDefaultNodeRealizer(new GraphNodeRealizer());
		typegraph.setDefaultNodeRealizer(new GraphNodeRealizer());
		if (scf != null)
			scf.dispose();
		if (typescf != null)
			typescf.dispose();
		scf = ConstraintLayerer.createConstraintFactory(graph);
		typescf = ConstraintLayerer.createConstraintFactory(typegraph);
		swimLane = graph.createNodeMap();
		graph.addDataProvider(IncrementalHierarchicLayouter.SWIMLANE_DESCRIPTOR_DPKEY, swimLane);
		hintMap = Maps.createHashedDataMap();
		typeHintMap = Maps.createHashedDataMap();
	    //graph.addDataProvider(IncrementalHierarchicLayouter.INCREMENTAL_HINTS_DPKEY, hintMap);
	    hintsFactory = hierarchicLayouter.createIncrementalHintsFactory();
	    typeHintsFactory = typeLayouter.createIncrementalHintsFactory();
	    //typegraph.addDataProvider(IncrementalHierarchicLayouter.INCREMENTAL_HINTS_DPKEY, typeHintMap);
		int_node_map = new HashMap<Integer, Node>();
		tv_temp_map = new HashMap<Node, Node>();
		opt_queue = new ArrayList<Integer>();
		topnodes = new ArrayList<Node>();
		highlight = null;
		selection = null;
		tvindex = 0;
	}
	
	public void activateLayouter () {
		if (demobase.incrementallayouter != this) {
			changed = true;
			typechanged = true;
			view.setGraph2D(graph);
			typeview.setGraph2D(typegraph);
			demobase.graphChanged(this);
		}
	}

	private void initGraphHelperHelper (Object o, int id) {
		if (o instanceof String)
			initGraphHelper((String)o, id);
		else if (o instanceof Symbol)
			initGraphHelper((Symbol)o, id);
		else if (o instanceof Integer)
			initGraphHelper((Integer)o, id);
		else if (o instanceof ArrayList)
			initGraphHelper((ArrayList)o, id);
	}

	private void initGraphHelper (ArrayList nodelist, int id) {
		assert (nodelist.size() > 1);

		assert(nodelist.get(0) instanceof Integer);
		assert(nodelist.get(1) instanceof Symbol);
		
		int comp_id = (Integer)nodelist.get(0);
		Symbol s = (Symbol)nodelist.get(1);

		if (s.isEqual("if")) {
			//test, exactly one element, a string
			assert(nodelist.size() == 5);
			assert(nodelist.get(2) instanceof ArrayList);
			ArrayList testnodes = (ArrayList)nodelist.get(2);
			assert(testnodes.size() == 1);
			assert(testnodes.get(0) instanceof String);
			Node test = createNodeWithLabel((String)testnodes.get(0), comp_id);
			changeLabel(test, "if ");
				
			//consequence
			assert(nodelist.get(3) instanceof ArrayList);
			ArrayList<Node> consequence = getNodes((ArrayList)nodelist.get(3));
				
			if (consequence.size() > 0) {
				graph.createEdge(test, consequence.get(0));
				setEdgeLabel("true");
			}
			
			
			
			for (Node c : consequence) {
				//scf.addPlaceNodeBelowConstraint(test, c);
				//groups.setInt(c, test.index());
			}
				
			//alternative
			assert(nodelist.get(4) instanceof ArrayList);
			ArrayList<Node> alternative = getNodes((ArrayList)nodelist.get(4));
				
			if (alternative.size() > 0) {
				graph.createEdge(test, alternative.get(0));
				setEdgeLabel("false");
			}
			for (Node a : alternative) {
				//scf.addPlaceNodeBelowConstraint(test, a);
				//groups.setInt(a, test.index() - 1);
			}

		} else if (s.isEqual("loop")) {
			Node loop = createNodeWithLabel("loop", comp_id);
			assert(nodelist.size() == 3);
			assert(nodelist.get(2) instanceof ArrayList);
				
			ArrayList<Node> body = getNodes((ArrayList)nodelist.get(2));

			//groupMap.set(n, loop-id);
			if (body.size() > 0)
				graph.createEdge(loop, body.get(0));
			
			for (Node b : body)
				if (graph.getRealizer(b).getLabelText().contains("CONTINUE"))
					safeCreateEdge(b, loop);

		} else if (s.isEqual("loop-call")) {
			assert(nodelist.size() == 3);
			assert(nodelist.get(2) instanceof Integer);
			Node loopc = createNodeWithLabel("CONTINUE", comp_id);
			if ((Integer) nodelist.get(2) != 0) {
				Node loop = int_node_map.get((Integer)nodelist.get(2));
				graph.createEdge(loopc, loop);
			}
		} else if (s.isEqual("bind-exit")) {
			assert(nodelist.size() == 3);
			//assert(nodelist.get(2) instanceof Integer); //entry-state
			assert(nodelist.get(2) instanceof ArrayList); //body
			Node bind_exit = createNodeWithLabel("bind-exit", comp_id);
			ArrayList<Node> body = getNodes((ArrayList)nodelist.get(2));
			if (body.size() > 0)
				graph.createEdge(bind_exit, body.get(0));
		} else if (s.isEqual("unwind-protect")) {
			assert(nodelist.size() == 4);
			assert(nodelist.get(2) instanceof ArrayList); //body
			assert(nodelist.get(3) instanceof ArrayList); //cleanups
			Node unwind_protect = createNodeWithLabel("unwind-protect", comp_id);
			ArrayList<Node> body = getNodes((ArrayList)nodelist.get(2));
			if (body.size() > 0)
				graph.createEdge(unwind_protect, body.get(0));
			ArrayList<Node> cleanups = getNodes((ArrayList)nodelist.get(3));
			if (body.size() > 0)
				graph.createEdge(unwind_protect, cleanups.get(0));
		}
	}
	
	private ArrayList<Node> getNodes(ArrayList arrayList) {
		ArrayList<Node> res = new ArrayList<Node>();
		for (Object o : arrayList) {
			assert(o instanceof Integer);
			Integer i = (Integer)o;
			assert(int_node_map.get(i) != null);
			res.add(int_node_map.get(i));
		}
		return res;
	}

	private Node initGraphHelper (String node, int id) {
		return createNodeWithLabel(node, id);
	}
	
	private Node initGraphHelper (Symbol node, int id) {
		return createNodeWithLabel(node.toString(), id);
	}
	
	private Node initGraphHelper (Integer node, int id) {
		return createNodeWithLabel(Integer.toString(node), id);
	}
	
	public Node createTypeNodeWithLabel (String label, int id) {
		Node n = typegraph.createNode();
		GraphNodeRealizer nr = (GraphNodeRealizer)typegraph.getRealizer(n);
		nr.setNodeColor(new Color(0, 0xff, 0, 0x33));
		nr.setIdentifier(id);
		nr.setNodeText(label);
		assert(int_node_map.get(id) == null);
		int_node_map.put(id, n);
		return n;
	}
	
	public Node createNodeWithLabel (String label, int id) {
		Node n = graph.createNode();
		GraphNodeRealizer nr = (GraphNodeRealizer)graph.getRealizer(n);
		nr.setIdentifier(id);
		nr.setNodeText(label);
		nr.setNodeColor(new Color(0, 0, 0xff, 0x44));
		//demobase.calcLayout();
		//System.out.println("created node (gr " + graph + ")");
		if (id > 0) {
			assert(int_node_map.containsKey(id) == false);
			int_node_map.put(id, n);
		} else if (id == 0)
			topnodes.add(n);
		if (label.equalsIgnoreCase("[bind]"))
			topnodes.add(n);
		return n;
	}
	
	public void changeLabel (Node n, String app) {
		GraphNodeRealizer nr = (GraphNodeRealizer)graph.getRealizer(n);
		nr.setNodeText(app);
	}

	public void createTemporary(int temp_id, int c_id, String text) {
		Node t = createNodeWithLabel(text, temp_id);
		GraphNodeRealizer gr = (GraphNodeRealizer)graph.getRealizer(t);
		gr.setNodeColor(new Color(Color.pink.getRed(), Color.pink.getBlue(), Color.pink.getGreen(), 0x44));
		if (c_id != 0) {
			Node gen = int_node_map.get(c_id);
			assert(gen != null);
			EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
			myreal.setLineColor(Color.blue);
			myreal.setLineType(LineType.DASHED_2);
			graph.createEdge(gen, t, myreal);
			scf.addPlaceNodeInSameLayerConstraint(t, gen);
		}
	}

	final static String[] tvnames = { "\u03B1", "\u03B2","\u03B3", "\u03B4", "\u03B5", "\u03B6", "\u03B7",
			"\u03B8", "\u03B9", "\u03BA", "\u03BB", "\u03BC", "\u03BD", "\u03BE", "\u03BF", "\u03C0", "\u03C1",
			"\u03C2", "\u03C3", "\u03C4", "\u03C5", "\u03C6", "\u03C7", "\u03C8", "\u03C9", "\u03CA", "\u03CB" };
	private int tvindex = 0;
	
	public void createTypeVariable (int id, int temp, String type) {
		Node tv = createTypeNodeWithLabel(tvnames[tvindex], id);
		GraphNodeRealizer nr = (GraphNodeRealizer)typegraph.getRealizer(typegraph.lastNode());
		nr.setNodeColor(new Color(Color.pink.getRed(), Color.pink.getBlue(), Color.pink.getGreen(), 0x44));
		nr.setReference(temp);
		tvindex = (tvindex + 1) % tvnames.length;
		if (temp != 0)
			tv_temp_map.put(tv, int_node_map.get(temp));
		//NodeRealizer nr = typegraph.getRealizer(tv);
		//NodeRealizer tr = graph.getRealizer(int_node_map.get(temp));
		//nr.setLocation(tr.getX(), tr.getY());
		
		//typegraph.getRealizer(tv).setX(graph.getRealizer(int_node_map.get(temp)).getX());
		//typegraph.getRealizer(tv).setY(graph.getRealizer(int_node_map.get(temp)).getY());		
		//graph.getRealizer(tv).setFillColor((Color.BLUE).brighter());
//		EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
//		myreal.setLineColor(Color.BLUE);
//		graph.createEdge(temp, tv, myreal);
//		scf.addPlaceNodeInSameLayerConstraint(tv, temp);
	}

	public void createTypeNode (int id, Node tv) {
		int_node_map.put(id, tv);
	}

	public boolean safeCreateEdge (Node source, Node target) {
		if (source == null || target == null) {
			System.out.println("FAIL from " + source + " target " + target + " (source or target null)");
			return false;
		}
		boolean connected = false;
		for (EdgeCursor ec = source.outEdges(); ec.ok(); ec.next())
			if (ec.edge().target() == target) {
				connected = true;
				break;
			}
		if (! connected) {
			graph.createEdge(source, target);
			//scf.addPlaceNodeBelowConstraint(source, target);
			return true;
		}
		System.out.println("FAIL: nodes " + source + " and " + target + " were already connected");
		return false;
	}

	public void createNewNode(ArrayList text) {
		assert(text.get(0) instanceof Integer);
		if (text.size() == 2)
			initGraphHelperHelper(text.get(1), (Integer)text.get(0));
		else
			initGraphHelperHelper(text, (Integer)text.get(0));
	}

	public void setEdgeLabel(String label) {
		EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
		myreal.setLabelText(label);
		if (label.equals("true"))
			myreal.setLineColor(Color.green);
		else if (label.equals("false"))
			myreal.setLineColor(Color.red);
		graph.setRealizer(graph.lastEdge(), myreal);
		
	}
	
	public void setEdgeLabel(Symbol label) {
		setEdgeLabel(label.toString());
	}
	
	public void setEdgeColor (Color color) {
		EdgeRealizer myreal = new GenericEdgeRealizer(graph.getDefaultEdgeRealizer());
		myreal.setLineColor(color);
		if (color == Color.blue)
			myreal.setLineType(LineType.DASHED_2);
		graph.setRealizer(graph.lastEdge(), myreal);
	}

	public void updatephase(String text) {
		if (text.equals("finished"))
			graphinprocessofbeingfinished = true;
		final boolean rfinished = graphinprocessofbeingfinished;
		if (lastChangeCount < numChanges) {
			lastEntry++;
			lastChangeCount = numChanges;
			changes.add(new ArrayList());
		}
		sliderLabels.put(lastEntry, new JLabel(text.substring(0, Math.min(40, text.length()))));
		Runnable updateMyUI = new Runnable() {
			public void run () {
				demobase.slider.setMaximum(lastEntry);
				demobase.slider.setValue(lastEntry);
				demobase.slider.updateUI();		
				if (rfinished)
					graphfinished = true;
			}
		};
		SwingUtilities.invokeLater(updateMyUI);
		lastslidervalue = lastEntry;
	}
	
	public boolean nextStep () {
		if (graphfinished && (lastslidervalue <= lastEntry)) {
			for (Object comm : changes.get(lastslidervalue)) {
				ArrayList com = (ArrayList)comm;
				Commands.processCommand(this, com, demobase);
			}

			lastslidervalue++;
			if (lastslidervalue <= lastEntry)
				for (Object o : changes.get(lastslidervalue)) {
					ArrayList comm = (ArrayList)o;
					if (! Commands.processIfNoChange(this, comm, demobase))
						break;
				}
			changed = true;
			typechanged = true;
			demobase.updatingslider = true;
			demobase.slider.setValue(lastslidervalue);
			demobase.updatingslider = false;
			demobase.calcLayout();
			return true;
		}
		return false;
	}

	public void resetGraph(int step) {
		if (step >= lastslidervalue) {
			demobase.unselect();
			for (int i = lastslidervalue; i < step; i++)
				for (Object comm : changes.get(i)) {
					ArrayList com = (ArrayList)comm;
					Commands.processCommand(this, com, demobase);
				}
		} else { //I was too lazy to implement undo, so do the graph from scratch
			demobase.unselect();
			graph = new Graph2D();
			typegraph = new Graph2D();
			initGraph();
			view.setGraph2D(graph);
			typeview.setGraph2D(typegraph);
			for (int i = 0; i < step; i++)
				for (Object comm : changes.get(i)) {
					ArrayList com = (ArrayList)comm;
					Commands.processCommand(this, com, demobase);
				}
		}
		for (Object o : changes.get(step)) {
			ArrayList comm = (ArrayList)o;
			if (! Commands.processIfNoChange(this, comm, demobase))
				break;
		}
		lastslidervalue = step;
		changed = true;
		demobase.calcLayout();
	}

	public void calcSwimLanes() {
	    int i = 1;
		for (Node t : topnodes) {
			if (swimLane.get(t) == null) {
				SwimLaneDescriptor sld = new SwimLaneDescriptor(-i * 16);
				swimLane.set(t, sld);
			}
			visited = graph.createNodeMap();
			currindex = -i * 16;
			middle = -i * 16;
			step = 8;
			for (EdgeCursor ec = t.outEdges(); ec.ok(); ec.next())
				if (graph.getRealizer(ec.edge()).getLineColor() == Color.black) 
					visitComputations(ec.edge().target(), null);
			int argc = -i * 16 + 1;
			for (EdgeCursor ec = t.outEdges(); ec.ok(); ec.next()) {
				Color c = graph.getRealizer(ec.edge()).getLineColor(); 
				if (c == Color.blue) {
					int lane = 0;
					for (NodeCursor nc = ec.edge().target().successors(); nc.ok(); nc.next())
						if (swimLane.get(nc.node()) != null && ((SwimLaneDescriptor)swimLane.get(nc.node())).getClientObject() != null)
							lane += (Integer)((SwimLaneDescriptor)swimLane.get(nc.node())).getClientObject();
					if (swimLane.get(ec.edge().target()) == null) {
						int divisor = ec.edge().target().outDegree();
						int la = argc;
						if (divisor != 0)
							la = lane / divisor;
						argc++;
						SwimLaneDescriptor sld2 = new SwimLaneDescriptor(la);
						swimLane.set(ec.edge().target(), sld2);
					}
				}
			}
			i++;
		}
	}
	
	private NodeMap visited;
	private int middle = 16;
	private int currindex = 16;
	private int step = 8;

	private void update (int direction) {
		if (direction == 1)
			currindex += step;
		else if (direction == -1)
			currindex -= step;
		else if (currindex < middle)
			currindex -= step;
		else
			currindex += step;
		step /= 2;
	}
	
	private void restore (int direction) {
		step *= 2;
		if (direction == 1)
			currindex -= step;
		else if (direction == -1)
			currindex += step;
		else if (currindex < middle)
			currindex += step;
		else
			currindex -= step;
	}

	private Node visitComputations (Node t, String until) {
		final LinkedList<Node> workingset = new LinkedList<Node>();
		workingset.add(t);
		while (workingset.size() > 0) {
			Node work = workingset.poll();
			if (until != null && graph.getLabelText(work).contains(until))
				return work;
			visited.set(work, true);
			Node next = visit(work);
			if (next != null) work = next;
			for (EdgeCursor ec = work.outEdges(); ec.ok(); ec.next())
				if (graph.getRealizer(ec.edge()).getLineColor() != Color.blue) {
					Node targ = ec.edge().target();
					if (visited.get(targ) == null)
						workingset.add(targ);
				}
		}
		return null;
	}
	
	private Node nextC (Node n) {
		for (EdgeCursor ec = n.outEdges(); ec.ok(); ec.next())
			if (graph.getRealizer(ec.edge()).getLineColor() != Color.blue)
				return ec.edge().target();
		return null;
	}
	private Node visit (Node n) {
		String text = graph.getLabelText(n);
		setL(currindex, n);
		for (EdgeCursor ec = n.edges(); ec.ok(); ec.next())
			if (graph.getRealizer(ec.edge()).getLineColor() == Color.blue) {
				int weight = step;
				if (ec.edge().opposite(n).degree() == 1) {
					setLf(currindex, ec.edge().opposite(n));
				} else if (currindex < middle)
					setL(currindex + weight, ec.edge().opposite(n));
				else
					setL(currindex - weight, ec.edge().opposite(n));
			}
		if (text.contains(" if ")) {
			Node merge = null;
			int direction = 1;
			for (EdgeCursor ec = n.outEdges(); ec.ok(); ec.next())
				if (graph.getRealizer(ec.edge()).getLineColor() == Color.green) {
					if (! graph.getLabelText(ec.edge().target()).contains("IF-MERGE"))
						if (swimLane.get(ec.edge().target()) != null)
							if ((Integer)((SwimLaneDescriptor)swimLane.get(ec.edge().target())).getClientObject() < currindex)
								direction = -1;
					update(direction);
					merge = visitComputations(ec.edge().target(), "IF-MERGE");
					restore(direction);
				} else if (graph.getRealizer(ec.edge()).getLineColor() == Color.red) {
					if (! graph.getLabelText(ec.edge().target()).contains("IF-MERGE"))
						if (swimLane.get(ec.edge().target()) != null)
							if ((Integer)((SwimLaneDescriptor)swimLane.get(ec.edge().target())).getClientObject() > currindex)
								direction = -1;
					update(-1 * direction);
					Node merge2 = visitComputations(ec.edge().target(), "IF-MERGE");
					if (merge == null)
						merge = merge2;
					restore(-1 * direction);
				}
			if (merge != null) {
				setL(currindex, merge);
				visited.set(merge, true);
			}
			return merge;
		}
		if (text.endsWith(" loop")) {
			if (n.outDegree() > 0) {
				update(0);
				Node mbreak = visitComputations(n.firstOutEdge().target(), "BREAK");
				restore(0);
				if (mbreak != null) {
					setL(currindex, mbreak);
					visited.set(mbreak, true);
				}
				return mbreak;
			}
		}
		if (text.endsWith(" bind-exit")) {
			update(0);
			Node mbreak = visitComputations(nextC(n), "BIND-EXIT-MERGE");
			restore(0);
			if (mbreak != null) {
				setL(currindex, mbreak);
				visited.set(mbreak, true);
			}
			return mbreak;
		}
		return n;
	}
	
	private void setL (int num, Node n) {
		if (swimLane.get(n) == null) {
			SwimLaneDescriptor sld = new SwimLaneDescriptor(num);
			swimLane.set(n, sld);
		} else {
		    SwimLaneDescriptor sld = (SwimLaneDescriptor)swimLane.get(n);
		    int oldnum = (Integer)sld.getClientObject();
		    if ((oldnum > (middle + 16)) || (oldnum < (middle - 16)) || oldnum > (step * 2)) {
				SwimLaneDescriptor sld2 = new SwimLaneDescriptor(num);
				swimLane.set(n, sld2);
		    }
		}
	}

	private void setLf (int num, Node n) {
		SwimLaneDescriptor sld = new SwimLaneDescriptor(num);
		swimLane.set(n, sld);
	}
}


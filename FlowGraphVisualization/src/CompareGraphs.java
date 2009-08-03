import java.awt.Color;
import java.util.ArrayList;

import y.base.EdgeCursor;
import y.base.Node;
import y.base.NodeCursor;
import y.view.Graph2D;


public final class CompareGraphs {
	static public void compareColorize (IncrementalHierarchicLayout graph1, IncrementalHierarchicLayout graph2) {
		ArrayList<Node> top1 = graph1.topnodes;
		ArrayList<Node> top2 = graph2.topnodes;
		System.out.println("top1 " + top1);
		System.out.println("top2 " + top2);
		assert(top1.size() == top2.size());
		for (int i = 0; i < top1.size(); i++)
			compareSubgraph(top1.get(i), top2.get(i));
	}
	
	static public void resetComparison (IncrementalHierarchicLayout g) {
		for (NodeCursor nc = g.graph.nodes(); nc.ok(); nc.next())
			getR(nc.node()).setDifferent(false);
	}
	
	static private ArrayList<Node> cfSuccs (Node n) {
		ArrayList<Node> res = new ArrayList<Node>();
		for (EdgeCursor ec = n.outEdges(); ec.ok(); ec.next())
			if (((Graph2D)n.getGraph()).getRealizer(ec.edge()).getLineColor() != Color.BLUE)
				res.add(ec.edge().target());
		return res;
	}
	
	static private GraphNodeRealizer getR (Node n) {
		return (GraphNodeRealizer)(((Graph2D)n.getGraph()).getRealizer(n));		
	}
	
	static private void compareSubgraph (Node t1, Node t2) {
		ArrayList<Node> succ1 = cfSuccs(t1);
		ArrayList<Node> succ2 = cfSuccs(t2);
		assert(succ1.size() == succ2.size());
		for (int i = 0; i < succ1.size(); i++) {
			if (compareNode(succ1.get(i), succ2.get(i)) != 0)
				getR(succ1.get(i)).setDifferent(true);
			compareSubgraph(succ1.get(i), succ2.get(i));
		}
	}
	
	static private int compareNode (Node n, Node m) {
		if (getR(n).getComparableText().equals(getR(m).getComparableText()))
			return 0;
		return 1;
	}
}

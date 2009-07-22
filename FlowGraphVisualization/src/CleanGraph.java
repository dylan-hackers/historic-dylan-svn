import java.io.IOException;

import y.base.Node;
import y.base.NodeCursor;
import y.io.GMLIOHandler;
import y.module.HierarchicLayoutModule;
import y.view.Graph2D;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import yext.svg.io.SVGIOHandler;


public final class CleanGraph {

	private static Graph2D getGraph (String filename) throws IOException {
		GMLIOHandler gmlio = new GMLIOHandler();
		Graph2D gr = new Graph2D();
		gmlio.read(gr, filename);
		return gr;
	}
	
	private static void modifyGraph (Graph2D graph) {
		for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next())
			adapt(nc.node());
		HierarchicLayoutModule hl = new HierarchicLayoutModule();
		hl.start(graph);
	}
	
	private static void adapt (Node n) {
		adapt(((Graph2D)n.getGraph()).getRealizer(n));
	}
	
	private static void adapt (NodeRealizer nr) {
		NodeLabel nl = nr.getLabel();
		nl.setFontSize(15);
		nr.setWidth(nl.getWidth() + 2 * 20);
	}
	
	private static void exportGraph (Graph2D graph, String filename) throws IOException {
		SVGIOHandler svg = new SVGIOHandler();
		svg.write(graph, filename);
	}
	
	public static void main(String[] args) {
		if (args.length != 1) {
			System.out.println("please provide a filename");
			System.exit(-1);
		}
		String fn = args[0];
		Graph2D gr;
		try {
			gr = getGraph(fn);
			modifyGraph(gr);
			exportGraph(gr, fn.substring(0, fn.lastIndexOf('.')) + ".svg");
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(-1);
		}
	}

}

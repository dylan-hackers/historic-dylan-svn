import java.awt.Color;

import y.view.GenericNodeRealizer;
import y.view.NodeLabel;
import y.view.NodeRealizer;


public class GraphNodeRealizer extends GenericNodeRealizer {
	private String nodetext = "";
	private String nodetype = "";
	private Color nodecolor = new Color(0xbb, 0xbf, 0xff);
	private int reference = 0;
	private int identifier = 0;
	private String prefix = "";
	private String suffix = "";
	private NodeLabel label;
	//private boolean selected
	//private boolean highlight
	//private boolean optimization_queue
	private final boolean debug = true;
	
	public GraphNodeRealizer () {
		label = createNodeLabel();
		label.setFontSize(15);
		setLabel(label);
	}
	
  	public NodeRealizer createCopy(NodeRealizer r) {
	    return new GraphNodeRealizer();
	}
	  
	private void update () {
		if (debug) {
			String ref = "";
			if (reference != 0)
				ref = " [" + Integer.toString(reference) + "]";
			label.setText(Integer.toString(identifier) + ": " +  prefix + " " + nodetext + " " + suffix + ref);
		} else
			label.setText(prefix + " " + nodetext + " " + suffix);
		setWidth(label.getWidth() + 2 * 20);
		setFillColor(nodecolor);
		repaint();
	}
	
	public String getNodeText () {
		return nodetext;
	}
	
	public void setNodeText (String s) {
		nodetext = s;
		update();
	}
	
	public String getNodeType () {
		return nodetype;
	}
	
	public void setNodeType (String s) {
		nodetype = s;
		//update();
	}
	
	public Color getNodeColor () {
		return nodecolor;
	}
	
	public void setNodeColor (Color c) {
		nodecolor = c;
		update();
	}
	
	public int getReference () {
		return reference;
	}
	
	public void setReference (int r) {
		reference = r;
		update();
	}
	
	public int getIdentifier () {
		return identifier;
	}
	
	public void setIdentifier (int i) {
		identifier = i;
		update();
	}
	
	public String getPrefix () {
		return prefix;
	}
	
	public void setPrefix (String p) {
		prefix = p;
		update();
	}
	
	public String getSuffix () {
		return suffix;
	}
	
	public void setSuffix (String s) {
		suffix = s;
		update();
	}	
}

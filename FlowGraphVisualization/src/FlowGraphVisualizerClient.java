import java.net.Socket;


public class FlowGraphVisualizerClient {
	public static void main(String[] args) {
		try {
			Socket cli = new Socket("visualization.dylan-user.org", 1234);
			new LayouterClient(cli).start();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}

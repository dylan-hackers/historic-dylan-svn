import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.LinkedList;


public final class FlowGraphVisualizer {
	private static final int port = 1234;
	protected static LinkedList<LayouterClient> clients = new LinkedList<LayouterClient>();
	
	public static void main(String[] args) {
		DemoBase.initLnF();
		FlowGraphVisualizer.listen();
	}

	static void listen () {
		try {
			ServerSocket sock = new ServerSocket(port);
			while (true) {
				Socket cli = sock.accept();
				System.out.println("new connection");
				LayouterClient lc = new LayouterClient(cli);
				clients.addFirst(lc);
				lc.start();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}


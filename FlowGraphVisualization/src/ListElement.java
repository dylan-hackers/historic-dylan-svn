
public class ListElement implements Comparable {
	private String item;
	
	public ListElement (String item) {
		this.item = item;
	}
	
	public int compareTo(Object o) {
		if (o instanceof ListElement) {
			ListElement other = (ListElement)o;
			return this.item.compareTo(other.toString());
		}
		return 0;
	}
	
	public String toString() {
		return item;
	}

}

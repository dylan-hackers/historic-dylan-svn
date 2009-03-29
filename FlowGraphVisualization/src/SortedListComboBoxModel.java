import java.io.Serializable;
import java.util.Vector;

import javax.swing.AbstractListModel;
import javax.swing.MutableComboBoxModel;


public class SortedListComboBoxModel extends AbstractListModel implements
		MutableComboBoxModel, Serializable {

	private Vector elements;
	private Object selectedItem = null;
	
	public SortedListComboBoxModel () {
		elements = new Vector();
	}
	public void addElement(Object arg0) {
		insertElementAt(arg0, 1);
	}

	public void insertElementAt(Object element, int foo) {
		if (getSize() < 2)
			elements.insertElementAt(element, getSize());
		else
			for (int index = 1; index < getSize(); index++)
			{
				Comparable c = (Comparable)getElementAt( index );

				if (c.compareTo(element) > 0) {
					elements.insertElementAt(element, index);
					break;
				}
				if (index == getSize() - 1) {
					elements.add(element);
					break;
				}
			}
		fireIntervalAdded(this, 0, getSize());
	}

	public void removeElement(Object arg0) {
		elements.remove(arg0);
	}

	public void removeElementAt(int arg0) {
		elements.remove(arg0);
	}

	public Object getSelectedItem() {
		return selectedItem;
	}

	public void setSelectedItem(Object arg0) {
		selectedItem = arg0;
	    fireContentsChanged(this, -1, -1);
	}

	public Object getElementAt(int arg0) {
		return elements.get(arg0);
	}

	public int getSize() {
		return elements.size();
	}

}

package sigue.btrack;

/**
 * A simple way of associating a number with some object.  Intended to
 * be used for all the code that presents lists of numbered options in
 * web pages.
 */
public class Option {

    private int number;
    private Object value;

    public Option (int n, Object v) {
        number = n;
        value = v;
    }

    public int getNumber () { return number; }
    public Object getValue () { return value; }

    public String toString () {
        return value.toString();
    }
}

package sigue.btrack;

/**
 * Wraps SQLExceptions into a runtime exception so it needn't
 * be declared in the throws clause of every bloody method
 * under the sun.
 */
public class DatabaseException extends BugTrackException {

    private Exception origin;

    public DatabaseException (Exception origin, String msg) {
        super(msg);
        this.origin = origin;
    }

    public String toString () {
        return (getMessage()
                + "  Original error: "
                + origin.getMessage());
    }

}

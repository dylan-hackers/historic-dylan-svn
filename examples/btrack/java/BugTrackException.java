package sigue.btrack;

public class BugTrackException extends RuntimeException {

    public BugTrackException (String msg) {
        super(msg);
    }

    public BugTrackException (Exception e) {
        super(e.toString());
    }

    public String toString () {
        return getMessage();
    }
}

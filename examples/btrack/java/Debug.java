package sigue.btrack;

/**
 * Simple conditional debug output.
 */
public final class Debug {
    private static boolean debugp = true;

    public static void setDebug (boolean state) {
        debugp = state;
    }

    public static boolean getDebug () {
        return debugp;
    }

    public static void println (String s) {
        if (debugp) {
            System.err.print("--> ");
            System.err.println(s);
        }
    }

    /**
     * Print a message with the current millisecond clock prepended.
     */
    public static void tprintln (String msg) {
        println(System.currentTimeMillis() + " " + Thread.currentThread() + ": " + msg);
    }

    public static void backtrace () {
        backtrace(new Exception());
    }

    public static void backtrace (Exception e) {
        if (debugp) {
            if (e == null) {
                backtrace();
            } else {
                e.printStackTrace();
            }
        }
    }

    /*
      public static void assert (boolean valid, String error) {
      if (!valid) {
      if (debugp) {
      AssertionFailureException e
      = new AssertionFailureException("Assertion failed: " + error);
      backtrace(e);
      throw e;
      }
      else 
      Debug.println(error);
      }
      }
    */
}

package sigue.btrack;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.*;

public class BaseServlet extends HttpServlet {

    /**
     * Used for login (for example), where we need to redirect the request
     * to the login page and then later go back to the original requested URL.
     * This is used to get the original URL so it can be stored in the session.
     */
    protected String getTargetURL (HttpServletRequest request) {
        String uri = request.getServletPath();
        String query = request.getQueryString();
        return (query == null
                ? uri
                : uri + "?" + query);
    }

    public void doGet (HttpServletRequest request, HttpServletResponse response)
        throws ServletException, IOException
    {
        HttpSession session = request.getSession(true);
        try {
            Debug.println("origin = " + request.getParameter("origin"));
            Object origin = request.getParameter("origin");
            if (origin != null) {
                session.setAttribute("origin", origin);
            }
            handleGet(request, response, session);
        } catch (BugTrackException e) {
            Util.noteError(session, e.toString());
        }
    }

    public void doPost (HttpServletRequest request, HttpServletResponse response)
        throws ServletException, IOException
    {
        HttpSession session = request.getSession(true);
        try {
            Debug.println("origin = " + request.getParameter("origin"));
            Object origin = request.getParameter("origin");
            if (origin != null) {
                session.setAttribute("origin", origin);
            }
            handlePost(request, response, session);
        } catch (BugTrackException e) {
            Util.noteError(session, e.toString());
            request.getRequestDispatcher("/home.html").forward(request, response);
        }
    }

    protected void handleGet (HttpServletRequest request,
                              HttpServletResponse response,
                              HttpSession session)
        throws ServletException, IOException
    {
        // do nothing
    }

    protected void handlePost (HttpServletRequest request,
                               HttpServletResponse response,
                               HttpSession session)
        throws ServletException, IOException
    {
        handleGet(request, response, session);
    }
}

package sigue.btrack;

import java.io.*;
import java.util.*;
import java.sql.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class RecordServlet extends BaseServlet
{

    private String ackPage (HttpSession session) {
        String origin = (String) session.getAttribute("origin");
        return "/" + (origin == null ? "home" : origin) + ".html";
    }

    /**
     * This is called for POST method as well, unless we override the
     * handlePost method also.
     */
    public void handleGet (HttpServletRequest request,
                           HttpServletResponse response,
                           HttpSession session)
        throws ServletException, IOException
    {
        // You must always be logged in to edit or create any record.
        if (!Util.isLoggedIn(session)) {
            session.setAttribute("login-target", getTargetURL(request));
            RequestDispatcher rd = request.getRequestDispatcher("/login.html");
            rd.forward(request, response);
            return;
        }
        String recordType = (String) request.getParameter("type");
        String className = Util.packageName(DatabaseRecord.class) + "." + recordType;
        DatabaseRecord proto = null;
        try {
            proto = DatabaseRecord.classPrototype(Class.forName(className));
        } catch (ClassNotFoundException e) {
            Debug.backtrace(e);
            Util.noteError(session, "Internal error: " + e);
            return;
        }
        if (proto.editRequiresAdminPrivs() && !Util.isAdmin(session)) {
            Util.noteError(session,
                           "You must be an administrator to create or edit a "
                           + proto.prettyName() + ".");
            request.getRequestDispatcher(ackPage(session)).forward(request, response);
        } else {
            // ...create, edit, or save...
            String idParam = request.getParameter("id");
            String action = request.getParameter("action");
            if (idParam != null) {
                doEdit(proto, recordType, idParam, request, response, session);
            } else if ("save".equalsIgnoreCase(action)) {
                doSave(proto, request, response, session); // Responding to a submitted edit form...
                request.getRequestDispatcher(ackPage(session)).forward(request, response);
            }
        }
    }

    /**
     * Respond to a request to create or edit a record.
     */
    private void doEdit (DatabaseRecord proto,
                         String recordType,
                         String idParam,
                         HttpServletRequest request,
                         HttpServletResponse response,
                         HttpSession session)
        throws ServletException, IOException
    {
        proto.prepareForEdit(request, session);
        DatabaseRecord record = null;
        if ("new".equalsIgnoreCase(idParam)) {
            try {
                record = (DatabaseRecord) proto.getClass().newInstance();
                record.initializeNewRecord(request, session);
            } catch (Exception e1) {
                Debug.backtrace(e1);
                Util.noteError(session, "Internal error: " + e1);
                return;
            }
        } else {
            try {
                Integer id = new Integer(idParam);
                record = DatabaseRecord.loadRecord(id, proto.getClass(), true);
            } catch (NumberFormatException nfe) {
                Util.noteError(session, "The requested " + proto.prettyName() + " (id = "
                               + idParam + ") was not found.");
                request.getRequestDispatcher(ackPage(session)).forward(request, response);
                return;
            }
        }
        Debug.println("RecordServlet.doEdit: Setting 'record' to " + record);
        session.setAttribute("record", record);
        String editPage = "/edit-" + recordType.toLowerCase() + ".html";
        request.getRequestDispatcher(editPage).forward(request, response);
    }

    /**
     * Respond to a post from edit-xxx.html.  Save the (possibly new) record
     * to the database.
     */
    private void doSave (DatabaseRecord proto,
                         HttpServletRequest request,
                         HttpServletResponse response,
                         HttpSession session)
        throws ServletException, IOException
    {
        DatabaseRecord record = (DatabaseRecord) session.getAttribute("record");
        if (record == null) {
            Util.noteError(session,
                           "Internal error: No "
                           + proto.prettyName() + " was found for saving.");
            return;
        } else {
            //---TODO: Consider having record descriptors that say what fields
            // need to be retrieved from the request, and just pass the values
            // to the record.save(...) method.
            record.respondToEditForm(request, session);
        }
    }

}

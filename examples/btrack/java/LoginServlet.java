package sigue.btrack;

import java.io.*;
import java.util.*;
import java.sql.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Respond to the login form.  If login incorrect, redisplay login form.
 * If login successful, redirect to whatever target was specified.
 * If no target was specified go to home page.
 */
public class LoginServlet extends HttpServlet {

    public void doPost (HttpServletRequest request, HttpServletResponse response)
        throws ServletException, IOException
    {
        HttpSession session = request.getSession(true);
        String username = request.getParameter("username");
        String password = request.getParameter("password");
        if (Util.validateLogin(session, username, password)) {
            Util.noteMessage(session, "Login successful");
            String target = (String) session.getAttribute("login-target");
            session.removeAttribute("login-target");
            request.getRequestDispatcher(target == null ? "/home.html" : target).forward(request, response);
        } else {
            // Login failed.  Redisplay login page.
            Util.noteError(session, "Incorrect login");
            request.getRequestDispatcher("/login.html").forward(request, response);
        }
    }

}

package sigue.btrack;

import java.sql.*;
import java.util.AbstractList;
import java.util.Hashtable;
import javax.servlet.http.*;

/**
 * A module is part of a product.  I.e., a product may have several
 * modules.  A module is "owned" by the user account that has primary
 * responsibility for the bugs reported against the module.  Practically
 * speaking this will probably mean the account that will receive e-mail
 * for each new bug reported against the module.
 */
public class Module extends OwnedRecord {

    private static boolean all_loaded = false;

    private Product product;

    public Module () {
        super();
    }

    protected Module (Integer id) {
        super(id);
    }

    public Product getProduct () { return product; }
    public void    setProduct (Product p) {
        if (p != null) {
            product = p;
        }
    }

    private static Module prototype = new Module(new Integer(DatabaseRecord.NO_RECORD));

    public static Module getPrototype () {
        return Module.prototype;
    }


    protected String tableName () {
        return "tbl_module";
    }

    protected String recordIDColumnName () {
        return "module_id";
    }

    public static Module loadModule (Integer id) {
        return (Module) DatabaseRecord.loadRecord(id, Module.class, true);
    }

    /**
     * This assumes the cursor of the given result set is pointing at a valid
     * row, and that is the row to be used.
     */
    public static Module loadModule (ResultSet rset)
    {
        return (Module) DatabaseRecord.loadRecord(rset, Module.class);
    }

    //---TODO: This could check for a cached record before hitting the DB.
    public static Module loadModuleByName (String name) {
        return (Module) DatabaseRecord.loadRecord("select * from tbl_module where name = '"
                                                  + name + "'",
                                                  Module.class);
    }

    /**
     * @see DatabaseRecord#initializeNewRecord
     */
    public void initializeNewRecord (HttpServletRequest request,
                                     HttpSession session) {
        int product_id = Util.parseInt(Util.getRequiredField(request, session, "productid"), 0);
        if (product_id != 0) {
            setProduct(Product.loadProduct(new Integer(product_id)));
        }
    }

    /**
     * The normal sequence of pages is list-xxx -> edit-xxx -> list-xxx.
     * However, for modules it is edit-product -> edit-module ->
     * edit-product.  All edit-xxx pages expect to find the record being
     * edited to be in the session under the "record" attribute.  This
     * method ensures that the "record" attribute is set back to the
     * product when we return to the edit-product page.
     */
    // ---TODO: This feels like a kludge.  Find a better way to handle it.
    public void respondToEditForm (HttpServletRequest request,
                                   HttpSession session)
    {
        String pid = request.getParameter("productid");
        if (pid != null) {
            try {
                int product_id = Integer.parseInt(pid);
                Product prod = Product.loadProduct(new Integer(product_id));
                if (prod == null) {
                    Util.noteError(session, "Internal error: No product with ID "
                                   + product_id + " was found.");
                } else {
                    setProduct(prod);
                }
            }
            catch (NumberFormatException e) {
                Util.noteError(session, "Internal error: Illegal productid: " + pid);
            }
        }
        super.respondToEditForm(request, session);
        Debug.println("Module.respondToEditForm: Setting 'record' to " + getProduct());
        session.setAttribute("record", getProduct());
    }

    public void save (Connection conn) throws SQLException {
        boolean is_new = isNew();
        String query
            = (is_new
               ? ("insert into " + tableName() + " (" + recordIDColumnName()
                  + ",mod_count,date_entered,date_modified,name,status,product,owner) values (?,?,?,?,?,?,?,?)")
               : ("update " + tableName() + " set mod_count = ?, date_entered = ?, "
                  + "date_modified = ?, name = ?, status = ?, product = ?, owner = ? where "
                  + recordIDColumnName() + " = ?"));
        int idx = 1;
        PreparedStatement ps = conn.prepareStatement(query);
        if (is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        int new_mod_count = mod_count.intValue() + 1;
        ps.setInt(idx++, new_mod_count);
        ps.setTimestamp(idx++, date_entered);
        ps.setTimestamp(idx++, date_modified); // ---TODO: set to current time
        ps.setString(idx++, getName());
        ps.setString(idx++, getStatus());
        ps.setInt(idx++, getProduct().getId().intValue());
        ps.setInt(idx++, getOwner().getId().intValue());
        if (!is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        ps.executeUpdate();
        ps.close();
        // Only increment the mod_count if there was no exception during the save.
        setModCount(new Integer(new_mod_count));
    }

    /**
     * Implements the CollectionGenerator interface for IterateTag.
     */
    public AbstractList generateCollection (HttpSession session) {
        Product prod = (Product) session.getAttribute("record");
        String query = "select * from tbl_module where product = " + prod.getId();
        return DatabaseRecord.loadRecords(query, Module.class);
    }

}

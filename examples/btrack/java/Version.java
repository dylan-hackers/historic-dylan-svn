package sigue.btrack;

import java.sql.*;
import java.util.AbstractList;
import javax.servlet.http.*;

/**
 * Represents a product version record.
 */
public class Version extends NamedRecord {
    
    //// Constructors...

    public Version () {
        super();
        initDefaults();
    }

    private Version (Integer id) {
        super(id);
        initDefaults();
    }

    private void initDefaults () {
        setProduct(Product.getPrototype());
    }



    //// Database fields...

    private Product product;

    

    //// Getters...

    public Product getProduct () { return product; }



    //// Setters...

    public void setProduct (Product p) { product = p; }



    //// The rest...

    private static Version prototype = new Version(new Integer(DatabaseRecord.NO_RECORD));

    public static Version getPrototype () {
        return Version.prototype;
    }

    protected String tableName () {
        return "tbl_version";
    }

    protected String recordIDColumnName () {
        return "version_id";
    }

    public static Version loadVersion (Integer id) {
        return (Version) DatabaseRecord.loadRecord(id, Version.class, true);
    }

    public static Version loadVersion (ResultSet rset)
    {
        return (Version) DatabaseRecord.loadRecord(rset, Version.class);
    }

    /**
     * The normal sequence of pages is list-xxx -> edit-xxx -> list-xxx.
     * However, for versions it is edit-product -> edit-version ->
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
        Debug.println("Version.respondToEditForm: Setting 'record' to " + getProduct());
        session.setAttribute("record", getProduct());
    }

    public void save (Connection conn) throws SQLException {
        boolean is_new = isNew();
        String query
            = (is_new
               ? ("insert into " + tableName() + " (" + recordIDColumnName()
                  + ",mod_count,date_entered,date_modified,name,status,product) values (?,?,?,?,?,?,?)")
               : ("update " + tableName() + " set mod_count = ?, date_entered = ?, "
                  + "date_modified = ?, name = ?, status = ?, product = ? where "
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
        if (!is_new) {
            ps.setInt(idx++, getId().intValue());
        }
        ps.executeUpdate();
        ps.close();
        // Only increment the mod_count if there was no exception during the save.
        setModCount(new Integer(new_mod_count));
    }

    /**
     * @see DatabaseRecord#initializeNewRecord
     */
    public void initializeNewRecord (HttpServletRequest request,
                                     HttpSession session)
    {
        int product_id = Util.parseInt(Util.getRequiredField(request, session, "productid"), 0);
        if (product_id != 0) {
            setProduct(Product.loadProduct(new Integer(product_id)));
        }
    }

    public AbstractList generateCollection (HttpSession session) {
        Product prod = (Product) session.getAttribute("record");
        String query = "select * from tbl_version where product = " + prod.getId();
        return DatabaseRecord.loadRecords(query, Version.class);
    }

}

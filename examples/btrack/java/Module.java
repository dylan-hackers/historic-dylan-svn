package sigue.btrack;

import java.sql.*;
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
        return (Module) DatabaseRecord.loadRecord(id, Module.class);
    }

    /**
     * This assumes the cursor of the given result set is pointing at a valid
     * row, and that is the row to be used.
     */
    public static Module loadModule (ResultSet rset, boolean must_be_unique)
        throws BugTrackException
    {
        return (Module) DatabaseRecord.loadRecord(rset, Module.class, must_be_unique);
    }

    //---TODO: This could check for a cached record before hitting the DB.
    public static Module loadModuleByName (String name) {
        return (Module) DatabaseRecord.loadRecord("select * from tbl_module where name = '"
                                                  + name + "'",
                                                  Module.class);
    }

    public void initializeNewRecord (HttpServletRequest request,
                                     HttpSession session) {
        int product_id = Util.parseInt(Util.getRequiredField(request, session, "productid"), 0);
        if (product_id != 0) {
            setProduct(Product.loadProduct(new Integer(product_id)));
        }
    }

}

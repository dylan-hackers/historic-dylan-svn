package sigue.btrack;

import java.sql.*;
import java.util.Hashtable;

/**
 * A product represents, well, a product.  For example, something you
 * think of as a unified whole, or something that can be delivered to
 * end-users.  It may be subdivided into modules.  A product is "owned"
 * by the user account with primary responsibility for handling bug
 * reports about the product.  Practically speaking this will probably
 * mean the account that will receive e-mail for each new bug reported
 * against the product.
 */
public class Product extends OwnedRecord {

    public Product () {
        super();
    }

    private Product (Integer id) {
        super(id);
    }

    private static Product prototype = new Product(new Integer(DatabaseRecord.NO_RECORD));

    public static Product getPrototype () {
        return Product.prototype;
    }


    protected String tableName () {
        return "tbl_product";
    }

    protected String recordIDColumnName () {
        return "product_id";
    }

    public static Product loadProduct (Integer id) {
        return (Product) DatabaseRecord.loadRecord(id, Product.class, true);
    }

    public static Product loadProduct (ResultSet rset)
    {
        return (Product) DatabaseRecord.loadRecord(rset, Product.class);
    }

}

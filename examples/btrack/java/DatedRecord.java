package sigue.btrack;

import java.sql.*;

/*
 * ---TODO:
 * - When getting a list of objects of any kind, need to check whether any new
 *   records of that type have been created since the list was last retrieved.
 *   If so, update the list from the database.  This means caching the number
 *   of records of each type.
 */

public abstract class DatedRecord extends DatabaseRecord {

    private static int NEW_MOD_COUNT = 0; // record never saved to DB yet

    protected Integer mod_count;
    protected java.sql.Timestamp date_entered;
    protected java.sql.Timestamp date_modified;

    protected DatedRecord () {
        super();
        initDefaults();
    }

    protected DatedRecord (Integer id) {
        super(id);
        initDefaults();
    }

    private void initDefaults () {
        this.mod_count = new Integer(NEW_MOD_COUNT);
        this.date_entered = new java.sql.Timestamp(System.currentTimeMillis());
        this.date_modified = new java.sql.Timestamp(System.currentTimeMillis());
    }


    //// Getters...

    public Integer   getModCount ()     { return mod_count; }
    public Timestamp getDateEntered ()  { return date_entered; }
    public Timestamp getDateModified () { return date_modified; }


    //// Setters...

    protected void setModCount (Integer mc) { mod_count = mc; }
    protected void setDateModified (Timestamp t) { date_modified = t; }


    //// The rest...

    /**
     * A database object is considered new if it hasn't ever been written
     * to the database.
     */
    public boolean isNew () {
        return mod_count.intValue() == NEW_MOD_COUNT;
    }

    protected void initializeFromRow (ResultSet rset) throws SQLException {
        //super.initializeFromRow(rset);  
        this.mod_count = new Integer(rset.getInt("mod_count"));
        this.date_entered = rset.getTimestamp("date_entered");
        this.date_modified = rset.getTimestamp("date_modified");
    }
            
}

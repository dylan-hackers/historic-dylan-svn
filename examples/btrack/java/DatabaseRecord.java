package sigue.btrack;

import java.sql.*;
import javax.servlet.*;         // should figure out how to not need this.
import javax.servlet.http.*;    // ditto.
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Enumeration;

/*
 * ---TODO:
 * - When getting a list of objects of any kind, need to check whether any new
 *   records of that type have been created since the list was last retrieved.
 *   If so, update the list from the database.  This means caching the number
 *   of records of each type.
 */

/**
 * Superclass of all classes that represent objects stored in the
 * database.
 */
public abstract class DatabaseRecord
    implements CollectionGenerator
{
    /**
     * Used for many columns that contain record ids to signify that there is no legitimate
     * value in the column.  The hope is that this will make some logic easier than if nulls
     * were allowed, but we'll see how it goes.
     */
    public static final int NO_RECORD = 0;

    private   Integer record_id;


    //// Constructors...

    protected DatabaseRecord () {
        this(new Integer(Util.getUniqueID()));
    }

    protected DatabaseRecord (Integer id) {
        this.record_id = id;
    }


    //// Getters...

    public Integer   getId () { return record_id; }


    //// Setters...

    private void setId (Integer id) { record_id = id; }


    //// Abstract methods...
    
    /**
     * Returns the name of the database table containing records for the class.
     */
    protected abstract String tableName ();

    /**
     * This method gives the subclass a chance to initialize any of its
     * fields from the ResultSet when a record is loaded from the database.
     */
    protected abstract void initializeFromRow (ResultSet row)
        throws SQLException;

    protected abstract String recordIDColumnName ();

    /**
     * Called when a form for editing a record has been submitted.  Subclasses
     * should implement this method to pull fields out of the request, parse
     * them, validate them, and store them in fields before calling save()
     */
    public abstract void respondToEditForm (HttpServletRequest request,
                                            HttpSession session);

    /**
     * Called before an edit-xxx page is displayed.  Gives the record itself
     * a chance to store something in the session, or whatever.
     */
    public void prepareForEdit (HttpServletRequest request,
                                HttpSession session) {
    }

    /**
     * Called after a new record is created and is about to be edited.
     * This gives each record type a chance to initialize any fields
     * based on the request that asked the record to be created.  e.g.,
     * the Version and Module records need to extract the product_id
     * from the query and store the correct product in the new record.
     */
    public void initializeNewRecord (HttpServletRequest request,
                                     HttpSession session)
    {
    }


    /**
     * Implements the CollectionGenerator interface for IterateTag.
     */
    public AbstractList generateCollection (HttpSession session) {
        return listAll();
    }

    /**
     * @return the user visible name of the record type.  For example, the
     * Account class might return "user account".
     */
    //---TODO: Make this abstract and implement it in all subclasses.
    public String prettyName () {
        return "record";
    }


    //// The rest...

    /**
     * Subclasses should override this if they DON'T require admin priveleges
     * in order for their database records to be created or edited.
     */
    public boolean editRequiresAdminPrivs () {
        return true;
    }

    private static final java.util.Hashtable cache = new java.util.Hashtable();

    private static DatabaseRecord getRecord (Integer id) {
        return (DatabaseRecord) cache.get(id);
    }

    public void cacheRecord () {
        if (getRecord(record_id) != null) {
            Debug.println("Warning: Replacing a cached record with ID = " + record_id);
        }
        DatabaseRecord.cache.put(record_id, this);
    }

    private static DatabaseRecord newDatabaseRecord (Class recordClass) {
        try {
            return (DatabaseRecord) recordClass.newInstance();
        } catch (InstantiationException ie) {
            Debug.backtrace(ie);
        } catch (IllegalAccessException iae) {
            Debug.backtrace(iae);
        }
        return null;
    }

    protected String loadRecordQuery () {
        return "select * from " + tableName() + " where " + recordIDColumnName() + " = ?";
    }

    private static DatabaseRecord loadRecordInternal (ResultSet rset, Class recordClass)
        throws SQLException
    {
        return loadRecordInternal(rset, recordClass, null);
    }

    private static DatabaseRecord loadRecordInternal (ResultSet rset, Class recordClass, Integer id)
        throws SQLException
    {
        DatabaseRecord prototype = classPrototype(recordClass);
        int row_id = rset.getInt(prototype.recordIDColumnName());
        if (id != null && id.intValue() != row_id) {
            throw new BugTrackException("Record ID (" + id
                                        + ") does not match the ID in the database ("
                                        + row_id + ").");
        } else {
            Integer recid = (id == null) ? new Integer(row_id) : id;
            DatabaseRecord record = getRecord(recid);
            if (record == null) {
                record = newDatabaseRecord(recordClass);
            }
            record.setId(recid);
            // Delegate to subclasses to initialize any subclass fields from the ResultSet.
            record.initializeFromRow(rset);
            record.cacheRecord();
            return record;
        }
    }
            
    public static DatabaseRecord loadRecord (ResultSet rset, Class recordClass) {
        try {
            return loadRecordInternal(rset, recordClass, null);
        } catch (SQLException e) {
            Debug.backtrace(e);
            throw new DatabaseException(e, "Unable to load record of class " + recordClass);
        }
    }

    public static DatabaseRecord loadRecord (Integer id, Class recordClass, boolean load_latest) {
        try {
            DatabaseRecord record = getRecord(id);
            if (record == null || load_latest == true) {
                // assert(id != null)
                if (id.intValue() == 0) {
                    return classPrototype(recordClass);
                }
                DatabaseRecord prototype = classPrototype(recordClass);
                Connection conn = Util.openDatabaseConnection();
                PreparedStatement ps = conn.prepareStatement(prototype.loadRecordQuery());
                ps.setInt(1, id.intValue());
                ResultSet rset = ps.executeQuery();
                if (!rset.next()) {
                    throw new BugTrackException("No record was found with ID " + id + "!");
                }
                return loadRecordInternal(rset, recordClass, id);
            }
            if (!recordClass.isInstance(record)) {
                Debug.println("loadRecord: Found a record but it is not the correct type.  "
                              + " id = " + id + ", requested class = " + recordClass
                              + ", actual class = " + record.getClass());
                return null;
            } else {
                // We've found a record of the correct type.  Now check whether it is
                // up-to-date by comparing the mod_count with the value in the database.
                // ---TODO: Not yet implemented.
                return record;
            }
        } catch (SQLException e) {
            Debug.backtrace(e);
            throw new DatabaseException(e, "Unable to load record with ID " + id);
        }
    }

    //---TODO: This could check for a cached account before hitting the DB.
    public static DatabaseRecord loadRecord (String query, Class recordClass) {
        try {
            Connection conn = Util.openDatabaseConnection();
            Statement stmt = conn.createStatement();
            ResultSet rset = stmt.executeQuery(query);
            if (rset.next()) {
                return loadRecord(rset, recordClass);
            }
        } catch (Exception e) {
            Debug.backtrace(e);
            throw new DatabaseException(e,
                                        "Unable to load record using query '"
                                        + query + "'.");
        }
        return null;
    }

    public static AbstractList loadRecords (String query, Class recordClass) {
        ArrayList records = new ArrayList();
        try {
            Connection conn = Util.openDatabaseConnection();
            Statement stmt = conn.createStatement();
            ResultSet rset = stmt.executeQuery(query);
            while (rset.next()) {
                DatabaseRecord rec = loadRecordInternal(rset, recordClass, null);
                if (rec != null) {
                    records.add(rec);
                }
            }
        } catch (Exception e) {
            Debug.backtrace(e);
            throw new DatabaseException(e,
                                        "Unable to load records using query '"
                                        + query + "'.");
        }
        return records;
    }
        

    /**
     * Save the record to the database.  If the record has never been saved a new
     * row will be inserted.  Otherwise the existing row will be updated.  We
     * always update all the columns in the row.  Eventually might be interesting
     * to update only the changed elements.  (Keep a hash table of changed values,
     * where the key is the column name.)
     */
    public final void save () {
        Connection conn = null;
        Exception error = null;
        try {
            conn = Util.openDatabaseConnection();
            save(conn);
            conn.commit();
            cacheRecord();  // in case it's a new record
        }
        catch (Exception e1) {
            error = e1;
            Debug.backtrace(e1);
            try {
                conn.rollback();
            }
            catch (SQLException e2) {}
        }
        finally {
            if (conn != null) {
                try {
                    conn.close();
                }
                catch (SQLException e) {}
            }
            if (error != null) {
                throw new DatabaseException(error,
                                            "Unable to save record (ID = "
                                            + getId() + ") to database.");
            }
        }
    }

    /**
     * This method does the actual saving.  The save() method above takes care
     * of establishing the connection, error handling, etc.
     */
    protected abstract void save (Connection conn) throws Exception;

    public static DatabaseRecord classPrototype (Class recordClass) {
        try {
            java.lang.reflect.Method m = recordClass.getMethod("getPrototype", null);
            return (DatabaseRecord) m.invoke(null, new Object[0]);
            //record = (DatabaseRecord) recordClass.newInstance(DatabaseRecord.NO_RECORD);
        } catch (Exception e) {
            throw new BugTrackException("Internal error: " + e);
        }
    }

    /**
     * Load all records of the given class into the cache.
     */
    //---TODO: Cache the results.
    private static void loadAll (Class recordClass) {
        DatabaseRecord prototype = classPrototype(recordClass);
        try {
            Connection conn = Util.openDatabaseConnection();
            Statement stmt = conn.createStatement();
            ResultSet rset = stmt.executeQuery("select * from " + prototype.tableName());
            int num_loaded = 0;
            while (rset.next()) {
                loadRecord(rset, recordClass);
                ++num_loaded;
            }
            Debug.println("DatabaseRecord.loadAll: loaded " + num_loaded + " "
                          + recordClass + " records.");
        } catch (SQLException e) {
            Debug.backtrace(e);
        }
    }

    public static AbstractList recordsOfType (Class type) {
        ArrayList list = new ArrayList();
        Enumeration enum = cache.elements();
        while (enum.hasMoreElements()) {
            Object o = enum.nextElement();
            if (type.isInstance(o)) {
                list.add(o);
            }
        }
        Debug.println("DatabaseRecord.recordsOfType: found " + list.size() + " " + type + " records.");
        return list;
    }

    /**
     * Returns a list of all records of the same class as the current instance.
     * @see #listRecords
     */
    public AbstractList listAll () {
        return DatabaseRecord.listRecords(getClass());
    }

    /**
     * Returns a list of all records of the given class.
     */
    public static AbstractList listRecords (Class recordClass) {
        DatabaseRecord.loadAll(recordClass);       // cache all objects of this class
        AbstractList records = recordsOfType(recordClass);
        Debug.println("DatabaseRecord.listRecords: found "
                      + records.size() + " records of class " + recordClass);
        return records;
    }

}

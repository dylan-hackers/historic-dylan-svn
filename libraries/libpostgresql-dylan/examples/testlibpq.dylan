module: testlibpq
use-libraries: common-dylan, streams, format, standard-io, io, dylan, postgresql, melange-support
use-modules: common-dylan,streams, format, standard-io, format-out, extensions, postgresql, melange-support

/*
 * testlibpq.dylan
 *		Test the Dylan version of LIBPQ, the POSTGRES frontend library.
 *
 * Original C version from the PostgreSQL distribution.
 * Dylan translation by Brent Fulgham, released into the Public Domain.
 */

define method bad-connection(conn :: <postgresql-connection>,
                          dbName) => ();
    format(*standard-error*, "Connection to database '%s' failed.\n", dbName);
	format(*standard-error*, "%s", PQerrorMessage(conn));
	PQfinish(conn);
    force-output(*standard-output*);
    exit();
end method bad-connection;

define method exit-nicely(conn :: <postgresql-connection>,
                          res :: <postgresql-result>,
                          msg) => ();
    format(*standard-error*, msg);
	PQclear(res);
	PQfinish(conn);
    force-output(*standard-output*);
    exit();
end method exit-nicely;

define method main(progname, #rest arguments) => ();

/*
#ifdef DEBUG
	FILE	   *debug;
#endif   // DEBUG
*/

    format(*standard-output*, "Starting!\n");
    force-output(*standard-output*);

	//
	// begin, by setting the parameters for a backend connection if the
	// parameters are null, then the system will try to use reasonable
	// defaults by looking up environment variables or, failing that,
	// using hardwired constants
	//
	let pghost = "";		// host name of the backend server
	let pgport = "";		// port of the backend server
	let pgoptions = ""; 	// special options to start up the backend server
	let pgtty = "";     	// debugging tty for the backend server
	let dbName = "template1";

	// make a connection to the database
    let conn :: <postgresql-connection>
        = PQsetdb(pghost, pgport, pgoptions, pgtty, dbName);

	// check to see that the backend connection was successfully made
	if (PQstatus(conn) == $ConnStatusType$CONNECTION-BAD)
		bad-connection(conn, dbName);
    end if;

/*
#ifdef DEBUG
	debug = fopen("/tmp/trace.out", "w");
	PQtrace(conn, debug);
#endif   // DEBUG
*/

	// start a transaction block
	let res = PQexec(conn, "BEGIN");
	if (PQresultStatus(res) ~= $ExecStatusType$PGRES-COMMAND-OK)
		exit-nicely(conn, res, "BEGIN command failed\n");
	end if;

	//
	// should PQclear PGresult whenever it is no longer needed to avoid
	// memory leaks
	//
	PQclear(res);

	//
	// fetch instances from the pg_database, the system catalog of
	// databases
	//
	res = PQexec(conn, "DECLARE myportal CURSOR FOR select * from pg_database");
	if (PQresultStatus(res) ~= $ExecStatusType$PGRES-COMMAND-OK)
		exit-nicely(conn, res, "DECLARE CURSOR command failed\n");
	end if;
	PQclear(res);

	res = PQexec(conn, "FETCH ALL in myportal");
	if (PQresultStatus(res) ~= $ExecStatusType$PGRES-TUPLES-OK)
        exit-nicely(conn, res, "FETCH ALL command didn't return tuples properly\n");
	end if;

	// first, print out the attribute names
	let nFields = PQnfields(res);
	for (i from 0 below nFields)
        format(*standard-output*, "%-15s", PQfname(res, i));
    end for;
	format(*standard-output*, "\n\n");
    force-output(*standard-output*);
        

	// next, print out the instances
	for (i from 0 below PQntuples(res))
		for (j from 0 below nFields)
			format(*standard-output*, "%-15s", PQgetvalue(res, i, j));
        end for;
		format(*standard-output*, "\n");
        force-output(*standard-output*);
    end for;	

	PQclear(res);

	// close the portal
	res = PQexec(conn, "CLOSE myportal");
	PQclear(res);

	// end the transaction
	res = PQexec(conn, "END");
	PQclear(res);

	// close the connection to the database and cleanup

	PQfinish(conn);

    format(*standard-error*, "Ending!\n");
    force-output(*standard-error*);
/*
#ifdef DEBUG
	fclose(debug);
#endif   // DEBUG
*/
end method main;

main(application-name(), application-arguments());

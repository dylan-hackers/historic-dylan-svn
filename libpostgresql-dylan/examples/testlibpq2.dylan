module: testlibpq2
use-libraries: common-dylan, streams, format, standard-io, io, dylan, postgresql, melange-support
use-modules: common-dylan, system, streams, format, standard-io, format-out, extensions, postgresql, melange-support

/*
 * testlibpq2.c
 *		Test of the asynchronous notification interface
 *
 * Original C version from the PostgreSQL distribution.
 * Dylan translation by Brent Fulgham, released into the Public Domain.
 *
 * populate a database with the following:

CREATE TABLE TBL1 (i int4);

CREATE TABLE TBL2 (i int4);

CREATE RULE r1 AS ON INSERT TO TBL1 DO (INSERT INTO TBL2 values (new.i); NOTIFY TBL2);

 * Then start up this program
 * After the program has begun, do

INSERT INTO TBL1 values (10);

 *
 *
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
    force-output(*standard-error*);
	exit();
end method exit-nicely;

define method main(progname, #rest arguments) => ();
	//
	// begin, by setting the parameters for a backend connection if the
	// parameters are null, then the system will try to use reasonable
	// defaults by looking up environment variables or, failing that,
	// using hardwired constants
	//
	let pghost = $null-string;	    // host name of the backend server
	let pgport = $null-string;	    // port of the backend server
	let pgoptions = $null-string;	// special options to start up the
                                    // backend server
	let pgtty = $null-string;		// debugging tty for the backend server
	let dbName = getenv("USER");	// change this to the name of your test
							    	// database

	// make a connection to the database
	let conn = PQsetdb(pghost, pgport, pgoptions, pgtty, dbName);

	// check to see that the backend connection was successfully made
	if (PQstatus(conn) == $ConnStatusType$CONNECTION-BAD)
        bad-connection(conn, dbName);
	end if;

	let res = PQexec(conn, "LISTEN TBL2");
	if (PQresultStatus(res) ~= $ExecStatusType$PGRES-COMMAND-OK)
		exit-nicely(conn, res, "LISTEN command failed\n");
	end if;

	//
	// should PQclear PGresult whenever it is no longer needed to avoid
	// memory leaks
	//
	PQclear(res);

    format(*standard-error*, "Waiting.");
    force-output(*standard-error*);
    block (escape-while)
        while (#t)
		    // async notification only come back as a result of a query
            // we can send empty queries
            let res = PQexec(conn, " ");
            // format(*standard-error*, "res->status = %s\n",
            //          PQresStatus(PQresultStatus(res)));
            //      force-output(*standard-error*);
            // check for asynchronous returns
            let notify = PQnotifies(conn);
            if (notify)
                format(*standard-error*, "ASYNC NOTIFY of '%s' from backend pid '%d' received\n", notify.relname, notify.backend-pid);
                force-output(*standard-error*);
                escape-while();
            end;
        end while;
    cleanup
        PQclear(res);
    end block;

	// close the connection to the database and cleanup
	PQfinish(conn);

    format(*standard-error*, "Ending!\n");
    force-output(*standard-error*);
end method main;

main(application-name(), application-arguments());

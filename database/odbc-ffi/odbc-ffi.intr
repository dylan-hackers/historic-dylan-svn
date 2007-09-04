module: odbc-ffi-melange
author: Dustin Voss
synopsis: The Melange interface declaration for the ODBC library.

define interface
  #include { "sql.h", "sqlext.h", "iodbcext.h", "sqltypes.h" },
		define: { "STRICT_ODBC_TYPES" },

    // See odbc-ffi-consts.dylan.
		exclude: { "SQL_NULL_HENV", "SQL_NULL_HDBC", "SQL_NULL_HSTMT",
		 					 "SQL_NULL_HDESC", "SQL_NULL_HANDLE" },

    // Melange tries to give these names like <SQL-INTERVAL$SQLINTERVAL>,
    // so name them explicitly.
		rename: { "SQLSMALLINT" => <SQLSMALLINT>,
		          "SQLINTERVAL" => <SQLINTERVAL> },
		
		map: { "SQLCHAR *" => <c-string> },
 		seal-functions: sealed;

	struct "struct tagDATE_STRUCT",
    name-mapper: minimal-name-mapping,
    rename: { "year"    => year-value,
              "month"   => month-value,
              "day"     => day-value };

  struct "struct tagTIME_STRUCT",
    name-mapper: minimal-name-mapping,
    rename: { "hour"    => hour-value,
              "minute"  => minute-value,
              "second"  => second-value };

  struct "struct tagTIMESTAMP_STRUCT",
	  name-mapper: minimal-name-mapping,
	  rename: { "year"    => year-value,
              "month"   => month-value,
              "day"     => day-value,
              "hour"    => hour-value,
              "minute"  => minute-value,
              "second"  => second-value,
              "fraction" => fraction-value };

  struct "struct tagSQL_YEAR_MONTH",
	  name-mapper: minimal-name-mapping,
    rename: { "year"    => year-value,
              "month"   => month-value };

  struct "struct tagSQL_DAY_SECOND",
	  name-mapper: minimal-name-mapping,
    rename: { "day"     => day-value,
              "hour"    => hour-value,
              "minute"  => minute-value,
              "second"  => second-value,
              "fraction" => fraction-value };

  struct "struct tagSQL_INTERVAL_STRUCT",
	  name-mapper: minimal-name-mapping,
	  rename: { "interval_type" => interval-type-value,
	            "interval_sign" => interval-sign-value,
	            "intval"        => intval-value };

  struct "struct tagSQL_NUMERIC_STRUCT",
	  name-mapper: minimal-name-mapping,
    rename: { "precision" => precision-value,
              "scale"     => scale-value,
              "sign"      => sign-value,
              "val"       => val-array };

  struct "struct tagSQLGUID",
	  name-mapper: minimal-name-mapping,
    rename: { "Data1" => data1-value,
              "Data2" => data2-value,
              "Data3" => data3-value,
              "Data4" => data4-array };

	function "SQLAllocConnect", output-argument:2;
	function "SQLAllocEnv", output-argument:1;
	function "SQLAllocHandle", output-argument:3;
	function "SQLAllocStmt", output-argument:2;
	function "SQLColAttribute", output-argument:6;
	function "SQLDataSources", output-argument:5, output-argument:8;
	function "SQLDescribeCol",
	 	output-argument:5, output-argument:6, output-argument:7,
	 	output-argument:8, output-argument:9;
	function "SQLError", output-argument:5, output-argument:8;
	function "SQLGetConnectAttr", output-argument:5;
	function "SQLGetCursorName", output-argument:4;
	function "SQLGetData", output-argument:6;
	function "SQLGetDescField", output-argument:6;
  function "SQLGetDescRec",
		output-argument:5, output-argument:6, output-argument:7,
		output-argument:8, output-argument:9, output-argument:10,
		output-argument:11;
	function "SQLGetDiagField", output-argument:7;
	function "SQLGetDiagRec", output-argument:5, output-argument:8;
	function "SQLGetEnvAttr", output-argument:5;
	function "SQLGetFunctions", output-argument:3;
	function "SQLGetInfo", output-argument:5;
	function "SQLGetStmtAttr", output-argument:5;
	function "SQLNumResultCols", output-argument:2;
	function "SQLRowCount", output-argument:2;
end interface;

Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosPersistenceDS_CLI ()
  check("", test-idl-file, *corba-services-files*, "CosPersistenceDS_CLI");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosPersistenceDS_CLI",
"// CosPersistenceDS_CLI module, p 5-34 CORBAservices,\n"
"// Persistent Object Service V1.0, 3/94\n"
"\n"
"#include \"CosPersistenceDDO.idl\"\n"
"// CosPersistenceDDO.idl #includes CosPersistencePID.idl\n"
"\n"
"module CosPersistenceDS_CLI {\n"
"\tinterface UserEnvironment {\n"
"\t\tvoid set_option (in long option,in any value);\n"
"\t\tvoid get_option (in long option,out any value);\n"
"\t\tvoid release();\n"
"\t};\n"
"\n"
"\tinterface Connection {\n"
"\t\tvoid set_option (in long option,in any value);\n"
"\t\tvoid get_option (in long option,out any value);\n"
"\t};\n"
"\n"
"\tinterface ConnectionFactory {\n"
"\t\tConnection create_object (\n"
"\t\t\tin UserEnvironment user_envir);\n"
"\t};\n"
"\n"
"\tinterface Cursor { \n"
"\t\tvoid set_position (in long position,in any value);\n"
"\t\tCosPersistenceDDO::DDO fetch_object();\n"
"\t};\n"
"\n"
"\tinterface CursorFactory {\n"
"\t\tCursor create_object (\n"
"\t\t\tin Connection connection);\n"
"\t};\n"
"\n"
" \tinterface PID_CLI : CosPersistencePID::PID {\n"
"\t\tattribute string datastore_id;\n"
"\t\tattribute string id;\n"
"\t};\n"
"\n"
"\n"
"\n"
"\tinterface Datastore_CLI {\n"
"\t\tvoid connect (in Connection connection,\n"
"\t\t\tin string datastore_id,\n"
"\t\t\tin string user_name,\n"
"\t\t\tin string authentication);\n"
"\t\tvoid disconnect (in Connection connection);\n"
"\t\tConnection get_connection (\n"
"\t\t\tin string datastore_id,\n"
"\t\t\tin string user_name);\n"
"\t\tvoid add_object (in Connection connection,\n"
"\t\t\tin CosPersistenceDDO::DDO data_obj);\n"
"\t\tvoid delete_object (\n"
"\t\t\tin Connection connection,\n"
"\t\t\tin CosPersistenceDDO::DDO data_obj);\n"
"\t\tvoid update_object (\n"
"\t\t\tin Connection connection,\n"
"\t\t\tin CosPersistenceDDO::DDO data_obj);\n"
"\t\tvoid retrieve_object(\n"
"\t\t\tin Connection connection,\n"
"\t\t\tin CosPersistenceDDO::DDO data_obj);\n"
"\t\tCursor select_object(\n"
"\t\t\tin Connection connection,\n"
"\t\t\tin string key);\n"
"\t\tvoid transact (in UserEnvironment user_envir,\n"
"\t\t\tin short completion_type);\n"
"\t\tvoid assign_PID (in PID_CLI p);\n"
"\t\tvoid assign_PID_relative (\n"
"\t\t\tin PID_CLI source_pid,\n"
"\t\t\tin PID_CLI target_pid);\n"
"\t\tboolean is_identical_PID (\n"
"\t\t\tin PID_CLI pid_1,\n"
"\t\t\tin PID_CLI pid_2);\n"
"\t\tstring get_object_type (in PID_CLI p);\n"
"\t\tvoid register_mapping_schema (in string schema_file); \n"
"\t\tCursor execute (in Connection connection,\n"
"\t\t\tin string command); \n"
"\t};\n"
"\n"
"};\n"
"\n",
setup: method (scepter)
         scepter-case-sensitive-reserved-words?(scepter) := #t;
       end method)

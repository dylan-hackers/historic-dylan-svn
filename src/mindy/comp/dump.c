/**********************************************************************\
*
*  Copyright (C) 1994, Carnegie Mellon University
*  All rights reserved.
*
*  This code was produced by the Gwydion Project at Carnegie Mellon
*  University.  If you are interested in using this code, contact
*  "Scott.Fahlman@cs.cmu.edu" (Internet).
*
***********************************************************************
*
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/dump.c,v 1.9 1994/04/20 00:23:16 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>

#include "mindycomp.h"
#include "src.h"
#include "literal.h"
#include "sym.h"
#include "fileops.h"
#include "compile.h"
#include "dump.h"
#include "version.h"
#include "envanal.h"
#include "lose.h"

static FILE *File = NULL;
static int table_index = 0;

static void dump_literal(struct literal *literal);
static void dump_constant(struct constant *c);
static void dump_constant(struct constant *c);


/* Base output routines */

inline static void dump_byte(unsigned byte)
{
    putc(byte, File);
}

#define dump_op dump_byte
#define dump_int1 dump_byte

inline static void dump_bytes(void *ptr, int bytes)
{
    int count;

    while (bytes > 0) {
	count = fwrite(ptr, 1, bytes, File);
	ptr += count;
	bytes -= count;
    }
}

inline static void dump_int2(short value)
{
    dump_bytes(&value, 2);
}

inline static void dump_int4(int value)
{
    dump_bytes(&value, 4);
}


/* Table manipulation */

static int implicit_store(void)
{
    return table_index++;
}

static int dump_store(void)
{
    dump_op(fop_STORE);
    return table_index++;
}

static void dump_ref(int handle)
{
    if (handle < (1<<16)) {
	dump_op(fop_SHORT_REF);
	dump_int2(handle);
    }
    else {
	dump_op(fop_REF);
	dump_int4(handle);
    }
}


/* Utility dumpers. */

static void dump_string_guts(int short_op, int long_op, char *str, int length)
{
    if (length < 256) {
	dump_op(short_op);
	dump_byte(length);
    }
    else {
	dump_op(long_op);
	dump_int4(length);
    }
    dump_bytes(str, length);
}

static void dump_integer(long value)
{
    if ((-1<<7) <= value && value < (1<<7)) {
	dump_op(fop_SIGNED_8);
	dump_int1(value);
    }
    else if ((-1<<15) <= value && value < (1<<15)) {
	dump_op(fop_SIGNED_16);
	dump_int2(value);
    }
    else {
	dump_op(fop_SIGNED_32);
	dump_int4(value);
    }
}

static void dump_symbol(struct symbol *symbol)
{
    if (symbol->handle != -1)
	dump_ref(symbol->handle);
    else {
	symbol->handle = implicit_store();
	dump_string_guts(fop_SHORT_SYMBOL, fop_SYMBOL, symbol->name,
			 strlen(symbol->name));
    }
}


/* Literal dumping. */

static void dump_symbol_literal(struct symbol_literal *literal)
{
    dump_symbol(literal->symbol);
}

static void dump_integer_literal(struct integer_literal *literal)
{
    dump_integer(literal->value);
}

static void dump_single_float_literal(struct single_float_literal *literal)
{
    dump_op(fop_SINGLE_FLOAT);
    dump_bytes(&literal->value, sizeof(literal->value));
}

static void dump_double_float_literal(struct double_float_literal *literal)
{
    dump_op(fop_DOUBLE_FLOAT);
    dump_bytes(&literal->value, sizeof(literal->value));
}

static void dump_extended_float_literal(struct extended_float_literal *literal)
{
    dump_op(fop_EXTENDED_FLOAT);
    dump_bytes(&literal->value, sizeof(literal->value));
}

static void dump_character_literal(struct character_literal *literal)
{
    dump_op(fop_CHAR);
    dump_byte(literal->value);
}

static void dump_string_literal(struct string_literal *literal)
{
    dump_string_guts(fop_SHORT_STRING, fop_STRING, literal->chars,
		     literal->length);
}

static void dump_list_literal(struct list_literal *literal)
{
    struct literal *part;
    int length;
    int i;

    length = 0;
    for (part = literal->first; part != NULL; part = part->next)
	length++;

    part = literal->first;
    while (length > 255+9) {
	dump_op(fop_DOTTED_LISTN);
	dump_byte(255);
	for (i = 0; i < 255+9; i++) {
	    dump_literal(part);
	    part = part->next;
	}
	length -= 255+9;
    }

    if (literal->tail)
	switch (length) {
	  case 0: lose("Zero element dotted list?\n");
	  case 1: dump_op(fop_DOTTED_LIST1); break;
	  case 2: dump_op(fop_DOTTED_LIST2); break;
	  case 3: dump_op(fop_DOTTED_LIST3); break;
	  case 4: dump_op(fop_DOTTED_LIST4); break;
	  case 5: dump_op(fop_DOTTED_LIST5); break;
	  case 6: dump_op(fop_DOTTED_LIST6); break;
	  case 7: dump_op(fop_DOTTED_LIST7); break;
	  case 8: dump_op(fop_DOTTED_LIST8); break;
	  default:
	    dump_op(fop_DOTTED_LISTN);
	    dump_byte(length - 9);
	    break;
	}
    else
	switch (length) {
	  case 0: dump_op(fop_NIL); break;
	  case 1: dump_op(fop_LIST1); break;
	  case 2: dump_op(fop_LIST2); break;
	  case 3: dump_op(fop_LIST3); break;
	  case 4: dump_op(fop_LIST4); break;
	  case 5: dump_op(fop_LIST5); break;
	  case 6: dump_op(fop_LIST6); break;
	  case 7: dump_op(fop_LIST7); break;
	  case 8: dump_op(fop_LIST8); break;
	  default:
	    dump_op(fop_LISTN);
	    dump_byte(length - 9);
	    break;
	}
    while (part != NULL) {
	dump_literal(part);
	part = part->next;
    }
    if (literal->tail)
	dump_literal(literal->tail);
}

static void dump_vector_header(int length)
{
    switch (length) {
      case 0: dump_op(fop_VECTOR0); break;
      case 1: dump_op(fop_VECTOR1); break;
      case 2: dump_op(fop_VECTOR2); break;
      case 3: dump_op(fop_VECTOR3); break;
      case 4: dump_op(fop_VECTOR4); break;
      case 5: dump_op(fop_VECTOR5); break;
      case 6: dump_op(fop_VECTOR6); break;
      case 7: dump_op(fop_VECTOR7); break;
      case 8: dump_op(fop_VECTOR8); break;
      default:
	dump_op(fop_VECTORN);
	if (length-9 < 254)
	    dump_byte(length-9);
	else if (length-9-254 < (1<<16)) {
	    dump_byte(254);
	    dump_int2(length-9-254);
	}
	else {
	    dump_byte(255);
	    dump_int4(length-9-254-(1<<16));
	}
	break;
    }
}

static void dump_vector_literal(struct vector_literal *literal)
{
    struct literal *part;
    int length;

    length = 0;
    for (part = literal->first; part != NULL; part = part->next)
	length++;

    dump_vector_header(length);

    for (part = literal->first; part != NULL; part = part->next)
	dump_literal(part);
}

static void dump_true_literal(struct literal *literal)
{
    dump_op(fop_TRUE);
}

static void dump_false_literal(struct literal *literal)
{
    dump_op(fop_FALSE);
}

static void dump_unbound_literal(struct literal *literal)
{
    dump_op(fop_UNBOUND);
}

static void (*LiteralDumpers[(int)literal_Kinds])() = {
    dump_symbol_literal, dump_integer_literal,
    dump_single_float_literal, dump_double_float_literal,
    dump_extended_float_literal, dump_character_literal, dump_string_literal,
    dump_list_literal, dump_vector_literal, dump_true_literal,
    dump_false_literal, dump_unbound_literal
};

static void dump_literal(struct literal *literal)
{
    (LiteralDumpers[(int)literal->kind])(literal);
}



/* Debug info dumping. */

static void dump_vars(struct scope_info *scope)
{
    struct var_info *var_info;

    if (scope->handle != -1)
	dump_ref(scope->handle);
    else {
	scope->handle = dump_store();

	if (scope->outer)
	    dump_op(fop_DOTTED_LIST1);
	else
	    dump_op(fop_LIST1);

	dump_vector_header(scope->nvars);
	for (var_info=scope->vars; var_info != NULL; var_info=var_info->next) {
	    int loc_info = var_info->offset << 2;
	    if (var_info->indirect)
		loc_info |= 2;
	    if (var_info->argument)
		loc_info |= 1;

	    dump_op(fop_VECTOR2);
	    dump_symbol(var_info->var->symbol);
	    dump_integer(loc_info);
	}

	if (scope->outer)
	    dump_vars(scope->outer);
    }
}

static void dump_debug_info(struct component *c)
{
    struct debug_info *info;
    
    dump_vector_header(c->ndebug_infos);
    for (info = c->debug_info; info != NULL; info = info->next) {
	dump_op(fop_VECTOR3);
	dump_integer(info->line);
	dump_integer(info->bytes);
	if (info->scope)
	    dump_vars(info->scope);
	else
	    dump_op(fop_NIL);
    }
}


/* Method Dumping */

static void dump_component(struct component *c)
{
    struct constant *constant;
    struct block *block;
    int bytes;

    if (c->nconstants < 256 && c->bytes < (1<<16)) {
	dump_op(fop_SHORT_COMPONENT);
	dump_byte(c->nconstants);
	dump_int2(c->bytes);
    }
    else {
	dump_op(fop_COMPONENT);
	dump_int4(c->nconstants);
	dump_int4(c->bytes);
    }

    if (c->debug_name)
	dump_literal(c->debug_name);
    else
	dump_op(fop_FALSE);

    dump_integer(c->frame_size);

    dump_debug_info(c);

    for (constant = c->constants; constant != NULL; constant = constant->next)
	dump_constant(constant);

    bytes = 0;
    for (block = c->blocks; block != NULL; block = block->next) {
	int count = block->end - block->bytes;
	dump_bytes(block->bytes, count);
	bytes += count;
    }
    if (bytes != c->bytes)
	lose("Planned on writing %d bytes, but ended up writing %d instead.",
	     c->bytes, bytes);
}

static void dump_method(struct method *method)
{
    struct param_list *params = method->params;
    struct keyword_param *k;
    int param_info, nkeys;
    int nclosure_vars;
    struct closes_over *over;

    if (params->rest_param)
	param_info = 1;
    else
	param_info = 0;
    if (params->allow_keys) {
	nkeys = 0;
	for (k = params->keyword_params; k != NULL; k = k->next)
	    nkeys++;
	param_info = param_info | (nkeys+1)<<1;
    }
	
    nclosure_vars = 0;
    for (over = method->closes_over; over != NULL; over = over->next)
	nclosure_vars++;
    
    if (param_info < 256 && nclosure_vars) {
	dump_op(fop_SHORT_METHOD);
	dump_byte(param_info);
	dump_byte(nclosure_vars);
    }
    else {
	dump_op(fop_METHOD);
	dump_int4(param_info);
	dump_int4(nclosure_vars);
    }

    for (k = params->keyword_params; k != NULL; k = k->next) {
	struct literal_expr *def = (struct literal_expr *)k->def;
	dump_symbol(k->keyword);
	if (def) {
	    if (def->kind != expr_LITERAL)
		lose("non-literal keyword default made it though expand?");
	    dump_literal(def->lit);
	}
	else
	    dump_op(fop_FALSE);
    }

    dump_component(method->component);
}

static void dump_varref(struct id *id, boolean written)
{
    if (id->internal)
	if (written)
	    dump_op(fop_BUILTIN_WRITABLE_VALUE_CELL);
	else
	    dump_op(fop_BUILTIN_VALUE_CELL);
    else
	if (written)
	    dump_op(fop_WRITABLE_VALUE_CELL);
	else
	    dump_op(fop_VALUE_CELL);

    dump_symbol(id->symbol);
}

static void dump_constant(struct constant *c)
{
    switch (c->kind) {
      case constant_LITERAL:
	dump_literal(c->u.literal);
	break;
      case constant_METHODDESC:
	dump_method(c->u.method);
	break;
      case constant_VARREF:
	dump_varref(c->u.varref.id, c->u.varref.written);
	break;
    }
}


/* Defconst and Defvar dumping. */

static void dump_defconst_or_var(struct param_list *params)
{
    int count;
    struct param *p;

    count = 0;
    for (p = params->required_params; p != NULL; p = p->next)
	count++;
    if (params->rest_param)
	count++;

    dump_integer(count);
    for (p = params->required_params; p != NULL; p = p->next)
	dump_symbol(p->id->symbol);
    if (params->rest_param)
	dump_symbol(params->rest_param->symbol);
}


/* Namespace (module and library) dumping. */


static void dump_defnamespace(struct defnamespace_constituent *c,
			      boolean dump_creates)
{
    struct use_clause *use;

    dump_literal(c->name);
    for (use = c->use_clauses; use != NULL; use = use->next) {
	dump_literal(use->name);
	dump_literal(use->import);
	dump_literal(use->exclude);
	dump_literal(use->prefix);
	dump_literal(use->rename);
	dump_literal(use->export);
    }
    dump_op(fop_FALSE);
    dump_literal(c->exported_literal);
    if (dump_creates)
	dump_literal(c->created_literal);
}


/* Interface to the output file dumper */

void dump_setup_output(char *source, FILE *file, char *what)
{
    struct stat buf;
    struct timeval tv;

    File = file;

    fprintf(File, "# %s of %s\n", what, source);
    if (stat(source, &buf) >= 0)
	fprintf(File, "# last modified on %s", ctime(&buf.st_mtime));
    fprintf(File, "# produced with the %s version of mindycomp\n", Version);
    gettimeofday(&tv, NULL);
    fprintf(File, "# at %s", ctime(&tv.tv_sec));

    dump_op(fop_BYTE_ORDER);
    dump_int2(1);
    dump_op(fop_IN_LIBRARY);
    if (LibraryName)
	dump_symbol(LibraryName);
    else
	dump_symbol(symbol("Dylan-User"));
    dump_op(fop_IN_MODULE);
    dump_symbol(ModuleName);
    dump_op(fop_SOURCE_FILE);
    dump_string_guts(fop_SHORT_STRING, fop_STRING, source, strlen(source));
}

void dump_top_level_form(struct component *c)
{
    dump_op(fop_TOP_LEVEL_FORM);
    dump_component(c);
}

void dump_defmethod(struct id *name, struct component *c)
{
    dump_op(fop_DEFINE_METHOD);
    dump_symbol(name->symbol);
    dump_component(c);
}

void dump_defgeneric(struct id *name, struct component *tlf)
{
    dump_op(fop_DEFINE_GENERIC);
    dump_symbol(name->symbol);
    dump_component(tlf);
}

void dump_defclass(struct id *name, struct slot_spec *slots,
		   struct component *tlf1, struct component *tlf2)
{
    struct slot_spec *slot;

    dump_op(fop_DEFINE_CLASS);
    dump_symbol(name->symbol);
    for (slot = slots; slot != NULL; slot = slot->next) {
	dump_symbol(slot->getter->symbol);
	if (slot->setter)
	    dump_symbol(slot->setter->symbol);
    }
    dump_op(fop_FALSE);
    dump_component(tlf1);
    dump_component(tlf2);
}

void dump_defconst(struct param_list *params, struct component *initializer)
{
    dump_op(fop_DEFINE_CONSTANT);
    dump_defconst_or_var(params);
    dump_component(initializer);
}

void dump_defvar(struct param_list *params, struct component *initializer)
{
    dump_op(fop_DEFINE_VARIABLE);
    dump_defconst_or_var(params);
    dump_component(initializer);
}

void dump_defmodule(struct defnamespace_constituent *c)
{
    dump_op(fop_DEFINE_MODULE);
    dump_defnamespace(c, TRUE);
}

void dump_deflibrary(struct defnamespace_constituent *c)
{
    dump_op(fop_DEFINE_LIBRARY);
    dump_defnamespace(c, FALSE);
}

void dump_finalize_output(void)
{
    dump_op(fop_DONE);
}



/* Stuff to dump program parses */

static void dump_body(struct body *body);
static void dump_expr(struct expr *expr);

static void dump_id(struct id *id)
{
    dump_symbol(id->symbol);
    dump_op(id->internal ? fop_TRUE : fop_FALSE);
    dump_integer(id->line);
}

static void dump_param_list(struct param_list *params)
{
    struct param *p;
    int nparams = 0;

    for (p = params->required_params; p != NULL; p = p->next)
	nparams++;
    dump_integer(nparams);
    for (p = params->required_params; p != NULL; p = p->next) {
	dump_id(p->id);
	if (p->type)
	    dump_expr(p->type);
	else
	    dump_op(fop_FALSE);
    }

    if (params->next_param)
	dump_id(params->next_param);
    else
	dump_op(fop_FALSE);

    if (params->rest_param)
	dump_id(params->rest_param);
    else
	dump_op(fop_FALSE);

    if (params->allow_keys) {
	struct keyword_param *k;
	int nkeys = 0;

	for (k = params->keyword_params; k != NULL; k = k->next)
	    nkeys++;
	dump_integer(nkeys);

	for (k = params->keyword_params; k != NULL; k = k->next) {
	    dump_symbol(k->keyword);
	    dump_id(k->id);
	    if (k->type)
		dump_expr(k->type);
	    else
		dump_op(fop_FALSE);
	    if (k->def)
		dump_expr(k->def);
	    else
		dump_op(fop_FALSE);
	}
    }
    else
	dump_op(fop_FALSE);
}

static void dump_bindings(struct bindings *bindings)
{
    dump_param_list(bindings->params);
    dump_expr(bindings->expr);
}

static void dump_rettypes(struct return_type_list *rettypes)
{
    struct return_type *r;
    int nreq = 0;

    if (rettypes != NULL) {
	for (r = rettypes->req_types; r != NULL; r = r->next)
	    nreq++;
	dump_integer(nreq);
	for (r = rettypes->req_types; r != NULL; r = r->next)
	    if (r->type)
		dump_expr(r->type);
	    else
		dump_op(fop_FALSE);
	if (rettypes->rest_type)
	    dump_expr(r->type);
	else
	    dump_op(fop_FALSE);
    }
    else
	dump_op(fop_FALSE);
}

static void dump_plist(struct plist *plist)
{
    if (plist) {
	struct property *p;
	int nprops = 0;

	for (p = plist->head; p != NULL; p = p->next)
	    nprops++;
	dump_integer(nprops);
	for (p = plist->head; p != NULL; p = p->next) {
	    dump_symbol(p->keyword);
	    dump_expr(p->expr);
	}
    }
    else
	dump_integer(0);
}

static void dump_method_parse(struct method *method)
{
    if (method->name)
	dump_id(method->name);
    else
	dump_op(fop_FALSE);
    dump_param_list(method->params);
    dump_rettypes(method->rettypes);
    dump_body(method->body);
}

static void dump_varref_expr(struct varref_expr *expr)
{
    dump_op(fop_VARREF_EXPR);
    dump_id(expr->var);
}

static void dump_literal_expr(struct literal_expr *expr)
{
    dump_op(fop_LITERAL_EXPR);
    dump_literal(expr->lit);
}

static void dump_call_expr(struct call_expr *expr)
{
    struct argument *args;
    int nargs = 0;

    dump_op(fop_CALL_EXPR);
    dump_expr(expr->func);
    for (args = expr->args; args != NULL; args = args->next)
	nargs++;
    dump_integer(nargs);
    for (args = expr->args; args != NULL; args = args->next)
	dump_expr(args->expr);
}

static void dump_method_expr(struct method_expr *expr)
{
    dump_op(fop_METHOD_EXPR);
    dump_method_parse(expr->method);
}

static void dump_dot_expr(struct dot_expr *expr)
{
    dump_op(fop_DOT_EXPR);
    dump_expr(expr->arg);
    dump_expr(expr->func);
}

static void dump_body_expr(struct body_expr *expr)
{
    dump_op(fop_BODY_EXPR);
    dump_body(expr->body);
}

static void dump_block_expr(struct block_expr *expr)
{
    dump_op(fop_BLOCK_EXPR);
    if (expr->exit_fun)
	dump_id(expr->exit_fun);
    else
	dump_op(fop_FALSE);
    dump_body(expr->body);
    if (expr->inner)
	lose("Dumping a block that still has exception clauses?");
    if (expr->cleanup)
	dump_body(expr->cleanup);
    else
	dump_op(fop_FALSE);
    if (expr->outer)
	lose("Dumping a block that still has exception clauses?");
}

static void dump_case_expr(struct case_expr *expr)
{
    lose("case made it though expand?");
}

static void dump_if_expr(struct if_expr *expr)
{
    dump_op(fop_IF_EXPR);
    dump_expr(expr->cond);
    dump_body(expr->consequent);
    dump_body(expr->alternate);
}

static void dump_for_expr(struct for_expr *expr)
{
    lose("for made it though expand?");
}

static void dump_select_expr(struct select_expr *expr)
{
    lose("select made it though expand?");
}

static void dump_varset_expr(struct varset_expr *expr)
{
    dump_op(fop_VARSET_EXPR);
    dump_id(expr->var);
    dump_expr(expr->value);
}

static void dump_binop_series_expr(struct binop_series_expr *expr)
{
    lose("binop series made it though expand?");
}

static void dump_loop_expr(struct loop_expr *expr)
{
    dump_op(fop_LOOP_EXPR);
    dump_body(expr->body);
}

static void dump_repeat_expr(struct repeat_expr *expr)
{
    dump_op(fop_REPEAT_EXPR);
}

static void dump_error_expr(struct expr *expr)
{
    lose("Called dump on a parse tree with errors?");
}

static void (*ExpressionDumpers[])() = {
    dump_varref_expr, dump_literal_expr, dump_call_expr,
    dump_method_expr, dump_dot_expr, dump_body_expr, dump_block_expr,
    dump_case_expr, dump_if_expr, dump_for_expr, dump_select_expr,
    dump_varset_expr, dump_binop_series_expr, dump_loop_expr,
    dump_repeat_expr, dump_error_expr
};

static void dump_expr(struct expr *expr)
{
    (*ExpressionDumpers[(int)expr->kind])(expr);
}

static void dump_defconst_constituent(struct defconst_constituent *c)
{
    dump_op(fop_DEFINE_CONSTANT);
    dump_bindings(c->bindings);
}

static void dump_defvar_constituent(struct defvar_constituent *c)
{
    dump_op(fop_DEFINE_VARIABLE);
    dump_bindings(c->bindings);
}

static void dump_defmethod_constituent(struct defmethod_constituent *c)
{
    dump_op(fop_DEFINE_METHOD);
    dump_method_parse(c->method);
}

static void dump_defgeneric_constituent(struct defgeneric_constituent *c)
{
    dump_op(fop_DEFINE_GENERIC);
    dump_id(c->name);
    dump_param_list(c->params);
    dump_rettypes(c->rettypes);
    dump_plist(c->plist);
}

static void dump_defclass_constituent(struct defclass_constituent *c)
{
    struct superclass *super;
    struct slot_spec *slot;
    struct keyword_spec *keyword;
    struct inherited_spec *inherit;
    int n;

    dump_op(fop_DEFINE_CLASS);

    n = 0;
    for (super = c->supers; super != NULL; super = super->next)
	n++;
    dump_integer(n);
    for (super = c->supers; super != NULL; super = super->next)
	dump_expr(super->expr);
    
    n = 0;
    for (slot = c->slots; slot != NULL; slot = slot->next)
	n++;
    dump_integer(n);
    for (slot = c->slots; slot != NULL; slot = slot->next) {
	switch (slot->alloc) {
	  case alloc_INSTANCE:
	    dump_symbol(symbol("instance"));
	    break;
	  case alloc_CLASS:
	    dump_symbol(symbol("class"));
	    break;
	  case alloc_SUBCLASS:
	    dump_symbol(symbol("subclass"));
	    break;
	  case alloc_CONSTANT:
	    dump_symbol(symbol("constant"));
	    break;
	  case alloc_VIRTUAL:
	    dump_symbol(symbol("virtual"));
	    break;
	  default:
	    lose("strange slot allocation");
	}
	if (slot->name)
	    dump_id(slot->name);
	else
	    dump_op(fop_FALSE);
	if (slot->type)
	    dump_expr(slot->type);
	else
	    dump_op(fop_FALSE);
	dump_plist(slot->plist);
    }

    n = 0;
    for (keyword = c->keywords; keyword != NULL; keyword = keyword->next)
	n++;
    dump_integer(n);
    for (keyword = c->keywords; keyword != NULL; keyword = keyword->next) {
	dump_symbol(keyword->keyword);
	dump_plist(keyword->plist);
    }

    n = 0;
    for (inherit = c->inherits; inherit != NULL; inherit = inherit->next)
	n++;
    dump_integer(n);
    for (inherit = c->inherits; inherit != NULL; inherit = inherit->next) {
	dump_id(inherit->name);
	dump_plist(inherit->plist);
    }
}

static void dump_expr_constituent(struct expr_constituent *c)
{
    dump_op(fop_EXPR_CONSTITUENT);
    dump_expr(c->expr);
}

static void dump_local_constituent(struct local_constituent *c)
{
    struct method *m;
    int nlocals = 0;

    dump_op(fop_LOCAL_CONSTITUENT);
    for (m = c->methods; m != NULL; m = m->next_local)
	nlocals++;
    dump_integer(nlocals);
    for (m = c->methods; m != NULL; m = m->next_local)
	dump_method_parse(m);
    dump_body(c->body);
}

static void dump_handler_constituent(struct handler_constituent *c)
{
    dump_op(fop_HANDLER_CONSTITUENT);
    dump_body(c->body);
}

static void dump_let_constituent(struct let_constituent *let)
{
    dump_op(fop_LET_CONSTITUENT);
    dump_bindings(let->bindings);
    dump_body(let->body);
}

static void dump_tlf_constituent(struct tlf_constituent *c)
{
    lose("top-level-form method inserted when parsing only?");
}

static void dump_error_constituent(struct constituent *c)
{
    lose("Called dump on a parse tree with errors?");
}

static void dump_defmodule_constituent(struct defnamespace_constituent *c)
{
    dump_op(fop_DEFINE_MODULE);
    dump_defnamespace(c, TRUE);
}

static void dump_deflibrary_constituent(struct defnamespace_constituent *c)
{
    dump_op(fop_DEFINE_LIBRARY);
    dump_defnamespace(c, FALSE);
}


static void (*DumpConstituents[])() = {
    dump_defconst_constituent, dump_defvar_constituent,
    dump_defmethod_constituent, dump_defgeneric_constituent,
    dump_defclass_constituent, dump_expr_constituent,
    dump_local_constituent, dump_handler_constituent,
    dump_let_constituent, dump_tlf_constituent,
    dump_error_constituent, dump_defmodule_constituent,
    dump_deflibrary_constituent
};

static void dump_body(struct body *body)
{
    struct constituent *c;
    int nconstits = 0;

    for (c = body->head; c != NULL; c = c->next)
	nconstits++;
    dump_integer(nconstits);
    for (c = body->head; c != NULL; c = c->next)
	(*DumpConstituents[(int)c->kind])(c);
}

void dump_program(struct body *body)
{
    dump_body(body);
}


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mapcalc.h"
#include "func_proto.h"

/****************************************************************************/

static expr_list *variables;

/****************************************************************************/

static func_desc *find_func(const char *name)
{
	int i;
	for (i = 0; func_descs[i].name; i++)
	{
		if (strcmp(name, func_descs[i].name) == 0)
			return &func_descs[i];
	}
	return NULL;
}

static expression *find_variable(const char *name)
{
	expr_list *l;
	for (l = variables; l; l = l->next)
		if (strcmp(name, l->exp->data.bind.var) == 0)
			return l->exp;
	return NULL;
}

static expression *allocate(int type, int res_type)
{
	expression *e = malloc(sizeof(expression));
	e->type = type;
	e->res_type = res_type;
	e->buf = NULL;
	return e;
}

/****************************************************************************/

int is_var(const char *name)
{
	return find_variable(name) ? 1 : 0;
}

/****************************************************************************/

void define_variable(expression *e)
{
	variables = list(e, variables);
}

char *composite(const char *name, const char *mapset)
{
	char *buf = malloc(strlen(name) + strlen(mapset) + 2);
	strcpy(buf, name);
	strcat(buf, "@");
	strcat(buf, mapset);
	return buf;
}

expr_list *list(expression *exp, expr_list *next)
{
	expr_list *l = malloc(sizeof(struct expr_list));
	l->exp = exp;
	l->next = next;
	return l;
}

int list_length(expr_list *l)
{
	int n = 0;
	for (; l; l = l->next)
		n++;
	return n;
}

expr_list *single(expression *e1)
{
	return list(e1, NULL);
}

expr_list *pair(expression *e1, expression *e2)
{
	return list(e1, list(e2, NULL));
}

expr_list *triple(expression *e1, expression *e2, expression *e3)
{
	return list(e1, list(e2, list(e3, NULL)));
}

expression *constant_int(int x)
{
	expression *e = allocate(expr_type_constant, CELL_TYPE);
	e->data.con.ival = x;
	return e;
}

expression *constant_float(float x)
{
	expression *e = allocate(expr_type_constant, FCELL_TYPE);
	e->data.con.fval = x;
	return e;
}

expression *constant_double(double x)
{
	expression *e = allocate(expr_type_constant, DCELL_TYPE);
	e->data.con.fval = x;
	return e;
}

expression *variable(const char *name)
{
	expression *var = find_variable(name);
	expression *e;

	if (!var)
	{
		fprintf(stderr, "undefined variable: %s\n", name);
		return NULL;
	}

	e = allocate(expr_type_variable, var->res_type);
	e->data.var.name = name;
	e->data.var.bind = var;
	return e;
}

expression *mapname(const char *name, int mod, int row, int col)
{
	expression *e = allocate(expr_type_map, map_type(name, mod));
	e->data.map.name = name;
	e->data.map.mod = mod;
	e->data.map.row = row;
	e->data.map.col = col;
	return e;
}

expression *binding(const char *var, expression *val)
{
	expression *e = allocate(expr_type_binding, val->res_type);
	e->data.bind.var = var;
	e->data.bind.val = val;
	e->data.bind.fd = -1;
	return e;
}

expression *function(const char *name, expr_list *arglist)
{
	func_desc *d = find_func(name);
	int argc = list_length(arglist);
	expression **args = malloc((argc + 1) * sizeof(expression *));
	int *argt = malloc((argc + 1) * sizeof(int));
	void **argv = malloc((argc + 1) * sizeof(void *));
	expression *e;
	expr_list *l;
	int i, n;

	for (l = arglist, i = 1; l; l = l->next, i++)
		args[i] = l->exp;

	for (i = 1; i <= argc; i++)
		argt[i] = args[i]->res_type;

	switch (d->check_args(argc, argt))
	{
	case 0:
		break;
	case E_ARG_LO:
		fprintf(stderr,
			"Too few arguments (%d) to function %s()\n",
			argc, name);
		return NULL;
	case E_ARG_HI:
		fprintf(stderr,
			"Too many arguments (%d) to function %s()\n",
			argc, name);
		return NULL;
	case E_ARG_TYPE:
		fprintf(stderr,
			"Incorrect argument types to function %s()\n",
			name);
		return NULL;
	default:
		fprintf(stderr,
			"Internal error for function %s()\n",
			name);
		return NULL;
	}

	for (i = 1; i <= argc; i++)
		if (argt[i] != args[i]->res_type)
		{
			if (argt[i] == CELL_TYPE)
				args[i] = to_int(args[i]);
			if (argt[i] == FCELL_TYPE)
				args[i] = to_float(args[i]);
			if (argt[i] == DCELL_TYPE)
				args[i] = to_double(args[i]);
		}

	e = allocate(expr_type_function, argt[0]);
	e->data.func.name = name;
	e->data.func.func = d->func;
	e->data.func.argc = argc;
	e->data.func.args = args;
	e->data.func.argt = argt;
	e->data.func.argv = argv;
	return e;
}

expression *to_int(expression *e1)
{
	expression *e = allocate(expr_type_function, CELL_TYPE);
	expression **args = malloc(2 * sizeof(expression *));
	int *argt = malloc(2 * sizeof(int));
	void **argv = malloc(2 * sizeof(void *));

	argt[0] = CELL_TYPE;

	args[1] = e1;
	argt[1] = e1->res_type;

	e->data.func.name = "int";
	e->data.func.func = f_int;
	e->data.func.argc = 1;
	e->data.func.args = args;
	e->data.func.argt = argt;
	e->data.func.argv = argv;
	return e;
}

expression *to_float(expression *e1)
{
	expression *e = allocate(expr_type_function, FCELL_TYPE);
	expression **args = malloc(2 * sizeof(expression *));
	int *argt = malloc(2 * sizeof(int));
	void **argv = malloc(2 * sizeof(void *));

	argt[0] = FCELL_TYPE;

	args[1] = e1;
	argt[1] = e1->res_type;

	e->data.func.name = "float";
	e->data.func.func = f_float;
	e->data.func.argc = 1;
	e->data.func.args = args;
	e->data.func.argt = argt;
	e->data.func.argv = argv;
	return e;
}

expression *to_double(expression *e1)
{
	expression *e = allocate(expr_type_function, DCELL_TYPE);
	expression **args = malloc(2 * sizeof(expression *));
	int *argt = malloc(2 * sizeof(int));
	void **argv = malloc(2 * sizeof(void *));

	argt[0] = DCELL_TYPE;

	args[1] = e1;
	argt[1] = e1->res_type;

	e->data.func.name = "double";
	e->data.func.func = f_double;
	e->data.func.argc = 1;
	e->data.func.args = args;
	e->data.func.argt = argt;
	e->data.func.argv = argv;
	return e;
}

/****************************************************************************/

void print_constant(FILE *fp, const expression *e)
{
	if (e->res_type == CELL_TYPE)
		fprintf(fp, "%d", e->data.con.ival);
	else
		fprintf(fp, "%f", e->data.con.fval);
}

void print_variable(FILE *fp, const expression *e)
{
	fprintf(fp, "%s", e->data.var.name);
}

void print_map(FILE *fp, const expression *e)
{
	const char *mod;

	switch (e->data.map.mod)
	{
	case 'r':	mod = "r#";	break;
	case 'g':	mod = "g#";	break;
	case 'b':	mod = "b#";	break;
	case '#':	mod = "#";	break;
	case '@':	mod = "@";	break;
	case 'M':	mod = "";	break;
	default:
		fprintf(stderr, "Invalid map modifier: '%c'\n",
			e->data.map.mod);
		break;
	}
	fprintf(fp, "%s%s[%d,%d]",
		mod, e->data.map.name,
		e->data.map.row, e->data.map.col);
}

void print_function(FILE *fp, const expression *e)
{
	int i;

	fprintf(fp, "%s(", e->data.func.name);
	for (i = 1; i <= e->data.func.argc; i++)
	{
		if (i > 1)
			fprintf(fp, ",");
		print_expression(fp, e->data.func.args[i]);
	}
	fprintf(fp, ")");
}

void print_binding(FILE *fp, const expression *e)
{
	fprintf(fp, "%s = (", e->data.bind.var);
	print_expression(fp, e->data.bind.val);
	fprintf(fp, ")");
}

void print_expression(FILE *fp, const expression *e)
{
	switch (e->type)
	{
	case expr_type_constant:	print_constant(fp, e);	break;
	case expr_type_variable:	print_variable(fp, e);	break;
	case expr_type_map:		print_map     (fp, e);	break;
	case expr_type_function:	print_function(fp, e);	break;
	case expr_type_binding:		print_binding (fp, e);	break;
	default:
		fprintf(stderr,
			"print_expression: unknown type: %d\n",
			e->type);
	}
}

/****************************************************************************/


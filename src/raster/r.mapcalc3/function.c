
#include <stdio.h>
#include <string.h>

#include "expression.h"
#include "func_proto.h"

func_desc func_descs[] = {
	{"add",		c_binop,	f_add		},
	{"sub",		c_binop,	f_sub		},
	{"mul",		c_binop,	f_mul		},
	{"div",		c_binop,	f_div		},
	{"mod",		c_binop,	f_mod		},
	{"pow",		c_binop,	f_pow		},

	{"neg",		c_unop,		f_neg		},
	{"abs",		c_unop,		f_abs		},

	{"gt",		c_cmpop,	f_gt		},
	{"ge",		c_cmpop,	f_ge		},
	{"lt",		c_cmpop,	f_lt		},
	{"le",		c_cmpop,	f_le		},
	{"eq",		c_cmpop,	f_eq		},
	{"ne",		c_cmpop,	f_ne		},

	{"and",		c_logop,	f_and		},
	{"or",		c_logop,	f_or		},

	{"not",		c_not,		f_not		},

	{"sqrt",	c_double1,	f_sqrt		},
	{"sin",		c_double1,	f_sin		},
	{"cos",		c_double1,	f_cos		},
	{"tan",		c_double1,	f_tan		},

	{"exp",		c_double12,	f_exp		},
	{"log",		c_double12,	f_log		},
	{"atan",	c_double12,	f_atan		},

	{"int",		c_int,		f_int		},
	{"float",	c_float,	f_float		},
	{"double",	c_double,	f_double	},
	{"round",	c_round,	f_round		},

	{"eval",	c_eval,		f_eval		},
	{"if",		c_if,		f_if		},
	{"isnull",	c_isnull,	f_isnull	},

	{"max",		c_varop,	f_max		},
	{"min",		c_varop,	f_min		},
#if 0
	{"median",	c_median,	f_median	},
	{"mode",	c_mode,		f_mode		},
#endif
	{"rand",	c_binop,	f_rand		},

	{"null",	c_int0,		f_null		},

	{"col",		c_int0,		f_col		},
	{"row",		c_int0,		f_row		},

	{"x",		c_double0,	f_x		},
	{"y",		c_double0,	f_y		},
	{"ewres",	c_double0,	f_ewres		},
	{"nsres",	c_double0,	f_nsres		},
	{NULL}
};

void print_function_names(void)
{
	int i;

	fprintf(stderr, "known functions:");
	for (i = 0; func_descs[i].name; i++)
		fprintf(stderr, "%c%-10s",
			i % 7 ? ' ' : '\n',
			func_descs[i].name);
	fprintf(stderr, "\n");
}


#include "glob.h"
#include "function.h"
polish (exp_fd, result)
    FILE *exp_fd;
    char *result;
{
    char buf[1024];
    char name[300];
    char *mapset;
    FILE *fd, *popen();
    static char *tempfile = NULL ;
    int ok,x;
    int n;
    int row,col,depth;
    double xd;

    expression_stack_depth = 0;
    max_nargs = 0;
    min_row = max_row = 0;
    min_col = max_col = 0;
    min_depth = max_depth = 0;

/* execute polish with output to a tempfile */
/* input from exp_fd (ie, write the expression to polish) */
    if (tempfile == NULL)
	tempfile = G_tempfile();
    unlink (tempfile);

    sprintf (buf, "%s/etc/polish3d > %s", G_gisbase(), tempfile);
    fd = popen (buf, "w");
    if (!fd) return 0;
    while ((n = fgetc (exp_fd)) > 0) 
	fputc ((char)n, fd);
    pclose (fd);
/* open the result file and put it into the expression stack */
    fd = fopen (tempfile, "r");
    if (!fd) return 0;
    ok = 1;
    while (G_getl(buf, sizeof buf, fd))
    {
	switch (buf[0])
	{
	case '=': /* assignment to result map */
		G_strip (buf+1);
		if (G_legal_filename (buf+1) < 0)
		{
		    fprintf (stderr, "%s - illegal name\n", buf+1);
		    ok = 0;
		}
		else
		    strcpy (result, buf+1);
		break;
	case '1': /* unary operator */
		if (buf[1] != '-')
		{
		    ok = 0;
		    fprintf (stderr, "%s - unknown operator\n", buf+1);
		}
		if (ok)
		    ok = push_expression_op (UMINUS);
		break;
	case '2': /* binary operator */
		switch (buf[1])
		{
		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		case '^':
			if (ok)
			    ok = push_expression_op ((int) buf[1]);
			break;
		default:
			ok = 0;
			fprintf (stderr, "%s - unsupported operation\n", buf+1);
			break;
		}
		break;
	case 'L': /* logical operator */
		switch (buf[1])
		{
		case '&':
			if (ok)
			    ok = push_expression_op (AND);
			break;
		case '|':
			if (ok)
			    ok = push_expression_op (OR);
			break;
		default:
			ok = 0;
			fprintf (stderr, "%s - unsupported operation\n", buf+1);
			break;
		}
		break;
	case 'C': /* comparison operator */
		switch (buf[1])
		{
		case '=':
			if (ok)
			    ok = push_expression_op (EQ);
			break;
		case '!':
			if (ok)
			    ok = push_expression_op (NE);
			break;
		case '>':
			if (buf[2] == '=')
			    x = GE;
			else
			    x = GT;
			if (ok)
			    ok = push_expression_op (x);
			break;
		case '<':
			if (buf[2] == '=')
			    x = LE;
			else
			    x = LT;
			if (ok)
			    ok = push_expression_op (x);
			break;
		default:
			ok = 0;
			fprintf (stderr, "%s - unsupported operation\n", buf+1);
			break;
		}
		break;
	case 'I': /* integer */
		if(ok && (ok = (sscanf (buf+1, "%d", &x) == 1)))
		    ok = push_expression_integer (x,0,0,0);
		break;
	case 'M': /* map name */
		if (sscanf(buf+2,"%d %d %d %[^\n]", &row, &col, &depth,  name) != 4)
		{
		    ok = 0;
		    fprintf (stderr, "Internal error parsing mapname\n");
		    break;
		}
                /* keep track of neighborhoods for off window logic */
		if (row < min_row) min_row = row;
		if (row > max_row) max_row = row;
		if (col < min_col) min_col = col;
                if (col > max_col) max_col = col;
                if (depth < min_depth) min_depth = depth;
                if (depth > max_depth) max_depth = depth;

		G_strip (name);

                if ((G_find_grid3 (name,G_mapset())) == NULL)
		{
		    fprintf (stderr, "<%s> - not found\n", name);
		    ok = 0;
		    break;
		}
                n = buf[1];
		x = openmap (name, G_mapset(), &n, row);
		if (x < 0)
		    ok = 0;
		else
		    ok = push_expression_map (x, n, row, col, depth);
		break;

	case 'V': /* assign to temp variable */
		if (ok && (ok = (sscanf(buf+1, "%d", &x) == 1)))
		{
                    create_variable(x);
                    ok = push_expression_set_variable(x);
		}
		break;
	case 'v': /* get temp variable */
		if (ok && (ok = (sscanf(buf+1, "%d", &x) == 1)))
		    ok = push_expression_get_variable(x);
		break;

	case 'D': /* floating point number */
		if (ok && (ok = (sscanf (buf+1,"%lf", &xd) == 1)))
                    ok = push_expression_double (xd,0,0,0);
		break;
	case 'F': /* function call */
		if (ok)
		    ok = (sscanf (buf+1,"%s %d", name, &x) == 2);
		if (!ok)
		    break;

                ok = find_function (name, &n);
		if (!ok)
		{
		    fprintf (stderr, "<%s> - unknown function\n", name);
		    print_function_names();
		}
		else if (!(*function_list[n].nargs)(x,name))
		    ok = 0;
		if(ok)
		    ok = push_expression_function(n,x);
		if (x > max_nargs)
			max_nargs = x;
		break;
	default:
		fprintf (stderr, "you have confused me\n");
		ok = 0;
		break;
	}
    }

    fclose (fd);
    unlink (tempfile);
    if (ok && (max_nargs > 0))
    {
	xargs = (double **) G3d_malloc (max_nargs * sizeof (double *));
    }
    return ok;
}

#include <unistd.h>
#include <string.h>
#include "glob.h"
#include "function.h"
#include "mapcalc.h"

int polish (FILE *exp_fd, char *result)
{
    char buf[1024], buf2[1024];
    char name[300];
    char *mapset,*p,*q;
    FILE *fd;
    static char *tempfile = NULL ;
    int ok,x;
    int i,j,n;
    int row,col;
    double xd;

    expression_stack_depth = 0;
    max_nargs = 0;
    min_row = max_row = 0;
    min_col = max_col = 0;

/* execute polish with output to a tempfile */
/* input from exp_fd (ie, write the expression to polish) */
    if (tempfile == NULL)
	tempfile = G_tempfile();
    unlink (tempfile);

    sprintf (buf, "%s/etc/polish > %s", G_gisbase(), tempfile);
    fd = popen (buf, "w");
    if (!fd) return 0;
    i = 0;
    while ((n = fgetc (exp_fd)) > 0){
	fputc ((char)n, fd);
	if(!strchr(" \t", (char)n))
		buf2[i++] = (char)n;
    }
    pclose (fd);
    buf2[i] = 0;

    cp_cats = 0;
    catmap[0] = 0;
    if((p=strchr(buf2, '='))){
	    p++;
	    q = p;
	    if(strstr(p, "if(") != p){
		    if(*q && *q != '.' && !isdigit(*q)){
			    for(; *p; p++)
				    if(strchr("^#\"'()[]+-*/%><!=&|,", *p))
					    break;
			    if(!*p && *q != '.' && !isdigit(*q)){
				    cp_cats = 1;
				    strcpy(catmap, q);
				    j = strlen(catmap);
				    if(catmap[j-1] == '\n')
				    	catmap[j-1] = 0;
			    }
		    }
	    }else{
		    for(i--; i>=0; i--){
			    if(buf2[i] == ')'){
				    buf2[i] = 0;
				    break;
			    }
		    }

		    i = 0;
		    for(p+=3; *p; p++){
			    if(*p == '(')
				    i++;
			    else
			    if(*p == ')')
				    i--;
			    else
			    if(*p == ',' && !i)
				    break;
		    }

		    p++;
		    if(*p && *p != '.'){
		    	    i = 0;
			    n = 0;
			    q = p;
			    for(; *p; p++){
				    if(strchr("^#\"'()[]+-*/%><!=&|", *p))
					    i++;
				    else
				    if(*p == ','){
					    if(n++ == 0){
						    *p = 0;
						    strcpy(catmap, q);
						    j = strlen(catmap);
				    		    if(catmap[j-1] == '\n')
							    catmap[j-1] = 0;
						    *p = ',';
					    }
					    break;
				    }
			    }
			    n = 0;
			    for(j=0; catmap[j]; j++){
				    if(isdigit(catmap[j]) || catmap[j] == '.')
					    n++;
			    }
			    if(n == strlen(catmap))
				    i++;
			    if(!i)
				    cp_cats = 2;
		    }
	    }
    }

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
		    ok = push_expression_integer (x,0,0);
		break;
	case 'M': /* map name */
		if (sscanf(buf+2,"%d %d %[^\n]", &row, &col, name) != 3)
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

		G_strip (name);
		if (!(mapset = G_find_cell2 (name, "")))
		{
		    fprintf (stderr, "<%s> - not found\n", name);
		    ok = 0;
		    break;
		}
		n = buf[1];
		x = openmap (name, mapset, &n, row);
		if (x < 0)
		    ok = 0;
		else
		    ok = push_expression_map (x, n, row, col);
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
		    ok = push_expression_double (xd,0,0);
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
	xargs = (double **) G_malloc (max_nargs * sizeof (double *));
	iargs = (CELL **) G_malloc (max_nargs * sizeof (CELL *));
    }
    return ok;
}

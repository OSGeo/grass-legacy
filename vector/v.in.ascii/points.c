#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "local_proto.h"

/* Determine if the string is integer, e.g. 123, +123, -123
 * return 1 if integer, 0 otherwise */
int is_int ( char *str ) 
{
    int i = -1;
    while ( str[++i] != '\0' ) {
	if ( i == 0 && (str[i] == '+' || str[i] == '-') ) continue;
	if ( !isdigit(str[i]) ) return 0;
    }
    return 1;
}

/* Determine if the string is double, e.g. 123.456, +123.456, -123.456
 * return 1 if double, 0 otherwise */
int is_double ( char *str ) 
{
    int i = -1, ndots = 0;
    while ( str[++i] != '\0' ) {
	if ( i == 0 && (str[i] == '+' || str[i] == '-') ) continue;
	if ( str[i] == '.' ) {
	    if ( ndots > 0 ) return 0; /* > 1 dot */
	    ndots++;
	    continue;
	}
	if ( !isdigit(str[i]) ) return 0;
    }
    return 1;
}

/* Analyse points ascii file. Determine number of columns and column types.
 * ascii_tmp: write copy of tempfile to ascii_tmp:
 * rowlength: maximum row length
 * ncolumns: number of columns
 * minncolumns: minimum number of columns
 * column_type: column types
 * column_length: column lengths (string only)
 */

int points_analyse ( FILE *ascii_in, FILE *ascii, char *fs, int head_type, 
	             int *rowlength, int *ncolumns, int *minncolumns, 
		     int **column_type, int **column_length )
{
    int i;
    int  buflen; /* buffer length */
    char *buf;   /* buffer */
    int row = 1;    /* line number, first is 1 */
    int ncols = 0; /* number of columns */
    int minncols = -1;
    int *coltype = NULL; /* column types */
    int *collen = NULL; /* column lengths */
    char **tokens;
    int ntokens;   /* number of tokens */
    int rowlen = 0; /* maximum row length */

    buflen = 1000;
    buf = (char *) G_malloc ( buflen );

    while (1) {
	int len;
	char tmpbuf[500]; 
	
	/* read next line to the memory */
	buf[0] = '\0';
	while (1) {
	    if ( fgets(tmpbuf,500,ascii_in) == NULL ) { /* end of file */

		/* test for Mac OS9 (\r) newline */
		if(tmpbuf[strlen(tmpbuf)-1] == '\r')  /* G_fatal_error() ?? */
		    G_warning ("Mac OS9 text format not supported (incorrect newline)");

		/* test for DOS (\r\n) newline */
		if( (tmpbuf[strlen(tmpbuf)-1] == '\n') && (tmpbuf[strlen(tmpbuf)-2] == '\r') )
		    G_warning ("DOS text format found, attempting import anyway");

		free (buf);
		buf = NULL;
		break;
	    }
	    /* Append tmpbuf to buf */
	    if ( (strlen(buf)+strlen(tmpbuf)+2) > buflen ) {
		buflen += 1000;
		buf = (char *) G_realloc ( buf, buflen );
	    }
	    strcpy ( strchr(buf,'\0'), tmpbuf);

	    /* Check if whole line was read */
	    if ( buf[strlen(buf)-1] == '\n' ) break; /* Yes, whole line was read */
	}
	if ( !buf ) break; /* end of file */
	/* header on the first line */
	if ( row == 1 && ( head_type == PNT_HEAD_NAMES || head_type == PNT_HEAD_NAMES ) ) continue; 
      
	fprintf ( ascii, buf );
	len = strlen ( buf );
	if (len > rowlen ) rowlen = len;
	
	/* remove '\n' (for G_tokenize) */
	buf[strlen(buf)-1] = '\0';

	/* clean DOS (\r\n) newline */
	if(buf[strlen(buf)-1] == '\r') {
	    buf[strlen(buf)-1] = '\0';
	    G_debug (4, "row %d : removed trailing CR", row);
	}

	G_debug (4, "row %d : %s", row, buf);

	tokens = G_tokenize (buf, fs);
	ntokens = G_number_of_tokens ( tokens );
	if ( ntokens > ncols ) { 
	    int c;
	    coltype = (int *) G_realloc ( coltype, ntokens * sizeof(int) );
	    collen = (int *) G_realloc ( collen, ntokens * sizeof(int) ); 
	    for ( c = ncols; c < ntokens; c++ ) {
		coltype[c] = DB_C_TYPE_INT; /* default type */
		collen[c] = 0;
	    }
	    ncols = ntokens;
	}

	if ( minncols == -1 || minncols > ntokens ) minncols = ntokens; 

	/* Determine column types */
	for ( i = 0; i < ntokens; i++ ) {
	    G_debug (4, "row %d col %d: '%s' is_int = %d is_double = %d", 
		         row, i, tokens[i], is_int(tokens[i]), is_double(tokens[i]) );
	    if ( is_int(tokens[i]) ) continue; /* integer */
	    if ( is_double(tokens[i]) ) { /* double */
		if ( coltype[i] == DB_C_TYPE_INT ) {
		     coltype[i] = DB_C_TYPE_DOUBLE;
		}
	       	continue;
	    }
            /* string */
	    coltype[i] = DB_C_TYPE_STRING;
	    len = strlen (tokens[i]);
	    if ( len > collen[i] ) collen[i] = len;
	}
	
	G_free_tokens(tokens);
	row++;
    }

    *rowlength = rowlen;
    *ncolumns = ncols;
    *minncolumns = minncols;
    *column_type = coltype;
    *column_length = collen;
	
    return 0;
}


/* Import points from ascii file.
 *
 * fs: field separator
 * head_type: header info on first line:
 *       0 - no header
 *       1 - column names 
 *       2 - column names + types
 * xcol, ycol, zcol, catcol: x,y,z,cat column in input file, first column is 1, 
 *                            zcol and catcol may be 0 (do not use)
 * rowlen: maximum row length
 * Note: column types (both in header or coldef) must be supported by driver
 */
int points_to_bin( FILE *ascii, int rowlen, struct Map_info *Map, dbDriver *driver, char *table, 
	           char *fs, int head_type,
	           int ncols, int *coltype,
	           int xcol, int ycol, int zcol, int catcol )
{
    char   *buf, buf2[1000];  
    int cat = 0;
    struct line_pnts *Points;
    struct line_cats *Cats;	
    dbString sql, val;

    rewind(ascii);
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();	

    buf = (char *) G_malloc ( rowlen + 1 );
    db_init_string (&sql);
    db_init_string (&val);

    while ( fgets(buf,rowlen+1,ascii) != NULL ) {
	int i, len;
	double x, y, z;
	char **tokens;
	int ntokens;   /* number of tokens */

	len = strlen(buf);
	if ( len == 0 ) continue; /* should not happen */
	
	/* remove '\n' (for G_tokenize) */
	buf[len-1] = '\0';

	/* clean DOS (\r\n) newline */
	if(buf[len-2] == '\r') {
	    buf[len-2] = '\0';
	    G_debug (4, "removed trailing CR");
	}

	G_debug (4, "row: %s", buf );
		
	tokens = G_tokenize (buf, fs);
	ntokens = G_number_of_tokens ( tokens );

	x = atof ( tokens[xcol] );
	y = atof ( tokens[ycol] );

	if ( zcol >= 0 ) z = atof ( tokens[zcol] );
	else z = 0.0;

	if ( catcol >= 0 ) cat = atof ( tokens[catcol] );
	else cat++; 

	Vect_reset_line ( Points );
	Vect_reset_cats ( Cats ); 

	Vect_append_point ( Points, x, y, z);
	Vect_cat_set ( Cats, 1, cat );
	
	Vect_write_line ( Map, GV_POINT, Points, Cats );

	/* Attributes */
	if ( driver ) {
	    sprintf ( buf2, "insert into %s values ( ", table);
	    db_set_string (&sql, buf2 );

	    if ( catcol < 0 ) {
		sprintf ( buf2, "%d, ", cat );
		db_append_string (&sql, buf2 );
	    }

	    for ( i = 0; i < ntokens; i++ ) {
		if ( i > 0 ) db_append_string (&sql, ", " );

		if ( coltype[i] == DB_C_TYPE_INT || coltype[i] == DB_C_TYPE_DOUBLE ) {
		    sprintf ( buf2, "%s", tokens[i] );
		} else { 
		    db_set_string (&val, tokens[i]);
		    db_double_quote_string ( &val );
		    sprintf ( buf2, "'%s'", db_get_string ( &val ) );
		}
		db_append_string (&sql, buf2 );
	    }
	    db_append_string (&sql, ")" );
	    G_debug ( 3, db_get_string ( &sql ) );

	    if (db_execute_immediate (driver, &sql) != DB_OK ) {
		G_fatal_error ( "Cannot insert values: %s", db_get_string ( &sql )  );
	    }
	}

	G_free_tokens(tokens);
    }
    return 0;	
}


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "shapefil.h"
#include "glocale.h"
#include "pgdump.h"

/******************************************************************/
 /*02/2000 dbf dump to Postgres
    *   Alex Shevlakov sixote@yahoo.com
    ***************************************************************** */
/*
 * 10/2003 Thierry Laronde tlaronde@polynum.org
 *	- WARNING the default delimiter is now a tabulation, not a comma!
 *	But this make only sense for PG super-user since this is only used
 *	for COPY FROM command;
 *	- added option "delimiter" to allow the explicit definition of 
 *	delimiter (character) in replacement of the default '\t' ;
 * 	- added option "null" (null_string) to allow the explicit definition
 *	of a string that PG should consider the expression of a NULL value;
 *	- change no_rattle to normal_user to let the variable match the name
 *	found in PgDumpFromDBF and... replace the "special" assignement
 *	- misc name changes and simplifications
 *	- added comments
 */

#define ERR_OPTION	1

int main(int argc, char *argv[])
{
    int normal_user;
    char *infile;
	char delim;	/* Character to use as the DELIMITER */
	short length;	/* Used when parsing the DELIMITER string */

	struct Option *input, *dumpmode, *delimiter, *null_string;

    /* Are we running in Grass environment ? */

    G_gisinit(argv[0]);

    /* define the different options */

	/* Get the dbf input file name */
    input = G_define_option();
    input->key = "input";
    input->type = TYPE_STRING;
    input->required = YES;
    input->description = _("Name of DBF file to be imported");

	/* The method to insert new values will change whether you are 
	 * PG super-user or not  */
    dumpmode = G_define_option();
    dumpmode->key = "dumpmode";
    dumpmode->type = TYPE_STRING;
    dumpmode->required = NO;
    dumpmode->description =
	_("Admin/normal user dump mode (Default = Postgres super-user)");
    dumpmode->options = "admin,normal";
    dumpmode->answer     = "admin";

	/* If the fields one imports have the default delimiter embedded
	 * in them, using this delimiter is doomed to make PG abort. So
	 * allow the change.
	 */
	delimiter = G_define_option();
	delimiter->key = "delimiter";
	delimiter->type = TYPE_STRING;
	delimiter->required = NO;
	delimiter->description =
		_("Delimiter (character) to use");
	delimiter->answer = "\\t";
	
	/* 
	 * Allow the specification of a NULL string
	 */
	null_string = G_define_option();
	null_string->key = "null";
	null_string->type = TYPE_STRING;
	null_string->required = NO;
	null_string->description =
		_("String to use as NULL");
	null_string->answer = "";
    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);

    infile = input->answer;

	/* Transform the delimiter string in a single char */
	length = strlen(delimiter->answer);
	if (length == 1)
		delim = delimiter->answer[0];
	else if (length == 2 && delimiter->answer[0] == '\\') 
		switch (delimiter->answer[1]) {
		case 'a': delim = '\a'; break;
		case 'b': delim = '\b'; break;
		case 'f': delim = '\f'; break;
		case 'n':
			fprintf(stderr, _("Since we are feeding PG with lines, \\n is not a great idea my friend!\n"));
			exit(ERR_OPTION);
		case 'r': delim = '\r'; break;
		case 't': delim = '\t'; break;
		case 'v': delim = '\v'; break;
		default:
		fprintf(stderr,_("Invalid escaped sequence given for the delimiter!\n"));
		exit(ERR_OPTION);
	} else {
		fprintf(stderr, 
		  _("The delimiter must be a character (escaped sequences are OK)!\n"));
		exit(ERR_OPTION);
	}

	/* To be or not to be (a super-user...) */
	if (!strcmp(dumpmode->answer, "admin"))
		normal_user = 0; /* Not human, superman! */
	else 
		normal_user = 1; /* sigh... Yes, just an average one */

    PgDumpFromDBF(infile, normal_user, delim, null_string->answer);

    exit(0);
}

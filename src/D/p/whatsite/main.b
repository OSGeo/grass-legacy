#include <stdio.h>

#define MAX_FIELDS 100
#define PGM argv[0]
#define FORM argv[1]
#define DATA argv[2]

main(argc,argv)	char *argv[];
{
	FILE *fd;
	FILE *fd2;
	char string[128];
	char buf[256];
	char *bufptr ;
	struct field
	{
		int row;
		int col;
		int len;
	} field[MAX_FIELDS];
	int col ;
	int row ;
	int num ;
	int fields ;
	int fields_read ;
	int nfields;
	int f;
	int send;
	int n_fields ;
	char *list[128] ;
	int i ;
	int err ;
	char *record[20] ;
	int table ;

/* must have 3 args */
	if (argc != 3)
	{
		usage (PGM);
		exit(1);
	}

/* flag if shell set command file present */

	Qopen();
	Qforms(0);

	if ( ! ( fd = fopen (FORM, "r"))) 
	{
		Qclose();
		perror (FORM);
		usage (PGM);
		exit(1);
	}

	get_field_list(".", DATA, list, &n_fields) ;

	f = 0 ;
	for (; fgets(buf, sizeof buf, fd);)
	{
		if (*buf == 't')        /* have a text record */
		{
			sscanf(buf,"%*s %d %d %d", &row, &col, &fields) ;
			if (fields > n_fields)
			{
				printf("Field referenced in format file that doesn't exist in data\n") ;
				exit(-1) ;
			}
			Qtext (row, col, list[fields-1]) ;
		}
		else if (*buf == 'f')   /* have a field record */
		{
			sscanf(buf,"%*s %d %d %d %d", 
				&field[f].row,
				&field[f].col,
				&field[f].len,
				&num ) ;
			if (++f != num)
			{
				Qclose ();
				fprintf(stderr,
					"Error in format file.  Fields not numbered consecutively.\n") ;
				exit(1) ;
			}
		}
	}

	fclose (fd);

	nfields = f;
	if (nfields <= 0) 
	{
		Qclose ();
		fprintf(stderr,"%s: no fields\n", FORM);
		usage (PGM);
		exit(1);
	}

	if ((table = open_table(".", DATA, 0)) < 0)
	{
		Qclose ();
		fprintf(stderr,"%s: ndata file not available\n", DATA);
		usage (PGM);
		exit(1);
	}
	while(0 < get_table_entry(table, record))
	{
	/* Read the stuff */
		for(f=0; f<n_fields; f++)
		{
			Qsetfield(field[f].row, field[f].col, field[f].len);
			/* strip (buf); */
			Qput (record[f]);
		}
		Qflush() ;
/*		system("sleep 2") ;  */
	}

	fclose (fd);
	
	Qtext (-1, 0, "    Hit ESC when form is complete");

	Qsend("\r\013\033");	/* RETURN, ctrl-k, ESC */
	f = 0;
	send = 0;
	while(send != 033) 
	{
/*
		if (send == '\r') 
		{
			f++;
			if (f >= nfields)
				f = 0;
		}
		else if (send == 013) 
		{
			f--;
			if (f < 0)
				f = nfields - 1;
		}
		Qsetfield(field[f].row, field[f].col, field[f].len);
*/
		send = Qask (buf);
		strip(buf);
		Qput(buf);
	}

/*
	if (data && !(fd = fopen (DATA, "w"))) 
	{
		Qclose();
		perror (DATA);
		usage (PGM);
		exit(1);
	}

	if (set)
	{
		if (!(fd2 = fopen (SET, "w"))) 
		{
			Qclose();
			perror (SET);
			usage (PGM);
			exit(1);
		}
		csh = strcmp (getenv("SHELL"),"/bin/csh") == 0;
	}

	for (f = 0; f < nfields; f++) 
	{
		Qsetfield(field[f].row, field[f].col, field[f].len);
		Qget (buf);
		if (data)
			fprintf(fd,"%s\n", buf);
		if (set)
		{
			if (csh)
				fprintf(fd2,"set ");
			fprintf (fd2,"field%d='%s'\n",f+1, buf);
		}
	}
	if (data)
		fclose (fd);
	if (set)
		fclose (fd2);
*/
	Qtext(-1,0,"\n");
	Qclose();
	exit(0);
}

/*
**  Modified by Dave Gerdes  9/1988    portable
**  US Army Construction Engineering Research Lab
*/
#include "dig_head.h"
#include <stdio.h>


/* routines to read and write DIGIT header */

dig_write_head_binary(digit, head)
	FILE *digit ;
	struct head *head;
{
	long   ltmp;
	double dtmp;

	fseek(digit, 0L, 0) ;

	fwrite(head->organization, sizeof(head->organization), 1,digit) ;
	fwrite(head->date,         sizeof(head->date),         1,digit) ;
	fwrite(head->your_name,    sizeof(head->your_name),    1,digit) ;
	fwrite(head->map_name,     sizeof(head->map_name),     1,digit) ;
	fwrite(head->source_date,  sizeof(head->source_date),  1,digit) ;
	fwrite(head->line_3,       sizeof(head->line_3),       1,digit) ;
	    dig__long_convert (&head->orig_scale, &ltmp, 1);
	fwrite(&ltmp,  		   sizeof(head->orig_scale),   1,digit) ;
	    dig__int_convert (&head->plani_zone, &ltmp, 1);
	fwrite(&ltmp,              sizeof(long),   1,digit) ;
	    dig__double_convert (&head->W, &dtmp, 1);
	fwrite(&dtmp,              sizeof(head->W),            1,digit) ;
	    dig__double_convert (&head->E, &dtmp, 1);
	fwrite(&dtmp,              sizeof(head->E),            1,digit) ;
	    dig__double_convert (&head->S, &dtmp, 1);
	fwrite(&dtmp,              sizeof(head->S),            1,digit) ;
	    dig__double_convert (&head->N, &dtmp, 1);
	fwrite(&dtmp,              sizeof(head->N),            1,digit) ;
	    dig__double_convert (&head->map_thresh, &dtmp, 1);
	fwrite(&dtmp,              sizeof(head->map_thresh),   1,digit) ;

	return(0) ;
}

dig_read_head_binary(digit, head)
	FILE *digit ;
	struct head *head;
{
	long ltmp;

	fseek(digit, 0L, 0) ;

	fread(head->organization, sizeof(head->organization), 1,digit) ;
	fread(head->date,         sizeof(head->date),         1,digit) ;
	fread(head->your_name,    sizeof(head->your_name),    1,digit) ;
	fread(head->map_name,     sizeof(head->map_name),     1,digit) ;
	fread(head->source_date,  sizeof(head->source_date),  1,digit) ;
	fread(head->line_3,       sizeof(head->line_3),       1,digit) ;
	fread(&head->orig_scale,  sizeof(head->orig_scale),   1,digit) ;
	    dig__long_convert (&head->orig_scale, &head->orig_scale, 1);
	fread(&ltmp,  sizeof(long),   1,digit) ;
	    dig__long_convert (&ltmp, &ltmp, 1);
	    head->plani_zone = ltmp;
	fread(&head->W,           sizeof(head->W),            1,digit) ;
	    dig__double_convert (&head->W, &head->W, 1);
	fread(&head->E,           sizeof(head->E),            1,digit) ;
	    dig__double_convert (&head->E, &head->E, 1);
	fread(&head->S,           sizeof(head->S),            1,digit) ;
	    dig__double_convert (&head->S, &head->S, 1);
	fread(&head->N,           sizeof(head->N),            1,digit) ;
	    dig__double_convert (&head->N, &head->N, 1);
	fread(&head->map_thresh,  sizeof(head->map_thresh),   1,digit) ;
	    dig__double_convert (&head->map_thresh, &head->map_thresh, 1);

	return(0) ;
}

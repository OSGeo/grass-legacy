/*
**
**   This file is one of two that can be loaded.  The flag CERL_PORTABLE
**   should *NOT* be defined unless you know what you are doing
**   See the file README_386 in this directory for more information.
*/
#ifdef CERL_PORTABLE
#include "./xrw_binary.c"
#else


/*  @(#)rw_binary.c	2.1  6/26/87  */
#include "dig_head.h"
#include <stdio.h>


/* routines to read and write DIGIT header */

dig_write_head_binary(digit, head)
	FILE *digit ;
	struct head *head;
{
	fseek(digit, 0L, 0) ;

	fwrite(head->organization, sizeof(head->organization), 1,digit) ;
	fwrite(head->date,         sizeof(head->date),         1,digit) ;
	fwrite(head->your_name,    sizeof(head->your_name),    1,digit) ;
	fwrite(head->map_name,     sizeof(head->map_name),     1,digit) ;
	fwrite(head->source_date,  sizeof(head->source_date),  1,digit) ;
	fwrite(head->line_3,       sizeof(head->line_3),       1,digit) ;
	fwrite(&head->orig_scale,  sizeof(head->orig_scale),   1,digit) ;
	fwrite(&head->plani_zone,  sizeof(head->plani_zone),   1,digit) ;
	fwrite(&head->W,           sizeof(head->W),            1,digit) ;
	fwrite(&head->E,           sizeof(head->E),            1,digit) ;
	fwrite(&head->S,           sizeof(head->S),            1,digit) ;
	fwrite(&head->N,           sizeof(head->N),            1,digit) ;
	fwrite(&head->map_thresh,  sizeof(head->map_thresh),   1,digit) ;

	return(0) ;
}

dig_read_head_binary(digit, head)
	FILE *digit ;
	struct head *head;
{
	fseek(digit, 0L, 0) ;

	fread(head->organization, sizeof(head->organization), 1,digit) ;
	fread(head->date,         sizeof(head->date),         1,digit) ;
	fread(head->your_name,    sizeof(head->your_name),    1,digit) ;
	fread(head->map_name,     sizeof(head->map_name),     1,digit) ;
	fread(head->source_date,  sizeof(head->source_date),  1,digit) ;
	fread(head->line_3,       sizeof(head->line_3),       1,digit) ;
	fread(&head->orig_scale,  sizeof(head->orig_scale),   1,digit) ;
	fread(&head->plani_zone,  sizeof(head->plani_zone),   1,digit) ;
	fread(&head->W,           sizeof(head->W),            1,digit) ;
	fread(&head->E,           sizeof(head->E),            1,digit) ;
	fread(&head->S,           sizeof(head->S),            1,digit) ;
	fread(&head->N,           sizeof(head->N),            1,digit) ;
	fread(&head->map_thresh,  sizeof(head->map_thresh),   1,digit) ;

	return(0) ;
}

#endif  /* !CERL_PORTABLE */

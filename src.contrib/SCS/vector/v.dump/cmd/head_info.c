/*  @(#)head_info.c	2.1  6/26/87  */
#include <stdio.h>
#include "vask.h"
#include "Vect.h"

int get_head_info (struct dig_head *dhead)
{

/*	if( ! have_old)
		strcpy(head.organization, "USDA, Soil Con. Service") ;*/

	V_clear() ;
	V_line(1,"Provide the following information:") ;

	V_line(3,"Your organization") ;
	V_line(4,"Todays date (mon,yr)") ;
	V_line(5,"Your name") ;
	V_line(6,"Map's name") ;
	V_line(7,"Map's date") ;
	V_line(8,"Map's scale         1:") ;
	V_line(9,"Other info") ;
	V_line(10,"Zone") ;
	V_line(11,"West edge of area") ;
	V_line(12,"South edge of area") ;
	V_line(13,"East edge of area") ;
	V_line(14,"North edge of area") ;
	V_line(15,"North edge of area") ;

	V_ques( dhead->organization, 's', 3,  20, 30-1) ;
	V_ques( dhead->date,         's', 4,  20, 20-1) ;
	V_ques( dhead->your_name,    's', 5,  20, 20-1) ;
	V_ques( dhead->map_name,     's', 6,  20, 41-1) ;
	V_ques( dhead->source_date,  's', 7,  20, 11-1) ;
	V_ques( &dhead->orig_scale,  'i', 8,  22, 9) ;
	V_ques( dhead->line_3,       's', 9,  20, 59-1) ;
	V_ques( &dhead->plani_zone,  'i', 10, 20, 5)  ;

	V_ques( &dhead->W,           'd', 11, 20, 14) ;
	V_ques( &dhead->S,           'd', 12, 20, 14) ;
	V_ques( &dhead->E,           'd', 13, 20, 14) ;
	V_ques( &dhead->N,           'd', 14, 20, 14) ;
	V_ques( &dhead->map_thresh,  'd', 15, 20, 14) ;
	

	V_call() ;

	/*
	endwin ();
	*/

	return 0;
}

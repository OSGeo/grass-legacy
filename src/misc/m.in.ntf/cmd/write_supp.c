/*****************************************************************************/
/***                                                                       ***/
/***                              write_supp()                             ***/
/***    Writes out supplimentary information for raster and vector files.  ***/
/***                 Jo Wood, Project ASSIST, 31st May 1993.               ***/
/***                                                                       ***/
/*****************************************************************************/

#include "ntf_in.h"

write_supp()
{

}


/*****************************************************************************/
/***                                get_date()                             ***/
/***          Converts numerical date into 10 character text date          ***/
/***                 Jo Wood, Project ASSIST, 31st May 1993.               ***/
/***                                                                       ***/
/*****************************************************************************/

get_date(date_num)
    char	*date_num;		/* Text string holding numerical     */
					/* version of date: YYYYMMDD	     */
{

     
     char	date_txt[11],
		pair[3];		/* Pair of digits YY or MM or DD     */


     strcpy(date_txt,"          ");
     strcpy(pair,"  ");


    /* Check for NULL strings */

    if ( (strcmp(date_num,"00000000")==0) || (strcmp(date_num,"        ")==0) )
    {
	strcpy(date_num,"Not Given");
	return(0);
    }


    /* Get Day */

    strncpy(date_txt,date_num+6,2);
    if (*(date_num+7) == '1')
	strncpy(date_txt+2,"st",2);
    else
    	if (*(date_num+7) == '2')
	    strncpy(date_txt+2,"nd",2);
    	else
    	    if (*(date_num+7) == '3')
	    	strncpy(date_txt+2,"rd",2);
   	    else
	    	strncpy(date_txt+2,"th",2);


    /* Get Month */

    if (strncmp(date_num+4,"01",2) == 0 )
	strncpy(date_txt+4," Jan",4);
    else
    	if (strncmp(date_num+4,"02",2) == 0 )
	    strncpy(date_txt+4," Feb",4);
    	else
   	    if (strncmp(date_num+4,"03",2) == 0 )
	    	strncpy(date_txt+4," Mar",4);
    	    else
   	    	if (strncmp(date_num+4,"04",2) == 0 )
	    	    strncpy(date_txt+4," Apr",4);
    	    	else
   	    	    if (strncmp(date_num+4,"05",2) == 0 )
	    	    	strncpy(date_txt+4," May",4);
    	    	    else
   	    	    	if (strncmp(date_num+4,"06",2) == 0 )
	    	    	    strncpy(date_txt+4," Jun",4);
    	    	    	else
   	    	    	    if (strncmp(date_num+4,"07",2) == 0 )
	    	    	    	strncpy(date_txt+4," Jul",4);
    	    	    	    else
   	    	    	    	if (strncmp(date_num+4,"08",2) == 0 )
	    	    	    	    strncpy(date_txt+4," Aug",4);
    	    	    	    	else
  	    	    	    	    if (strncmp(date_num+4,"09",2) == 0 )
	    	    	    	    	strncpy(date_txt+4," Sep",4);
    	    	    	    	    else
  	    	    	    	    	if (strncmp(date_num+4,"10",2) == 0 )
	    	    	    	    	    strncpy(date_txt+4," Oct",4);
    	    	    	    	    	else
   	    	    	    	    	    if (strncmp(date_num+4,"11",2) == 0 )
	    	    	    	    	    	strncpy(date_txt+4," Nov",4);
    	    	    	    	    	    else
   	    	    	    	    	    	if (strncmp(date_num+4,"12",2) == 0 )
	    	    	    	    	    	    strncpy(date_txt+4," Dec",4);
    /* Get Year */
    strncpy(date_txt+8,date_num+2,2);

    strcpy(date_num,date_txt);
}







    




    








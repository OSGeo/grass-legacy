/***********************************************************************************/
/***                                 old2new()					 ***/
/***  		    Converts NTF Version 1.1 to NTF Version 2.0	 	 	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 3rd June 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

old2new()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    char	newfile[128],		/* Name of new NTF file to create.		*/
		rec_desc,		/* Record Descriptor.				*/
		end_of_file=FALSE,	/* Flag indicating end of file.			*/
		vol_num = 1,		/* Counts through multiple volume numbers.	*/
		fixed_length = FALSE;	/* Flag indicating fixed/variable length files.	*/

    /*----------------------------------------------------------------------------------*/
    /*                        GET NAME OF NEW NTF FILE TO CREATE			*/
    /*----------------------------------------------------------------------------------*/

    fprintf(stderr,"WARNING: Volume header indicates old NTF version.\n");

    do
    {
	fprintf(stderr,"Would you like to create a new NTF Version 2.00 file ?\n> ");
    }
    while ( G_gets(newfile) == 0);


    if(	(strcmp(newfile,"YES") !=0) &&	(strcmp(newfile,"Yes") !=0) &&
	(strcmp(newfile,"yes") !=0) &&	(strcmp(newfile,"Y")   !=0) &&
	(strcmp(newfile,"y")   !=0) )
    	exit(0);


    /* Prompt user for new file name. */

    do
    {
	fprintf(stderr,"Type in the name of the new NTF file to be created:\n> ");
    }
    while ( G_gets(newfile) == 0);


    /* Open new file */

    if ( (new_fptr=fopen(newfile,"r")) != NULL )
    {
	fprintf(stderr,"ERROR: File <%s> already exists. Please try another\n",newfile);
	fclose(new_fptr);
	exit(-1);
    }

    if ( (new_fptr=fopen(newfile,"w")) == NULL )
    {
	fprintf(stderr,"ERROR: Could not open output file <%s>\n",newfile);
	exit(-1);
    }


    /*----------------------------------------------------------------------------------*/
    /*                         CONVERT THE VOLUME HEADER RECORD				*/
    /*----------------------------------------------------------------------------------*/

    strncpy(text+57,"0200",4);		/* Change Version number to 2.00		*/

    if (*(text+61) == 'F')		/* Change from fixed to variable length records	*/
    {
	*(text+61) = 'V';
	fixed_length = TRUE;

	while(text[strlen(text) - 2] == ' ')		/* Remove surplus spaces from 	*/
	{						/* end of line.			*/
	    text[strlen(text) - 2] = text[strlen(text) - 1];
	    text[strlen(text)-1] = NULL;
	}
    }

    strcpy(text+strlen(text),"%\n");	/* Add line terminator '%' to end.		*/

    if ((text[66] == '%') && (text[64] == ' ') ) /* Remove extra space. 		*/
    {
	text[64] = text[65];
     	strcpy(text+65,"%\n");
    }	

    fputs(text,new_fptr);		/* Write new record to file.			*/

    /*----------------------------------------------------------------------------------*/
    /*                           CONVERT THE REST OF THE FILE				*/
    /*----------------------------------------------------------------------------------*/

    do
    {
	rec_desc = read_line();

    	if (fixed_length == TRUE)
	    while(text[strlen(text) - 2] == ' ')	/* Remove surplus spaces from 	*/
	    {						/* end of line.			*/
	    	text[strlen(text) - 2] = text[strlen(text) - 1];
	    	text[strlen(text) - 1] = NULL;
	    }


	switch(rec_desc)
	{
	    case 02:
		new02();
		break;

	    case 03:
		new03();
		break;

	    case 04:
		new04();
		break;

	    case 05:
		new05();
		break;

	    case 06:
		new06();
		break;

	    case 07:
		new07();
		break;

	    case 14:
		new14();
		break;

	    case 15:
		new15();
		break;

	    case 23:
		new23();
		break;

	    case 40:
		new40();
		break;

	    case 90:
		new90();
		break;

	    case 99:
		if ( (text[strlen(text) - 1] == '0') || (text[strlen(text) - 2] == '0') )
		{
		    end_of_file = TRUE;
		    strcpy(text,"99End of Transfer Set0\%\n");
		}
		else
		{
		    sprintf(text,
			"99End of Volume (%d). Transfer Set continues on Volume (%d)"
									,vol_num,vol_num+1);
		    strcat (text,".1\%\n");
		    vol_num++;
		}
    		fputs(text,new_fptr);		/* Write new record to file.		*/
		break;

	    default:
    		strcpy(text+strlen(text),"%\n");/* Add line terminator '%' to end.	*/
    		fputs(text,new_fptr);		/* Write new record to file.		*/
		break;
	}
    }
    while (end_of_file == FALSE);
		

    /*----------------------------------------------------------------------------------*/
    /*                             CLOSE NEW FILE AND QUIT				*/
    /*----------------------------------------------------------------------------------*/

    fclose (new_fptr);

    fprintf(stderr,"\n");
    fprintf(stderr,"*************************************************************\n");
    fprintf(stderr,"**             CONVERSION TO NTF V2.00 COMPLETE            **\n");
    fprintf(stderr,"*************************************************************\n");
    exit(0);

}

/****************************************************************************************/
/***										      ***/
/***                            Conversion Routines to NTF V2.0			      ***/
/***  			Jo Wood, Project ASSIST, V1.0 12th June 1993                  ***/
/***										      ***/
/****************************************************************************************/

new02()	/* ADD 3 EMPTY ELEMENTS */
{

    strcpy(text+strlen(text),"%\n");		/* Add line terminator '%' to end.	*/

    if (strncmp(text+2,"OS_1:625",8) == 0)	/* Change DBNAME field.			*/
	strncpy(text+2,"OS_ROUTEPLANNER_DATA",20);
 
    fputs(text,new_fptr);			/* Write new record to file.		*/
   
    if ( text[strlen(text) - 3] == '1')		/* Modify continuation record.		*/
    {
	read_line();
	strcpy(text+30,"                    00000000000%\n");
    	fputs(text,new_fptr);			/* Write new record to file.		*/
    }
}

new03()	/* ADD 3 EMPTY ELEMENTS */
{
    char	temp[80],			/* Temporarily stores new record.	*/

		cont_mark;

    strncpy(temp,text+50,strlen(text) - 51);
    temp[strlen(text)-51] = '\\';
    strncpy(temp+strlen(text)-50,text+20,6);
    strncpy(temp+strlen(text)-45,text+26,24);
    strcpy(temp+strlen(text)-23,"  0\%\n");

    cont_mark = text[strlen(text) - 3];

    strcpy(text+20,temp);
    fputs(text,new_fptr);			/* Write new record to file.		*/


    if (cont_mark == '1')			/* Ignore any continuation lines.	*/
    {
	read_line();

    	while (text[strlen(text)] == '1')
	    read_line();
    }
}

new04() /* ADD ONE EMPTY ELEMENT */
{
    char 	cont_mark,			/* Identifies continuation lines.	*/
 		posn=16,offset=0,		/* Position along string.		*/
		temp[90];			/* Temporary storage of record.		*/

    while(posn+11 < strlen(text)-10) 
    {
	strncpy(temp+posn-16+offset,text+posn,10);
	temp[posn-6+offset] = ' ';
	posn +=10;
	offset ++;
    }
    strcpy(temp+posn-16+offset,text+posn);
    strcpy(text+16,temp);
    strcpy(text+strlen(text),"%\n");		/* Add line terminator '%' to end.	*/
    fputs(text,new_fptr);			/* Write new record to file.		*/

    while (text[strlen(text) - 3] == '1')	/* Add extra element to continuation 	*/
    {						/* lines.				*/
	read_line();
	offset=0;
	posn=2;
    	while(posn+11 < strlen(text)-10)
    	{
	    strncpy(temp+posn-2+offset,text+posn,10);
	    temp[posn+8+offset] = ' ';
	    posn +=10;
	    offset ++;
    	}
    	strcpy(temp+posn-2+offset,text+posn);
    	strcpy(text+2,temp);
    	strcpy(text+strlen(text),"%\n");	/* Add line terminator '%' to end.	*/
    	fputs(text,new_fptr);			/* Write new record to file.		*/
    }
}


new05()	/* ADD THE FEAT_CODE ELEMENT */
{
    char buffer[5],
	 letter_code;
    int	 feat_code;

    strcpy(text+strlen(text),"%\n");		/* Add line terminator '%' to end.	*/

    if ((strncmp(text+2,"    ",4) == 0) &&	/* If attribute is referenced by the 	*/
	(strncmp(text+6,"          ",10) != 0))	/* CODE_COM, convert it into 4 digit FC	*/
    {

	/* To convert a 7 digit CODE_COM to a 4 digit FEAT_CODE:

		1. Feature code comprises of digits 3,4,5 & 6 of CODE_COM.
		2. Add the numeric equivalent (A=0, Z=25) of digit 1.
		3. Add 26 * digit 2.
		4. Add 260 for Line data, and 520 for Area data.

	   eg.
		H11032L  becomes 1032 + 7 + (26*1) + 260 = 1325

	   Note, there could be an overflow, if the central 4 digits > 9220

	*/

	strncpy(buffer,	text+8,4);		/* Copy main numeric code		*/
	buffer[4] = NULL;
    	feat_code = atoi(buffer);		/* Step 1.				*/

	letter_code = text[6];			/* Step 2.				*/
    	feat_code += letter_code - 'A';		
	feat_code += 26 * text[7];		/* Step 3.				*/
	
	switch(text[12])			/* Step 4.				*/
	{
	    case 'P':
		feat_code += 0 * 260;
		break;
	    case 'L':
		feat_code += 1 * 260;
		break;
	    case 'A':
		feat_code += 2 * 260;
		break;
	    default:
		fprintf(stderr,"WARNING: Non standard attribute code.\n");
		break;
	}

	sprintf(text+2,"%d",feat_code);
	text[6] = letter_code;
    }

    fputs(text,new_fptr);			/* Write new record to file.		*/
   
}


new06() /* IGNORE THIS RECORD */
{
    while (text[strlen(text)-1] == '1')
	read_line();
}

new07() /* CONVERT Z_DATUM TO 10 DIGITS */
{
    char 	cont_mark;			/* Identifies continuation lines.	*/

    strcpy(text+74,"  1\%\n");
    fputs(text,new_fptr);			/* Write new record to file.		*/

    read_line();
    cont_mark = text[strlen(text)-1];
    strcpy(text+strlen(text)-1,"0%\n");		/* Add line terminator '%' to end.	*/
    fputs(text,new_fptr);			/* Write new record to file.		*/


    if (cont_mark == '1')
    {
    	do					/* Read following lines.		*/
    	{	
	    read_line();
    	}
    	while (text[strlen(text)-1] == '1');
    }
}


new14()	/* CONVERT ANY LC(LONG CODE) VALUES TO FC (FEAT_CODE) VALUES */
{
    char buffer[5],
	 letter_code,
	 new_text[80],
	 new_textscan=0,
	 orient,
	 txt_string=FALSE;

    int	 feat_code,
	 textscan;

    do
    {
	for (textscan=0; textscan<strlen(text); textscan++)
	{
	    if ((strncmp(text+textscan,"LC",2) ==0 ) && (txt_string == FALSE))
	    {
		/* To convert a 7 digit CODE_COM to a 4 digit FEAT_CODE:

		1. Feature code comprises of digits 3,4,5 & 6 of CODE_COM.
		2. Add the numeric equivalent (A=0, Z=25) of digit 1.
		3. Add 26 * digit 2.
		4. Add 260 for Line data, and 520 for Area data.

		   eg.
			H11032L  becomes 1032 + 7 + (26*1) + 260 = 1325

		Note, there could be an overflow, if the central 4 digits > 9220

		*/

		strncpy(buffer,	text+textscan+4,4);	/* Copy main numeric code	*/
		buffer[4] = NULL;
    		feat_code = atoi(buffer);		/* Step 1.			*/

		letter_code = text[textscan+2];		/* Step 2.			*/
	    	feat_code += letter_code - 'A';		
		feat_code += 26 * text[textscan+3];	/* Step 3.			*/
	
		switch(text[textscan+8])		/* Step 4.			*/
		{
		    case 'P':
			feat_code += 0 * 260;
			break;
		    case 'L':
			feat_code += 1 * 260;
			break;
		    case 'A':
			feat_code += 2 * 260;
			break;
		    default:
			fprintf(stderr,"WARNING: Non standard attribute code.\n");
			break;
		}
		sprintf(new_text+new_textscan,"FC%d",feat_code);
		new_textscan += 6;
		textscan     += 9;
	    }
	    else
	    	if ((strncmp(text+textscan,"OR",2) == 0) && (txt_string == FALSE))
	    	{
		    for (orient=0; orient<5; orient++)
		        *(new_text+new_textscan+orient) = *(text+textscan+orient);

		    new_textscan += orient;
		    textscan += orient+1;		/* Ignore last digit of number.	*/
		}
		else
	    	{
		    *(new_text+new_textscan) = *(text+textscan);
		    new_textscan++;

		    if ((strncmp(text+textscan,"PN",2) == 0) ||
			(strncmp(text+textscan,"NU",2) == 0) )
			txt_string = TRUE;
		    else
		        if (*(text+textscan) == '\\')
			    txt_string = FALSE;
	    	}
	}

    }
    while (*(text+strlen(text)-1) == 1);	/* Repeat for continuation lines.	*/

    strcpy(new_text+new_textscan,"%\n");	/* Add line terminator '%' to end.	*/

    fputs(new_text,new_fptr);			/* Write new record to file.		*/
}



new15()	/* REMOVE NAME ID AND ADD NUM_ATT */
{
    char cont_mark;				/* Identifies continuation lines.	*/
    cont_mark = text[strlen(text) - 1];

    strcpy(text+strlen(text),"%\n");		/* Add line terminator '%' to end.	*/

    if (text[27] == '%')			/* Default configuration		*/
    {
	text[16] = text[14];			/* Move ATT_ID forward two characters.	*/
	text[17] = text[15];
	text[18] = text[16];
	text[19] = text[17];
	text[20] = text[18];
	text[21] = text[19];

	text[14] = '0';				/* Add NUM_ATT				*/
	text[15] = '1';
	

	text[22] = text[26];			/* Move terminator back 4 spaces.	*/
    	strcpy(text+23,"%\n");
    }
    else
	if ((text[37] == '%') && (text[35] == ' ') ) /* Remove extra space. 		*/
	{
	    text[35] = text[36];
     	    strcpy(text+36,"%\n");
	}	

    fputs(text,new_fptr);			/* Write new record to file.		*/
  
}


new23()	/* REMOVE NAME ID AND ADD NUM_ATT */
{
    char cont_mark;				/* Identifies continuation lines.	*/
    cont_mark = text[strlen(text) - 1];

    strcpy(text+strlen(text),"%\n");		/* Add line terminator '%' to end.	*/

    if (text[27] == '%')			/* Default configuration		*/
    {
	text[16] = text[14];			/* Move ATT_ID forward two characters.	*/
	text[17] = text[15];
	text[18] = text[16];
	text[19] = text[17];
	text[20] = text[18];
	text[21] = text[19];

	text[14] = '0';				/* Add NUM_ATT				*/
	text[15] = '1';
	

	text[22] = text[26];			/* Move terminator back 4 spaces.	*/
    	strcpy(text+23,"%\n");
    }
    else
	if ((text[37] == '%') && (text[35] == ' ') ) /* Remove extra space. 		*/
	{
	    text[35] = text[36];
     	    strcpy(text+36,"%\n");
	}		

    fputs(text,new_fptr);			/* Write new record to file.		*/
}

new40()	/* ADD SECOND DIVIDER TO END OF STRING */
{
    char cont_mark;				/* Identifies continuation lines.	*/
    cont_mark = text[strlen(text) - 1];


    strcpy(text+strlen(text)-1,"\\0%\n");	/* Add second '\' to string and		*/
						/* line terminator '%' to end.		*/
    text[strlen(text) - 3] = cont_mark;

    fputs(text,new_fptr);			/* Write new record to file.		*/
  
}



new90() /* IGNORE THIS RECORD */
{
    while (text[strlen(text)-1] == '1')
	read_line();
}



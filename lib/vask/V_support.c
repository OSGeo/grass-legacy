/**
 * \file V_support.c
 *
 * \brief Interactive support functions.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * \author GRASS GIS Development Team
 *
 * \date 1999-2006
 */

#include <grass/config.h>
#include <stdio.h>
#ifndef __MINGW32__
#include <pwd.h>
#endif
#include <unistd.h>
#include <sys/types.h>
#include <curses.h>
#include <grass/gis.h>
#include <grass/vask.h>
#include <grass/glocale.h>


/**
 * \fn int V__dump_window ()
 *
 * \brief Dumps screen to file.
 *
 * \return 0 on success
 * \return -1 on error
 */

int V__dump_window()
{
	int atrow, atcol ;
	FILE *file ;
	char home[80] ;
	int curx, cury ;

#ifdef __MINGW32__
        /* Quick hack in absence of understanding */ 
        sprintf(home,"c:/visual_ask" ) ;
#else        
	sprintf(home,"%s/visual_ask", getpwuid(getuid())->pw_dir ) ;
#endif
    
	if ((file=fopen(home, "a")) == NULL)
	{
		G_warning (_("Unable to open file %s"), home);
		return(-1) ;
	}

	getyx(stdscr, cury, curx) ;

	fprintf(file,"--------------------------------------------------------\n") ;
	for (atrow=0; atrow<LINES; atrow++)
	{
		for (atcol=0; atcol<COLS-1; atcol++)
		{
			move(atrow, atcol) ;
			fprintf(file,"%c",(int)(inch() & A_CHARTEXT)) ;
		}
		fprintf(file,"\n") ;
	}
	fprintf(file,"--------------------------------------------------------\n") ;
	fprintf(file,"\n\n") ;
	fclose(file) ;

	move(cury, curx) ;
	return 0;
}


/**
 * \fn int V__remove_trail (int ans_col, char *ANSWER)
 *
 * \brief Remove trailing text from <b>ANSWER</b>?
 *
 * \param[in] ans_col
 * \param[in] ANSWER
 * \return always returns 0;
 */

int V__remove_trail( int ans_col , char *ANSWER )
{
	char *ANS_PTR ;

	ANS_PTR = ANSWER + ans_col ;
	while (ans_col>=0) 
	{
		int c = *(unsigned char *)ANS_PTR;
		if (c > '\040' && c != '\177' && c != '_')
			return 0 ;

		*ANS_PTR = '\0' ;
		ans_col-- ;
		ANS_PTR-- ;
	}

	return 0;
}

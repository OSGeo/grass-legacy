/* Frank Koormann	$Date$
 * $Id$
 * 
 * Copyright (C) 2000 by Frank Koormann
 * 
 *    This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License
 *   as published by the Free Software Foundation; either version 2
 *   of the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Description: utils.c
 *	collection of useful functions:
 *	- getline
 *	- do_nothing	
 *	- tabtok
 *	explanations see below. *
 *
 */

#include "utils.h"

/* getline -------------------------------------------------------------*/
/* reads a line (limited by \n) out of stream fp, returns last 
 * char, esp eof                                                        */
int getline(FILE *fp, char s[] )
{	int c, i;

	i=0;
	while ( (c=getc(fp))!=EOF && c!='\n' )
		s[i++]=c;
	s[i]='\0';
	return c;
}

/* do_nothing ----------------------------------------------------------*/
/* dto.									*/
void do_nothing( void )
{
}

/* tabtok --------------------------------------------------------------*/
/* like strtok, breaks a string in sequences delimited by tabs, but do not
 * overreads sequences of directly followed tabs: like "\t\t\ttest" is
 * divided by strtok into "test" but by tabtok into "", "", "", "test"  */
char *tabtok( char *s ) 
{        
        static char *b, *e; 
 
        if ( s == NULL ) 
                b = e; 
        else 
                b = s; 
 
        if ( b == NULL ) 
                return b; 
        else 
                e = b;
         
        while ( (*e != '\t') && (*e != '\0') )
                e++; 
         
        if ( *e == '\0' ) 
                e = NULL; 
        else 
                *e = '\0', e++;
 
        return b; 
} 

/* dtok --------------------------------------------------------------*/
/* like strtok, breaks a string in sequences delimited by delim, but do not
 * overreads sequences of directly followed delims: like "\t\t\ttest" is
 * divided by strtok into "test" but by dtok into "", "", "", "test"  */
char *dtok( char *s, char delim ) 
{        
        static char *b, *e; 
 
        if ( s == NULL ) 
                b = e; 
        else 
                b = s; 
 
        if ( b == NULL ) 
                return b; 
        else 
                e = b;
         
        while ( (*e != delim ) && (*e != '\0') )
                e++; 
         
        if ( *e == '\0' ) 
                e = NULL; 
        else 
                *e = '\0', e++;
 
        return b; 
} 

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
 * Description: header of utils.c
 *	collection of useful functions:
 *	- getline
 *	- do_nothing	
 *	- tabtok
 *	explanations see below.
 */

#ifndef __utils__
#define __utils__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
/* global variables ----------------------------------------------------*/
/* action of a program after evaluating the command line		*/
#define ABORT 0
#define EVALUATE 1
#define LIST 2

/* function declaration ------------------------------------------------*/

/* getline: reads a line (limited by \n) out of stream fp, returns last 
 * char, esp eof							*/
int getline(FILE *fp, char s[]);

/* do_nothing: dto.							*/
void do_nothing( void );

/* tabtok --------------------------------------------------------------
 * like strtok, breaks a string in sequences delimited by tabs, but do not 
 * overreads sequences of directly followed tabs: like "\t\t\ttest" is
 * divided by strtok into "test" but by tabtok into "", "", "", "test"	*/  
char *tabtok( char *s );

/* dtok --------------------------------------------------------------
 * like strtok, breaks a string in sequences delimited by delim, but do not 
 * overreads sequences of directly followed delims: like "\t\t\ttest" is
 * divided by strtok into "test", but by dtok into "", "", "", "test"	*/  
char *dtok( char *s , char delim );

#endif


/******************************************************************************
 * $Id: dbfadd.c,v 1.1 2000/03/08 14:34:07 markus Exp $
 *
 * Project:  Shapelib
 * Purpose:  Sample application for adding a record to an existing .dbf file.
 * Author:   Frank Warmerdam, warmerda@home.com
 *
 ******************************************************************************
 * Copyright (c) 1999, Frank Warmerdam
 *
 * This software is available under the following "MIT Style" license,
 * or at the option of the licensee under the LGPL (see LICENSE.LGPL).  This
 * option is discussed in more detail in shapelib.html.
 *
 * --
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************
 *
 * $Log: dbfadd.c,v $
 * Revision 1.1  2000/03/08 14:34:07  markus
 * contribution by Frank Warmerdam
 *
 * Revision 1.5  1999/11/05 14:12:04  warmerda
 * updated license terms
 *
 * Revision 1.4  1998/12/03 16:36:06  warmerda
 * Added stdlib.h and math.h to get atof() prototype.
 *
 * Revision 1.3  1995/10/21 03:13:23  warmerda
 * Use binary mode..
 *
 * Revision 1.2  1995/08/04  03:15:59  warmerda
 * Added header.
 *
 */

static char rcsid[] = 
  "$Id: dbfadd.c,v 1.1 2000/03/08 14:34:07 markus Exp $";

#include "shapefil.h"
#include <math.h>
#include <stdlib.h>

int main( int argc, char ** argv )

{
    DBFHandle	hDBF;
    int		i, iRecord;

/* -------------------------------------------------------------------- */
/*      Display a usage message.                                        */
/* -------------------------------------------------------------------- */
    if( argc < 3 )
    {
	printf( "dbfadd xbase_file field_values\n" );

	exit( 1 );
    }

/* -------------------------------------------------------------------- */
/*	Create the database.						*/
/* -------------------------------------------------------------------- */
    hDBF = DBFOpen( argv[1], "r+b" );
    if( hDBF == NULL )
    {
	printf( "DBFOpen(%s,\"rb+\") failed.\n", argv[1] );
	exit( 2 );
    }
    
/* -------------------------------------------------------------------- */
/*	Do we have the correct number of arguments?			*/
/* -------------------------------------------------------------------- */
    if( DBFGetFieldCount( hDBF ) != argc - 2 )
    {
	printf( "Got %d fields, but require %d\n",
	        argc - 2, DBFGetFieldCount( hDBF ) );
	exit( 3 );
    }

    iRecord = DBFGetRecordCount( hDBF );

/* -------------------------------------------------------------------- */
/*	Loop assigning the new field values.				*/
/* -------------------------------------------------------------------- */
    for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
    {
	if( DBFGetFieldInfo( hDBF, i, NULL, NULL, NULL ) == FTString )
	    DBFWriteStringAttribute(hDBF, iRecord, i, argv[i+2] );
	else
	    DBFWriteDoubleAttribute(hDBF, iRecord, i, atof(argv[i+2]) );
    }

/* -------------------------------------------------------------------- */
/*      Close and cleanup.                                              */
/* -------------------------------------------------------------------- */
    DBFClose( hDBF );

    return( 0 );
}

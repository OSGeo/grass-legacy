/***************************************************************************
* $Id$
*
* MODULE: 	g.version
* AUTHOR(S):	Michael Shapiro, CERL
*               Andreas Lange - <andreas.lange@rhein-main.de>
*  	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
* PURPOSE: 	Output GRASS version number, date and copyright message.
*             
* COPYRIGHT:  	(C) 2000 by the GRASS Development Team
*
*   	    	This program is free software under the GPL (>=v2)
*   	    	Read the file COPYING that comes with GRASS for details.
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include "gis.h"

#ifndef GRASS_VERSION_UPDATE_PKG
#define GRASS_VERSION_UPDATE_PKG "0.1"
#endif

int main(int argc, char *argv[])
{
    struct GModule *module;
    struct Flag *copyright;
    
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = "Displays version and copyright information.";

    copyright = G_define_flag();
    copyright->key = 'c';
    copyright->description = "Print the copyright message as well";

    if (argc > 1 && G_parser(argc, argv))
	exit(1);

    fprintf (stdout, "GRASS %s (%s) %s\n",
    	GRASS_VERSION_NUMBER, GRASS_VERSION_DATE, GRASS_VERSION_UPDATE_PKG );
    fprintf (stdout, "\n");
    
    if (copyright->answer)
    	fputs (COPYING, stdout);
    
    return 0;
}

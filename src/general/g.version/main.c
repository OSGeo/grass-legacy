#include <stdio.h>

int main(int argc, char *argv[])
{
    fprintf (stdout, "GRASS %s (%s) %s\n", 
	    VERSION_NUMBER, VERSION_DATE, VERSION_UPDATE_PKG );
    fprintf (stdout, "\n");
    fprintf (stdout, "Copyright and License Statement\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "The Geographic Resources Analysis and Support System (GRASS)\n");
    fprintf (stdout, "Geographic Information System (GIS) is Copyright by the\n");
    fprintf (stdout, "GRASS Development Team headquartered at Baylor University,\n");
    fprintf (stdout, "in Waco, Texas.\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "This program is free software; you can redistribute it and/or modify it\n");
    fprintf (stdout, "under the terms of the GNU General Public License as publishe by the\n");
    fprintf (stdout, "Free Software Foundation; either version 2 of the License, or (at your\n");
    fprintf (stdout, "option) any later version.\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "This program is distributed in the hope that it will be useful,\n");
    fprintf (stdout, "but WITHOUT ANY WARRANTY; without even the implied warranty of\n");
    fprintf (stdout, "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n");
    fprintf (stdout, "GNU General Public License (GPL) for more details.\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "You should have received a copy of the GNU General Public License\n");
    fprintf (stdout, "along with this program; if not, write to the\n");
    fprintf (stdout, "  Free Software Foundation, Inc.,\n");
    fprintf (stdout, "  59 Temple Place - Suite 330,\n");
    fprintf (stdout, "  Boston, MA  02111-1307, USA.\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "Questions regarding GRASS GIS should be directed to the\n");
    fprintf (stdout, "GRASS Development Team at the following address:\n");
    fprintf (stdout, "  GRASS Development Team\n");
    fprintf (stdout, "  Center for Applied Geographic and Spatial Research\n");
    fprintf (stdout, "  Baylor University\n");
    fprintf (stdout, "  P.O. Box 7351\n");
    fprintf (stdout, "  Waco, Texas  76798-7351\n");
    fprintf (stdout, "  254-710-6814\n");
    fprintf (stdout, "  grass@baylor.edu\n");
    fprintf (stdout, "  http://www.baylor.edu/~grass/\n");
    fprintf (stdout, "\n");    
    exit(0);
}

/*****************************************************************************/
/***                                                                       ***/
/***				write_head()                               ***/
/***         Writes out VRML header information (texture, light etc.)	   ***/
/***                  Jo Wood, V1.0, 23rd April, 1996		           ***/
/***                                                                       ***/
/*****************************************************************************/

#include "delaunay.h"

write_head(region)
    struct Cell_head region;
{
    /*------------------------------------------------------------------*/
    /*				INITIALISE				*/
    /*------------------------------------------------------------------*/

    int centreX,		/* Centre of view */
	centreY,		
  	camheight;		/* Height of camera above suface. */

    centreX = (region.east - region.west)/2;	/* Set up camera location. */
    centreY = (region.north - region.south)/2;
    camheight = centreX;

    /*------------------------------------------------------------------*/
    /*			DISPLAY HEADER INFORMATION			*/
    /*------------------------------------------------------------------*/

    printf("#VRML V1.0 ascii\n\n");
    printf("# -- Original raster size %d rows by %d columns.\n\n",
						region.rows,region.cols);
    printf("# --        North: %.1f\n",region.north);
    printf("# --  West: %.1f      East: %.1f\n",region.west, region.east);
    printf("# --        South: %.1f\n\n",region.south);
    printf("# -- Triangulation based on Delaunay triangulation of VIPs\n");
    printf("# -- Vertical scaling: %f\n",zscale);

    printf("Separator {\n");
    printf("    Material {\n");
    printf("        ambientColor  0.5 0.5 0.5\n");
    printf("        diffuseColor  0.4 0.4 0.4\n");
    printf("        specularColor 0.4 0.4 0.4\n");
    printf("        emissiveColor 0.3 0.3 0.3\n");
    printf("        shininess     0.2\n");
    printf("        transparency  0\n");
    printf("    } # End of Material\n\n");

    printf("    DEF TINsurf Separator {\n");
    printf("        PerspectiveCamera {\n");
    printf("            position %d %d %d\n",centreX,centreY,camheight);
    printf("        } # End of PerspectiveCamera \n\n");


}

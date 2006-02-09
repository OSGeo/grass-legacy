/****************************************************************************
*
* MODULE:       r.out.vtk  
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert@gmx.de
* 		08 23 2005 Berlin
* PURPOSE:      Converts raster maps into the VTK-Ascii format  
*
* COPYRIGHT:    (C) 2005 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "globaldefs.h"



/* ************************************************************************* */
/* Write the normal VTK Header, no! Elevation map is supportet ****************** */
/* ************************************************************************* */
void
writeVTKNormalHeader (FILE * fp, struct Cell_head region, double elevation, int type)
{
  G_debug (3, _("writeVTKNormalHeader: Writing VTK-Header"));

  /*Simple vtk ASCII header */
  fprintf (fp, "# vtk DataFile Version 3.0\n");
  fprintf (fp, "GRASS 6 Export\n");
  fprintf (fp, "ASCII\n");
  fprintf (fp, "DATASET STRUCTURED_POINTS\n");	/*We are using the structured point dataset. */

  if (type)
    fprintf (fp, "DIMENSIONS %i %i %i\n", region.cols, region.rows, 1);
  else
    fprintf (fp, "DIMENSIONS %i %i %i\n", region.cols + 1, region.rows + 1, 1);

  fprintf (fp, "SPACING %lf %lf %lf\n", region.ew_res, region.ns_res, 0.0);

  if (type)
    fprintf (fp, "ORIGIN %lf %lf %lf\n", region.west + region.ew_res / 2, region.south + region.ns_res / 2, elevation);
  else
    fprintf (fp, "ORIGIN %lf %lf %lf\n", region.west, region.south, elevation);

}


/* ************************************************************************* */
/* Write the Elevtaion VTK Header, Elevation is supportet ****************** */
/* ************************************************************************* */
void
writeVTKStructuredElevationHeader (FILE * fp, struct Cell_head region)
{
  G_debug (3, _("writeVTKStructuredElevationHeader: Writing VTK-Header"));

  /*Simple vtk ASCII header */
  fprintf (fp, "# vtk DataFile Version 3.0\n");
  fprintf (fp, "GRASS 6 Export\n");
  fprintf (fp, "ASCII\n");
  fprintf (fp, "DATASET STRUCTURED_GRID\n");	/*We are using the structured grid dataset. */
  fprintf (fp, "DIMENSIONS %i %i %i\n", region.cols, region.rows, 1);
  fprintf (fp, "POINTS %i float\n", region.cols * region.rows);	/*The Coordinates */
}

/* ************************************************************************* */
/* Write the Rectilinear Elevtaion VTK Header, Elevation is supportet ****** */
/* ************************************************************************* */
void
writeVTKPolygonalElevationHeader (FILE * fp, struct Cell_head region)
{
  G_debug (3, _("writeVTKPolygonalElevationHeader: Writing VTK-Header"));

  /*Simple vtk ASCII header */
  fprintf (fp, "# vtk DataFile Version 3.0\n");
  fprintf (fp, "GRASS 6 Export\n");
  fprintf (fp, "ASCII\n");
  fprintf (fp, "DATASET POLYDATA\n");	/*We are using polydataset. */
  fprintf (fp, "POINTS %i float\n", region.cols * region.rows);
}

/* ************************************************************************* */
/* Write the CellDataHeader ************************************************ */
/* ************************************************************************* */
void
writeVTKCellDataHeader (FILE * fp, struct Cell_head region)
{
  G_debug (3, _("writeVTKCellDataHeader: Writing VTK-DataHeader"));
  fprintf (fp, "CELL_DATA %i\n", region.cols * region.rows);
}

/* ************************************************************************* */
/* Write the PointDataHeader ************************************************ */
/* ************************************************************************* */
void
writeVTKPointDataHeader (FILE * fp, struct Cell_head region)
{
  G_debug (3, _("writeVTKPointHeader: Writing VTK-DataHeader"));
  fprintf (fp, "POINT_DATA %i\n", region.cols * region.rows);
}


/* ************************************************************************* */
/* Write the VTK Structured Coordinates ************************************ */
/* ************************************************************************* */
void
writeVTKStructuredCoordinates (int fd, FILE * fp, char *varname, struct Cell_head region, int out_type, char *null_value, double scale)
{
  int ncols = region.cols;
  int nrows = region.rows;
  int row, col;
  int rowcount = 0, colcount = 0;
  double nspos = 0.0, ewpos = 0.0;
  void *ptr, *raster;

  G_debug (3, _("writeVTKStructuredCoordinates: Writing Coordinates"));


  raster = G_allocate_raster_buf (out_type);

  rowcount = 0;
  for (row = nrows - 1; row >= 0; row--)
    {
      colcount = 0;
      G_percent ((row - nrows) * (-1), nrows, 2);

      if (G_get_raster_row (fd, raster, row, out_type) < 0)
	{
	  G_fatal_error (_("Unable to read row %i\n"), row);
	  return;
	}

      nspos = region.ns_res / 2 + region.south + rowcount * region.ns_res;

      for (col = 0, ptr = raster; col < ncols;
	   col++, ptr = G_incr_void_ptr (ptr, G_raster_size (out_type)))
	{
	  ewpos = region.ew_res / 2 + region.west + colcount * region.ew_res;

	  if (!G_is_null_value (ptr, out_type))
	    {
	      if (out_type == CELL_TYPE)
		fprintf (fp, "%lf %lf %lf\n", ewpos, nspos,
			 *((CELL *) ptr) * scale);
	      else if (out_type == FCELL_TYPE)
		{
		  fprintf (fp, "%lf %lf %lf\n", ewpos, nspos,
			   *((FCELL *) ptr) * scale);
		}
	      else if (out_type == DCELL_TYPE)
		{
		  fprintf (fp, "%lf %lf %lf\n", ewpos, nspos,
			   *((DCELL *) ptr) * scale);
		}
	    }
	  else
	    fprintf (fp, "%lf %lf %s\n", ewpos, nspos, null_value);

	  colcount++;
	}
      rowcount++;
    }
  return;
}

/* ************************************************************************* */
/* Write the VTK Data ****************************************************** */
/* ************************************************************************* */
void
writeVTKPolygonalCoordinates (int fd, FILE * fp, char *varname, struct Cell_head region, int out_type, char *null_value, double scale, int polytype)
{
  int ncols = region.cols;
  int nrows = region.rows;
  int row, col;
  int rowcount = 0, colcount = 0;
  double nspos = 0.0, ewpos = 0.0;
  void *ptr, *raster;
  int i, j, count;

  G_debug (3, _("writeVTKPolygonalCoordinates: Writing VTK Polygonal data"));


  /*First we are writing the coordinate points, the elevation cell is only used for the z coordinate */
  raster = G_allocate_raster_buf (out_type);

  rowcount = 0;
  for (row = nrows - 1; row >= 0; row--)
    {
      colcount = 0;
      G_percent ((row - nrows) * (-1), nrows, 10);

      if (G_get_raster_row (fd, raster, row, out_type) < 0)
	{
	  G_fatal_error (_("Unable to read row %i\n"), row);
	  return;
	}

      nspos = region.ns_res / 2 + region.south + rowcount * region.ns_res;

      for (col = 0, ptr = raster; col < ncols;
	   col++, ptr = G_incr_void_ptr (ptr, G_raster_size (out_type)))
	{
	  ewpos = region.ew_res / 2 + region.west + colcount * region.ew_res;

	  if (!G_is_null_value (ptr, out_type))
	    {
	      if (out_type == CELL_TYPE)
		fprintf (fp, "%9f %9f %9f\n", ewpos, nspos,
			 *((CELL *) ptr) * scale);
	      else if (out_type == FCELL_TYPE)
		{
		  fprintf (fp, "%9f %9f %9f\n", ewpos, nspos,
			   *((FCELL *) ptr) * scale);
		}
	      else if (out_type == DCELL_TYPE)
		{
		  fprintf (fp, "%9f %9f %9f\n", ewpos, nspos,
			   *((DCELL *) ptr) * scale);
		}
	    }
	  else
	    fprintf (fp, "%9f %9f %s\n", ewpos, nspos, null_value);

	  colcount++;
	}
      rowcount++;
    }

/*Now we need to write the Connectivity between the points*/

  if (polytype == QUADS) /*The default*/
    {
      /*If Datafiltering should be supportet, we use Polygons to represent the grid */
      fprintf (fp, "POLYGONS %i %i\n", (region.rows - 1) * (region.cols - 1), 5 * (region.rows - 1) * (region.cols - 1));

      /*We creat a grid of quads, the corners of the quads are the datapoints */
      for (i = 0; i < region.rows - 1; i++)
	{
	  for (j = 0; j < region.cols - 1; j++)
	    {
	      fprintf (fp, "4 %i %i %i %i \n", i * region.cols + j, i * region.cols + j + 1, (i + 1) * region.cols + j + 1, (i + 1) * region.cols + j);
	    }
	}
    }


  if (polytype == TRIANGLE_STRIPS)
    {
      /*TriangleStrips, take a look ate www.vtk.org for the definition of triangle_strips in vtk */
      /*If we use triangelestrips, the number of points per strip is equal to the double number of cols we have */
      fprintf (fp, "TRIANGLE_STRIPS %i %i\n", region.rows - 1, (region.rows - 1) + (region.rows - 1) * (2 * region.cols));

      /*For every Row-1 make a strip */
      for (i = 0; i < region.rows - 1; i++)
	{

	  /*Number of Points */
	  fprintf (fp, "%i ", region.cols * 2);

	  /*Write the points */
	  for (j = 0; j < region.cols; j++)
	    {
	      fprintf (fp, "%i %i ", i * region.cols + j,
		       (i + 1) * region.cols + j);
	    }
	  fprintf (fp, "\n");
	}
    }

  if (polytype == VERTICES)
    {
      /*If the Data should be Triangulated by VTK, we use vertices to represent the grid */
      fprintf (fp, "VERTICES %i %i\n", region.rows, region.rows + (region.rows) * (region.cols));

      count = 0;
      for (i = 0; i < region.rows; i++)
	{
	  fprintf (fp, "%i ", (region.cols));
	  for (j = 0; j < region.cols; j++)
	    {
	      fprintf (fp, "%i ", count);
	      count++;
	    }
	  fprintf (fp, "\n");
	}
    }

  return;
}

/* ************************************************************************* */
/* Write the VTK Data ****************************************************** */
/* ************************************************************************* */

void
writeVTKData (int fd, FILE * fp, char *varname, struct Cell_head region,
	      int out_type, char *null_value)
{
  int ncols = region.cols;
  int nrows = region.rows;
  int row, col;
  void *ptr, *raster;

  G_debug (3, _("writeVTKData: Writing VTK-Data"));

  fprintf (fp, "SCALARS %s float 1\n", varname);
  fprintf (fp, "LOOKUP_TABLE default\n");


  raster = G_allocate_raster_buf (out_type);

  for (row = nrows - 1; row >= 0; row--)
    {
      G_percent ((row - nrows) * (-1), nrows, 10);

      if (G_get_raster_row (fd, raster, row, out_type) < 0)
	{
	  G_fatal_error (_("Unable to read row %i\n"), row);
	  return;
	}

      for (col = 0, ptr = raster; col < ncols;
	   col++, ptr = G_incr_void_ptr (ptr, G_raster_size (out_type)))
	{
	  if (!G_is_null_value (ptr, out_type))
	    {
	      if (out_type == CELL_TYPE)
		fprintf (fp, "%d ", *((CELL *) ptr));
	      else if (out_type == FCELL_TYPE)
		{
		  fprintf (fp, "%lf ", *((FCELL *) ptr));
		}
	      else if (out_type == DCELL_TYPE)
		{
		  fprintf (fp, "%lf ", *((DCELL *) ptr));
		}
	    }
	  else
	    fprintf (fp, "%s ", null_value);
	}
      fprintf (fp, "\n");
    }
  return;

}



/* ************************************************************************* */
/* Write the VTK RGB Image Data ******************************************** */
/* ************************************************************************* */

void
writeVTKRGBImageData (int redfd, int greenfd, int bluefd, FILE * fp,
		      const char *varname, struct Cell_head region,
		      int out_type)
{
  int ncols = region.cols;
  int nrows = region.rows;
  int row, col;
  void *redptr, *redraster;
  void *greenptr, *greenraster;
  void *blueptr, *blueraster;
  double r = 0.0, g = 0.0, b = 0.0;

  G_debug (3, _("writeVTKRGBImageData: Writing VTK-ImageData"));

  fprintf (fp, "COLOR_SCALARS %s 3\n", varname);


  redraster = G_allocate_raster_buf (out_type);
  greenraster = G_allocate_raster_buf (out_type);
  blueraster = G_allocate_raster_buf (out_type);

  for (row = nrows - 1; row >= 0; row--)
    {
      G_percent ((row - nrows) * (-1), nrows, 10);

      if (G_get_raster_row (redfd, redraster, row, out_type) < 0)
	{
	  G_fatal_error (_("Unable to read row %i\n"), row);
	  return;
	}
      if (G_get_raster_row (greenfd, greenraster, row, out_type) < 0)
	{
	  G_fatal_error (_("Unable to read row %i\n"), row);
	  return;
	}
      if (G_get_raster_row (bluefd, blueraster, row, out_type) < 0)
	{
	  G_fatal_error (_("Unable to read row %i\n"), row);
	  return;
	}

      for (col = 0, redptr = redraster, greenptr = greenraster, blueptr =
	   blueraster; col < ncols;
	   col++, redptr =
	   G_incr_void_ptr (redptr, G_raster_size (out_type)), greenptr =
	   G_incr_void_ptr (greenptr, G_raster_size (out_type)), blueptr =
	   G_incr_void_ptr (blueptr, G_raster_size (out_type)))
	{
	  r = 0;
	  g = 0;
	  b = 0;
	  if (!G_is_null_value (redptr, out_type)
	      && !G_is_null_value (greenptr, out_type)
	      && !G_is_null_value (blueptr, out_type))
	    {
	      if (out_type == CELL_TYPE)
		{
		  r = (double) *((CELL *) redptr);
		  g = (double) *((CELL *) greenptr);
		  b = (double) *((CELL *) blueptr);
		}
	      else if (out_type == FCELL_TYPE)
		{
		  r = (double) *((FCELL *) redptr);
		  g = (double) *((FCELL *) greenptr);
		  b = (double) *((FCELL *) blueptr);
		}
	      else if (out_type == DCELL_TYPE)
		{
		  r = (double) *((DCELL *) redptr);
		  g = (double) *((DCELL *) greenptr);
		  b = (double) *((DCELL *) blueptr);
		}
	      /*Test of valuerange, the data should be 1 byte gray values*/
	      if (r > 255 || g > 255 || b > 255 || r < 0 || g < 0 || b < 0)
		{
		  G_warning (_("Wrong map values! Values should in between 0 and 255!\n"));
		  fprintf (fp, "0 0 0 \n");
		}
	      else
		{
		  fprintf (fp, "%lf %lf %lf \n", r / 255, g / 255, b / 255);
		}

	    }
	  else
	    fprintf (fp, "0 0 0 \n");
	}
      fprintf (fp, "\n");
    }
  return;

}

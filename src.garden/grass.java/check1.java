/*
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
    USA
*/
/*
 * Copyright (C) 1998 The Information-technology Promotion Agency, Japan(IPA), LGPL
 * $Id$
 */
import java.util.*;
import java.lang.reflect.*;
import java.awt.*;
import GRASS.*;

/**
 * Class for testing if library works 
 *
 * @author  $Author$
 * @version $Revision$
 */
public class check1 {

  public static void main(String args[]) {

    if (args.length == 4) {
      new check1( args );
    } else {
      System.err.println("Usage:\n\tjava check1"+
			 " GISBASE GISDBASE LOCATION_NAME MAPSET");
      System.exit(-1);
    }
    
  }

  libgis gl = null;
  libfun lf = null;

  check1(String args[]) {

    try {
    
      gl = new libgis(
	this.getClass().getName(),
        args[0],  // GISBASE
        args[1],  // GISDBASE
        args[2],  // LOCATION_NAME
        args[3]   // MAPSET
      );

      lf = new libfun( gl );

    } catch (GRASSException e) {
      System.err.println("Initialization failed: "+e);
      System.exit(-1);
    }

    // build list of test method
    Method  methods[] = this.getClass().getMethods();
    boolean success[] = new boolean[ methods.length ];
    int c = 1;  // counter for tests

    // start calling function by function
    for ( int m = 0; m < methods.length; m++ ) {
      try {

	if (methods[ m ].getName().startsWith("t_")) {
	  System.err.println("\nTest "+c+++": "+
			     methods[ m ].getName());
	  long start = (new Date()).getTime();
	  methods[ m ].invoke( this, null );
	  long end = (new Date()).getTime();
	  System.err.println("Succeeded, time elapsed (ms): "+(end - start));
	  success[ m ] = true;
	}

      } catch (InvocationTargetException e) {
	success[ m ] = false;
	System.err.println("Test "+methods[ m ].getName()+
			  " failed due to:\n" + e.getTargetException());

      } catch (IllegalAccessException e) {
	System.err.println("Test sequence failed on "+methods[ m ].getName()+
			  " due to:\n" + e);
	System.exit(1);

      } catch (IllegalArgumentException e) {
	System.err.println("Test sequence failed on "+methods[ m ].getName()+
			  " due to:\n" + e);
	System.exit(1);
      }

    }

    // printing results out
    System.out.println("\nResults (only failed tests shown):");
    System.out.println("----------------");
    c = 0;
    int failed = 0;
    for (int m=0; m < methods.length; m++ ) {
      if (methods[ m ].getName().startsWith("t_") ) {
        c++;
	if ( !success[ m ] ) {
	  System.out.println(c+". "+methods[ m ].getName()+
			   ": ok="+success[ m ]);
	  failed++;
        }
      }
    }
    System.out.println("----------------");
    System.out.println( c+" tests, "+failed+" failed" );

    System.exit(failed);

  }

  // Tests start here ///////////////////////////////////////////////////

  public void t_gis_getpid() {
    System.out.println("PID="+gl.getpid());
  }
  
  public void t_gis_getGISRC() {
    System.out.println("GISRC="+gl.getGISRC());
  }

  public void t_gis_G_projection() throws GRASSException {
    System.out.println("Projection: "+gl.G_projection() );
  }

  public void t_gis_G_database_projection_name() throws GRASSException {
    System.out.println("Projection: "+gl.G_database_projection_name() );
  }

  public void t_gis_G_database_unit_name() {
    System.out.println("Unit name (plural=true): "+
		       gl.G_database_unit_name(true));
    System.out.println("Unit name (plural=false): "+
		       gl.G_database_unit_name(false));
  }

  public void t_gis_G_database_units_to_meters_factor() {
    System.out.println("units_to_meters_factor="+
		       gl.G_database_units_to_meters_factor());
  }

  public void t_gis_G_zone() {
    System.out.println( "Zone="+gl.G_zone() );
  }

  public void t_gis_g_list() throws GRASSException {
    System.out.print("List: ");
    for (Enumeration e=lf.g_list(lf.TYPE_RAST); e.hasMoreElements(); ) {
      System.out.print(e.nextElement()+" ");
    }
    System.out.println("");
  }
    
  String layer = "vegcover";
  String mapset;

  public void t_gis_G_find_cell() throws GRASSException {
    mapset = gl.G_find_cell( layer, "" );
    System.out.println("Found in mapset: "+ mapset);
  }
  
  int rows, cols;

  public void t_gis_G_get_window() {
    System.out.println( gl.G_get_window() );
  }
  
  public void t_gis_G_get_default_window() {
    System.out.println( gl.G_get_default_window() );
  }
  
  public void t_gis_G_window() {
    rows = gl.G_window_rows();
    cols = gl.G_window_cols();
    System.out.println("Region: "+rows+"x"+cols);
  }
  
  Cellhead ch;

  public void t_gis_G_get_set_window() {
    ch = gl.G_get_set_window();
    System.out.println( ch );
  }

  double easting;

  public void t_Cellhead_G_col_to_easting() {
    int c = ch.cols() >> 1;
    easting = ch.G_col_to_easting( c );
    System.out.println("Easting for column "+c+" is "+easting);
  }

  double northing;

  public void t_Cellhead_G_row_to_northing() {
    int r = ch.rows() >> 1;
    northing = ch.G_row_to_northing( r );
    System.out.println("Northing for column "+r+" is "+northing);
  }

  public void t_Cellhead_G_easting_to_col() {
    System.out.println("Column for easting "+easting+
		       " is "+ch.G_easting_to_col( easting ) );
  }

  public void t_Cellhead_G_northing_to_row() {
    System.out.println("Row for northing "+northing+
		       " is "+ch.G_northing_to_row( northing ) );

  }

  public void t_gis_G_begin_cell_area_calculations() {
    System.out.println( "G_begin_cell_area_calculations()="+
			gl.G_begin_cell_area_calculations() 
			);
  }

  public void t_gis_G_area_of_cell_at_row() {
    int r = ch.rows() >> 1;
    System.out.println( "Area at row "+r+" = "+
			gl.G_area_of_cell_at_row(r) );
  }
  
  Cellhead ch2;

  public void t_Cellhead_clone() {
    ch2 = (Cellhead)ch.clone();
    ch2.setRows( 500 ); 
    ch2.setCols( 500 );
    System.out.println( "Cloned: " + ch2 );
  }
  
  public void t_Cellhead_G_adjust_Cell_head() throws GRASSException {
    ch2.G_adjust_Cell_head(true, true);
    System.out.println( "New: " + gl.G_get_set_window() );
  }
  
  public void t_gis_G_set_window() throws GRASSException {
    gl.G_set_window(ch2);
    System.out.println( "Window set: " + gl.G_get_set_window() );
  }

  Colors cl;

  public void t_gis_G_read_colors() throws GRASSException  {
    cl = new Colors( layer, mapset );
    System.out.println( "Read: " + cl );
  }
  
  public void t_Colors_G_get_color() {
    int cat = 5;
    System.out.println( "Category "+cat+
			" has color " + cl.G_get_color( cat ) );
  }
  
  public void t_Colors_intColor() {
    int cat = 5;
    int col = cl.intColor( cat );
    System.out.println( "Category "+cat+
			" has color "+col+
			" that is "+ (new java.awt.Color(col)) );
  }
  
  CELL buf;
  
  public void t_CELL_G_allocate_cell_buf() {
    buf = new CELL();
  }
  
  public void t_CELL_G_zero_cell_buf() throws GRASSException {
    buf.G_zero_cell_buf();
    System.out.println("Alloc and zero: "+buf );
  }
  
  FD fd;

  public void t_gis_G_open_cell_old() throws GRASSException {
    fd = gl.G_open_cell_old( layer, mapset );
    System.out.println("Open fd="+fd );
  }
  
  Range range;

  public void t_Range_G_read_range() throws GRASSException {
    range = new Range( layer, mapset );
    System.out.println("Range for layer '"+layer+"' is "+range);
  }

  public void t_Range_G_update_range() {
    range.G_update_range( Integer.MAX_VALUE );
    System.out.println("Updated with MAX_INT range is "+range);
  }

  Cellstats cs;

  public void t_Cellstats_NEW() {
    cs = new Cellstats();
  }

  public void t_READING_CELLS() throws GRASSException {

    int x = 0;
    int pos = 50;
    int[] arr    = buf.toArray();
    int[] colrs  = buf.toColorArray( cl );

    range = new Range();

    for (int r = 0; r < rows; r++) {
      fd.G_get_map_row( buf, r);

      buf.toArray(arr);
      buf.toColorArray(colrs, cl);

      System.out.print("Row "+r+" val at cell "+pos+": "+
		       buf.cellAt(pos)+"\t\r");
      //System.out.print("Row "+r+":");
      for (int c = 0; c < cols; c++) { 
	int cat = buf.cellAt(c);
	Color rgb = cl.G_get_color(cat);
	//System.out.print(cat+"="+rgb+" ");

	if (  arr[c] != cat) System.err.println("toarray");
	if (colrs[c] != rgb.getRGB()) 
	  System.err.println("RGB: "+rgb+" <-> "+(new Color(colrs[c])));

      }
      range.G_row_update_range( buf, cols );
      cs.G_update_cell_stats( buf, cols );
      //System.out.println("");
    }
    System.out.println("");
    System.out.println("Recalculated range is "+range);
    fd.G_close_cell();
    gl.G_set_window(ch); // return original region
  }
  
  public void t_Cellstats_G_find_cell_stat() {

    for( int i=1; i < 8; i++ )
      try {
	System.out.print( i+"->"+cs.G_find_cell_stat( i )+" " );
      } catch (GRASSException e) {
	System.out.println( e );
      }
  }

  Categories cat;

  public void t_Categories_G_init_cats() {
    cat = new Categories( 8, "Empty Categories" );
    System.out.println( cat );
  }

  public void t_Categories_G_set_cats_title() {
    cat.G_set_cats_title("New Title");
    System.out.println( cat );
  }

  public void t_Categories_G_set_cat() {
    int n = 4;
    cat.G_set_cat( n, "Newly set Label for cat "+n);
    System.out.println( "Category "+n+" Label='"+cat.G_get_cat( n )+
			"' in "+cat );
  }

  public void t_gis_G_read_cats() throws GRASSException {
    System.err.println("Before reading cats");
    cat = gl.G_read_cats( layer, mapset );
    System.out.println( cat );
  }
  
  public void t_STATSCATS_ENUMERATION() {

    cs.G_rewind_cell_stats();

    System.out.println("Histogram:");
    Cellstats.Pair Pair;
    while( (Pair = cs.G_next_cell_stat()) != null ) 
    
      System.out.println( Pair.cat+"->"+Pair.count+
			" ("+cat.G_get_cat( Pair.cat )+")" 
			  );

  }
  
  String sitename = "bugsites";
  FILE fs;

  public void t_gis_G_find_sites() throws GRASSException {
    System.out.println("Sites "+sitename+" found in mapset "+
		       gl.G_find_sites( sitename, "" ) );
  }
  
  public void t_gis_G_fopen_sites_old() throws GRASSException {
    fs = gl.G_fopen_sites_old( sitename, mapset );
  }

  public void t_FILE_G_get_site() {
    Site s;
    int i=0;
    while ( ((s = fs.G_get_site()) != null) && (i<20) ) 
      System.out.println( ++i+": "+s );
  }

  String vname = "railroads";

  public void t_gis_G_find_vector() throws GRASSException {
    System.out.println("Vector file "+vname+
		       " found in mapset "+gl.G_find_vector( vname, "" )
		       );
  }
  
  libvect lv;

  public void t_vect_NEW() {
    lv = new libvect( gl );
  }

  Mapinfo mi;

  public void t_vect_Vect_open_old() throws GRASSException {
    mi = lv.Vect_open_old( vname, mapset );
    System.err.println( mi );
  }

  linepnts lp = null;
  
  public void t_READING_VECT() throws GRASSException {
    int i=0;
    while ( ((lp = mi.Vect_read_next_line()) != null) && (i<20) ) 
      System.out.println( ++i+": "+lp );
  }

  public void t_Mapinfo_Vect_rewind() throws GRASSException {
    mi.Vect_rewind();
  }

  public void t_Mapinfo_Vect_set_constraint_type() throws GRASSException {
    mi.Vect_set_constraint_type( linepnts.AREA );
    t_READING_VECT();
  }

  public void t_Mapinfo_Vect_remove_constraints() throws GRASSException {
    mi.Vect_remove_constraints();
    mi.Vect_rewind();
    t_READING_VECT();
  }
    /*    
    public void t_Mapinfo_Vect_set_constraint_region() throws GRASSException {

	// set some just a little bit meaningfull coordinates
	double sn = (ch.north() - ch.south())/10;
	double ew = (ch.east() - ch.west())/10;

	mi.Vect_set_constraint_region( ch.north() - sn,
				       ch.south() + sn,
				       ch.east()  - ew,
				       ch.west()  + ew  );
	System.out.println("Constrainnts set");
	t_READING_VECT();
	mi.Vect_remove_constraints();
	mi.Vect_rewind();
    }
    */
  public void t_linepnts_Vect_copy_pnts_to_xy() throws GRASSException {
    mi.Vect_rewind();
    lp = mi.Vect_read_next_line();
    int np = lp.n_points();
    double[] x = new double[ np ];
    double[] y = new double[ np ];
    System.out.println( "Cootdinate pairs for "+lp );
    lp.Vect_copy_pnts_to_xy( x, y );
    for (int i=0; i<np; i++) 
      System.out.println( i+": x="+x[i]+" y="+y[i] );
  }

  public void t_linepnts_Vect_copy_xy_to_pnts() throws GRASSException {
    double x[] = {1, 2, 3, 4, 5};
    double y[] = {5, 2, 3, 1, 4};
    lp = new linepnts();
    lp.Vect_copy_xy_to_pnts( x, y );
    System.out.println( "Cootdinate pairs for "+lp );
    lp.Vect_copy_pnts_to_xy( x, y );
    int np = lp.n_points();
    for (int i=0; i<np; i++) 
      System.out.println( i+": x="+x[i]+" y="+y[i] );
  }

  String lev2name = "fields";
  //String lev2name = "railroads";
  Mapinfo mi2;

  public void t_Mapinfo_V2_line() throws GRASSException {
    mi2 = lv.Vect_open_old( lev2name, mapset );
    System.out.println( mi2 );
    int n = mi2.V2_num_lines();
    System.out.println("Number of lines: "+n);
    for (int i=0; i<=n && i<20; i++) 
      System.out.println( "Line "+i+": "+mi2.V2_read_line(i)+
			  " cat="+mi2.V2_line_att(i) );
  }

  public void t_Mapinfo_V2_area() throws GRASSException {
    int n = mi2.V2_num_areas();
    System.out.println("Number of lines: "+n);
    for (int i=0; i<=n && i<20; i++) 
      System.out.println( "Area "+i+": "+mi2.Vect_get_area_points(i)+
			  " cat="+mi2.V2_area_att(i) );
  }
  
  public void t_gis_G_read_vector_cats() throws GRASSException {
    gl.G_read_vector_cats( lev2name, mapset );
    for (int i=0; i<cat.num() && i<20; i++)
      System.out.println( "Cat "+i+"="+cat.G_get_cat(i) );
  }

  /*
  
  public void t_() {
  }

  */
  
}







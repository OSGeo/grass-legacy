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
package GRASS;

import java.io.*;
import java.util.*;

/**
 * This class wraps functions of GIS library of GRASS.  Programs must
 * use this class first to access the GRASS database.  It provides
 * loading of the shared object that contains original GRASS routines
 * and initiats connection to specified GRASS database.  This class
 * wraps original GRASS functions in the most simple and
 * straightforward manner and does not provide any object-oriented
 * abstraction in terms of GIS.
 *
 * <BR><B>Side Effect:</I> This class executes
 * <tt>System.runFinalizersOnExit(true)</tt> on initialization. You
 * can switch back after calling constructor of this class but it is
 * not safe.
 *
 * @author  $Author$
 * @version $Revision$
 *
 * @see GRASS.libfun
 *
 */
public class libgis {

  /**
   * String that handles application name.
   * To be removed in future versions.
   */
  protected String appname;
  
  /**
   * String that handles value of GISBASE environmental variable.
   * To be removed in future versions.
   */
  protected String gisbase;
  
  /**
   * String that handles value of GISDBASE environmental variable.
   * To be removed in future versions.
   */
  protected String gisdbase;
  
  /**
   * String that handles value of LOCATION_NAME environmental variable.
   * To be removed in future versions.
   */
  protected String location;
  
  /**
   * String that handles value of MAPSET environmental variable.
   * To be removed in future versions.
   */
  protected String mapset;
  
  /**
   * String that handles value of GISRC environmental variable.
   * To be removed in future versions.
   */
  protected String gisrc;
  
  /**
   * True if a need to delete GISRC file exists.
   */
  private boolean delete_rc;

  /** 
   * True if operations to finish working with libgis were not
   * done.
   */
  private boolean open = false;

  /**
   * Initializes GIS library for stand-alone applications (<I>i.e.</I>
   * ones not running from beyond GRASS). Writes rc file with
   * application name in user's home directory, sets environmental
   * variables, calls GIS library initialization routine.
   * 
   * @param appname  Application name, one that can be obtained in C
   *                 from argv[0]
   *
   * @param gisbase  Location of GRASS executable files. Same as value 
   *                 of GISBASE environmental variable while running GRASS.
   *
   * @param gisdbase Full path to GRASS database. Same as value 
   *                 of GISDBASE environmental variable while running GRASS. 
   *
   * @param location Location name in GRASS database. Same as value 
   *                 of LOCATION_NAME environmental variable while running 
   *                 GRASS. 
   *
   * @param mapset   Mapset name in GRASS database under specified location. 
   *                 Same as value of MAPSET environmental variable while 
   *                 running GRASS.
   *
   * @exception GRASS.GRASSException Exception with an appropriate message
   *            is thrown in cases when constructor fails to create/write 
   *            rc file, set environmental variables or intialize GRASS 
   *            library
   *
   */
  public libgis( 
    String appname, 
    String gisbase, 
    String gisdbase, 
    String location,
    String mapset
  ) throws GRASSException {
  
    this.appname  = appname;
    this.gisbase  = gisbase;
    this.gisdbase = gisdbase;
    this.location = location;
    this.mapset   = mapset;

    // writing GISRC
    gisrc = System.getProperty("user.home")+"/."+appname+".rc";
    try {
      // here must be check for existemce of rc file and of the process
      PrintWriter pw = new PrintWriter(new FileWriter( gisrc ) );
      pw.println("GISDBASE: "      + gisdbase);
      pw.println("LOCATION_NAME: " + location);
      pw.println("MAPSET: "        + mapset);
      pw.println("\n");
      pw.close();
      delete_rc = true;
    } catch (IOException e) {
      throw new GRASSException( e.toString() );
    }
    
    if ( 
      putenv("GISRC="+gisrc) != 0 ||
      putenv("GISBASE="+gisbase) != 0
    ) throw new GRASSException("Unable to set environmental vars");
    
    G_gisinit(appname);
    open = true;
  }
  
  /**
   * Initializes GIS library for applications that run under GRASS. 
   * Assumes that .grassrc file exists and appropriate environmental
   * variables are set by GRASS initialization script.
   *
   * @param appname  Application name, one that can be obtained in C
   *                 from argv[0]
   *
   * @exception GRASS.GRASSException Always thrown with the message 
   *            "Constructor not Implemented"
   * 
   */
  public libgis(String appname) throws GRASSException {
    delete_rc = false;
    G_gisinit(appname);
    open = true;

    this.appname  = appname;
    this.gisbase  = G_gisbase();
    this.gisdbase = G_gisdbase();
    this.location = G_location();
    this.mapset   = G_mapset();

  }
  // Method was not tested

  // supplementary native methods //////////////////////////////////////////
  /**
   * Changes or adds value to environment. Accepts 
   * argument in form "name=value". Not intended for calling by any
   * methods except for members of this class. Wraps C stdlib 
   * function:
   *
   * <pre>
   * int putenv(const char *string);
   * </pre>
   *
   * @param str String of the form "name=value" 
   *
   * @return non-zero value if operation failed, otherwise zero
   *
   */
  protected native int putenv(String str);

  /**
   * Returns the process ID of the process. Wraps C function: 
   *
   * <pre>
   * pid_t getpid(void);
   * </pre>
   *
   * @return process ID
   *
   */
  public native int getpid();

  /**
   * Returns the name of GISRC file created by this class.
   *
   * @return GISRC file name
   *
   */
  public String getGISRC() {
    return gisrc;
  }

  // 12.2 Library initialization ///////////////////////////////////
  /**
   * Initializes GRASS library. Must be never called by any methods 
   * other than constructors of this class. Requires certain checkups 
   * to be done in the system before calling. Wraps GRASS library 
   * routine:
   *
   * <pre>
   * G_gisinit (program_name)
   *           char *program_name;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.2 <I>Library initialization</I>
   * 
   * @param program_name Name of the program that uses GRASS library
   *
   */
  protected native void G_gisinit(String program_name);
  
  // 12.3 Diagnostic messages and errors ///////////////////////////////////
  /**
   * Sets/unsets pause on errors. Wraps GRASS library 
   * routine:
   *
   * <pre>
   * G_sleep_on_error (flag)  
   *               int flag;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.3 <I>Diagnostic messages</I>
   *
   * @param flag Pause flag:  false -- no pause on errors; true -- 
   *             pause will occur.
   *
   */
  public native void G_sleep_on_error(boolean flag);
  
  /**
   * Sets/unsets reporting warnings. Wraps GRASS library 
   * routine:
   *
   * <pre>
   * G_suppress_warnings (flag)  
   *               int flag;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.3 <I>Diagnostic messages</I>
   *
   * @param flag Warnings flag. false -- print warning messages, true -- 
   *             suppress warnings.
   *
   */
  public native void G_suppress_warnings(boolean flag);

  // 12.4 Environment and database information /////////////////////////////
  /**
   * Returns the name of the current database location. Wraps GRASS library 
   * routine:
   *
   * <pre>
   * char * 
   * G_location () 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.4 <I>Environment and database information</I>
   *
   * @return The name of the current database location.
   *
   */
  public native String G_location();
  
  /**
   * Returns the name of the current mapset in current location. Wraps 
   * GRASS library routine:
   *
   * <pre>
   * char * 
   * G_mapset () 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.4 <I>Environment and database information</I>
   *
   * @return The name of the current mapset in current location.
   *
   */
  public native String G_mapset();
  
  /**
   * Returns a one line title for the database location. Wraps 
   * GRASS library routine:
   *
   * <pre>
   * char * 
   * G_myname () 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.4 <I>Environment and database information</I>
   *
   * @return One line title for the database location.
   *
   */
  public native String G_myname();
  
  /**
   * Returns the full path name of the top level directory for 
   * GRASS programs. Wraps GRASS library routine:
   *
   * <pre>
   * char * 
   * G_gisbase () 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.4 <I>Environment and database information</I>
   *
   * @return Full path name of the top level directory for GRASS programs. 
   *
   */
  public native String G_gisbase();
  
  /**
   * Returns the full UNIX path name of the directory which holds the 
   * database locations. Wraps GRASS library routine:
   *
   * <pre>
   * char * 
   * G_gisdbase () 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.4 <I>Environment and database information</I>
   *
   * @return Full UNIX path name of the directory which holds the 
   *         database locations.
   *
   */
  public native String G_gisdbase();
  
  /**
   * Returns the full UNIX path name of the current database location. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * char * 
   * G_location_path () 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.4 <I>Environment and database information</I>
   *
   * @return Full UNIX path name of the current database location.
   *
   */
  public native String G_location_path();
  
  // 12.7.1 The Database Region //////////////////////////////////////
  /**
   * Reads the database region as stored in the WIND file in the
   * user's current mapset.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_get_window (region)
   *    struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.1 <I>The Database Region</I>
   *
   * @return Database region.
   *
   */
  public native Cellhead G_get_window();
  
  /**
   * Reads the default region for the location.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_get_default_window (region)
   *     struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.1 <I>The Database Region</I>
   *
   * @return Default region.
   *
   */
  public native Cellhead G_get_default_window();
  
  // 12.7.2 The Active Program Region //////////////////////////////////////
  /**
   * Returns the number of rows in the active program region.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_window_rows () 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.2 <I>The Active Program Region</I>
   *
   * @return Number of rows in the active program region.
   *
   */
  public native int G_window_rows();
  
  /**
   * Returns the number of columns in the active program region.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_window_cols () 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.2 <I>The Active Program Region</I>
   *
   * @return Number of columns in the active program region.
   *
   */
  public native int G_window_cols();
  
  /**
   * Sets the active region using parameters in Cellhead.
   * Somewhat equivalent to GRASS library routine:
   *
   * <pre>
   * G_set_window (region)
   *        struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.2 <I>The Active Program Region</I>
   *
   * @param ch Cellhead to use parameters to set region to.
   *
   * @exception GRASSException Exception with an appropriate 
   *            message is thrown in cases when provided
   *            region is not valid.
   * 
   */
  public native void G_set_window(Cellhead ch) throws GRASSException;
  
  /**
   * Writes the database region file (WIND) in the user's current mapset
   * from cellhead.
   * Somewhat equivalent to GRASS library routine:
   *
   * <pre>
   * G_put_window (region)
   *        struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.1 <I>The Active DatabaseRegion</I>
   *
   * @param ch Cellhead to use parameters to set region to.
   *
   * @exception GRASSException Exception with an appropriate 
   *            message is thrown in cases when provided
   *            region is not valid.
   * 
   */
  public native void G_put_window(Cellhead ch) throws GRASSException;
  
  /**
   * Gets the values of currently active region into a new Cellhead object.  
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_get_set_window (region)
   *        struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.2 <I>The Active Program Region</I>
   *
   * @return Cellhead whose values are set.
   *
   */
  public native Cellhead G_get_set_window();
  
  // 12.8 Raster Area Calculations
  /**
   * This routine must be called once before any call to
   * G_area_of_cell_at_row. It can be used in either planimetric
   * projections or the latitude-longitude projection. It returns 2 if
   * the projection is latitude-longitude, 1 if the projection is
   * planimetric, and 0 of the projection doesn't hav e a metric
   * (e.g. imagery.) If the return value is 1 or 0, all the grid cells
   * in the map have the same area. Otherwise the area of a grid cell
   * varies with the row.
   * Wraps GRASS library routine:
   *
   * <pre>
   *   G_begin_cell_area_calculations ()
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.8 <I>Raster Area Calculations</I>
   *
   * @return 2 if the projection is latitude-longitude, 1 if the
   *         projection is planimetric, and 0 of the projection doesn't have
   *         a metric (e.g. imagery.)
   *
   */
  public native int G_begin_cell_area_calculations();
  
  /**
   * This routine returns the area in square meters of a cell in the
   * specified row. This value is constant for planimetric grids and
   * varies with the row if the projection is latitude-longitude.
   * Wraps GRASS library routine:
   *
   * <pre>
   * double
   * G_area_of_cell_at_row (row)
   *    int row ;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.8 <I>Raster Area Calculations</I>
   *
   * @param row row to return area at
   *
   * @return area in square meters of a cell in the
   *         specified row
   *
   */
  public native double G_area_of_cell_at_row(int row);
  
  // 12.9.2 Finding Raster File in Database ////////////////////////////////
  /**
   * Looks for raster file name in the database. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * char*
   * G_find_cell (name, mapset)
   *      char *name;
   *      char *mapset;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.2 <I>Finding Raster File in Database</I>
   *
   * @param name   Raster file name to search for.
   *
   * @param mapset Name of mapset in which raster should reside. Can be empty 
   *               string which means search all the mapsets in the user's 
   *               current mapset search path.
   *
   * @return mapset where raster file resides
   *
   * @exception GRASSException if raster file was not found
   *
   */
  public native String G_find_cell(String name, String mapset)
      throws GRASSException;

  // 12.9.3 Opening an Existing Raster File ////////////////////////////////
  /**
   * Opens existing raster file. Wraps GRASS library routine:
   *
   * <pre>
   * G_open_cell_old (name, mapset)
   *      char *name;
   *      char *mapset;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.3 <I>Opening an Existing Raster File</I>
   *
   * @param name   Raster file name to open.
   *
   * @param mapset Name of mapset in which raster should reside. It can be 
   *               obtained by calling G_find_cell.
   *
   * @return file descriptor of the open raster
   *
   * @exception GRASSException is thrown if an error was returned by the
   *            GRASS routine
   *
   */
  public native FD G_open_cell_old(String name, String mapset)
    throws GRASSException;

  /**
   * Opens a new raster file. Wraps GRASS library routine:
   *
   * <pre>
   * G_open_cell_new (name, mapset)
   *      char *name;
   *      char *mapset;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.4 <I>Creating and Opening Opening Raster Files</I>
   *
   * @param name   Raster file name to open.
   *
   * @return file descriptor of the open raster
   *
   * @exception GRASSException is thrown if an error was returned by the
   *            GRASS routine
   *
   */
  public native FD G_open_cell_new( String name ) 
    throws GRASSException;

  /**
   * Opens a new raster file for random writes by G_put_mpa_row.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_open_cell_new_random (name, mapset)
   *      char *name;
   *      char *mapset;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.4 <I>Creating and Opening Opening Raster Files</I>
   *
   * @param name   Raster file name to open.
   *
   * @return file descriptor of the open raster or negative value if error
   *         occured
   *
   * @exception GRASSException is thrown if an error was returned by the
   *            GRASS routine
   *
   */
  public native FD G_open_cell_new_random( String name )
    throws GRASSException;

  /**
   * The category file for raster file name in mapset is read into the
   * this object. Wraps GRASS library routine:
   *
   * <pre>
   * G_read_cats (name, mapset, cats)
   *     char *name;
   *     char *mapset;
   *     struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.10.2.1.</I> Reading and Writing the Raster Category File
   *
   * @param name raster layer name
   *
   * @param mapset mapset name
   *   
   * @return new instance of Categories 
   *   
   * @exception GRASSException is thrown with an appropriate message if 
   * there was an error reading category file
   *
   */
  public native Categories G_read_cats(String name, String mapset)
    throws GRASSException;

  /**
   * The category file for vector file name in mapset is read into
   * the new Categories object.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_read_vector_cats (name, mapset, cats)
   *     char *name;
   *     char *mapset;
   *     struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.11.6</I> Vector Category File
   *
   * @param name vector layer name
   *
   * @param mapset mapset name
   *
   * @return new instance of Categories 
   *   
   * @exception GRASSException is thrown with an appropriate message if 
   * there was an error reading category file
   *
   */
  public native Categories G_read_vector_cats(String name, String mapset)
    throws GRASSException;

  /**
   * Looks for stes list name in the database. 
   * This function is not documented in <B>GRASS 4.2 Programmer's Manual</B>.
   *
   * @param name   Site list name to search for.
   *
   * @param mapset Name of mapset in which sitelist should reside. 
   *               Can be empty 
   *               string which means search all the mapsets in the user's 
   *               current mapset search path.
   *
   * @return mapset where site list file resides
   *
   * @exception GRASSException if raster file was not found
   *
   */
  public native String G_find_sites(String name, String mapset)
      throws GRASSException;

  // 12.12.2 Site List File
  /**
   * Creates an empty site list file name in the current mapset and
   * opens it for writing.  Wraps GRASS library routine:
   *
   * <pre>
   * FILE* 
   * G_fopen_sites_new (name) 
   *       char *name;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.12.2 <I>Opening Site List Files</I>
   *
   * @param name GRASS site list file name
   *
   * @return A new GRASS.FD object to access site list
   *
   * @exception GRASSException Exception with an appropriate error message 
   *            is thrown if some error occured while opening site list
   *            file.
   *
   */
  public native FILE G_fopen_sites_new(String name) throws GRASSException;
  // Method was never tested!

  /**
   * Opens the site list file name in mapset for reading. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * FILE *
   * G_fopen_sites_old (name, mapset)
   *       char *name;
   *       char *mapset;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.12.2 <I>Opening Site List Files</I>
   *
   * @param name GRASS site list file name
   *
   * @param mapset Name of mapset where site list file should reside
   *
   * @return A new GRASS.FD object to access site list
   *
   * @exception GRASSException Exception with an appropriate error message 
   *            is thrown if some error occured while opening site list
   *            file.
   *
   */
  public native FILE G_fopen_sites_old(String name, String mapset)
    throws GRASSException;

  // 12.11.2 Finding Vector Files in Database ////////////////////////////////
  /**
   * Looks for vector file name in the database. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * char*
   * G_find_vector (name, mapset)
   *      char *name;
   *      char *mapset;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.11.2 <I>Finding Vector File in Database</I>
   *
   * @param name   Vector file name to search for.
   *
   * @param mapset Name of mapset in which vector should reside. Can be empty 
   *               string which means search all the mapsets in the user's 
   *               current mapset search path.
   *
   * @return mapset where vector file resides
   *
   * @exception GRASSException if vector file was not found
   *
   */
  public native String G_find_vector(String name, String mapset)
      throws GRASSException;

  // 12.7.3 Projection information ////////////////////////////////
  /**
   * This routine returns a code indicating the projection for the
   * active region. The current values are: 
   * <TABLE>
   * <TR><TH>Code<TH>Projection
   * <TR><TD>0<TD>unreferenced x,y (imagery data)
   * <TR><TD>1<TD>UTM
   * <TR><TD>2<TD>State Plane
   * <TR><TD>3<TD>Latitude-Longitude
   * </TABLE>
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_projection ( )
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.3 <I>Projection information</I>
   *
   * @return code indicating the projection for the active region
   *
   */
  public native int G_projection();

  /**
   * Returns a string which is a printable name for projection in active
   * region.
   * Wraps GRASS library routine:
   *
   * <pre>
   * char*
   * G_database_projection_name ()
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.3 <I>Projection information</I>
   *
   * @return name for projection in active region
   *
   */
  public native String G_database_projection_name();

  /**
   * Returns a string describing the database grid units.
   * Wraps GRASS library routine:
   *
   * <pre>
   * char *
   * G_database_unit_name (plural)
   *    int plural
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.3 <I>Projection information</I>
   *
   * @param plural return a plural form (eg. feet) if true.
   *
   * @return database grid units
   *
   */
  public native String G_database_unit_name(boolean plural);

  /**
   * Returns a factor which converts the grid unit to meters (by
   * multiplication).  If the database is not metric (eg. imagery)
   * then 0.0 is returned.
   * Wraps GRASS library routine:
   *
   * <pre>
   * double
   *     G_database_units_to_meters_factor ()
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.3 <I>Projection information</I>
   *
   * @return factor which converts the grid unit to meters
   *
   */
  public native double G_database_units_to_meters_factor();

  /**
   * This routine returns the zone for the active region. The meaning
   * for the zone depends on the projection.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_zone ()
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.3 <I>Projection information</I>
   *
   * @return projection zone for the active region
   *
   */
  public native int G_zone();

  /**
   * Deletes rc file if one was created.
   */
  public void close() throws GRASSException {

    if ( open && delete_rc) {
      (new File( gisrc )).delete();
      open = false;
    }

  }

  // supplementary java methods ////////////////////////////////////////////
  /**
   * Deletes rc file if one was created and libgis was left open..
   *
   * @exception Throwable IOException is thrown if there is a problem
   *            deleting rc file
   */
  protected void finalize() throws Throwable {
    
    if (open) close();
    super.finalize();

  }

  /**
   * Loads libgis.so. libgis.so must be somewhere on LD_LIBRARY_PATH.
   */
  static {
    System.loadLibrary("gis");
    System.runFinalizersOnExit(true);
  }
  
}



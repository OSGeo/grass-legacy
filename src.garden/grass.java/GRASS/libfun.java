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
 * Class that provides methods to imitate behaviour of some of GRASS commands. 
 * It is recommended to avoid ever using this class.
 *
 * @author  $Author$
 * @version $Revision$
 *
 * @see GRASS.libgis
 *
 */
public class libfun {

  /**
   * Pointer to GIS library object.
   *
   * @see GRASS.libgis
   */
  protected libgis lg;
  
  /**
   * Type of GRASS file raster.
   */
  public static final int TYPE_RAST  = 0x1000; 
  
  /**
   * Type of GRASS file vector.
   */
  public static final int TYPE_VECT  = 0x1001; 
  
  /**
   * Type of GRASS file sites.
   */
  public static final int TYPE_SITES = 0x1002; 
  
  /**
   * Intializes this object by saving pointer to GRASS library object.
   *
   * @param lg Previously initialized GRASS library
   *
   * @see GRASS.libgis
   *
   */
  public libfun( libgis lg ) {
    this.lg = lg;
    
    // code to read $GISBASE/etc/element_list into rast_dirs
  }
  
  /**
   * List of elements (directories names) for raster layers
   * <I>(list may be incomplete)
   * better if they are read from $GISBASE/etc/element_list)</I>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 4.5 <I>Mapset Structure</I>
   */
  protected static String rast_dirs[]   = 
    { "cell", "cellhd", "cats", "colr", "colr2", "cell_misc", "hist" };
  
  /**
   * List of elements (directories names) for vector layers
   * <I>(list may be incomplete)
   * better if they are read from $GISBASE/etc/element_list)</I>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 4.5 <I>Mapset Structure</I>
   */
  protected static String vect_dirs[]   = 
    { "dig", "dig_ascii", "dig_att", "dig_cats", "dig_plus", "reg" };
  
  /**
   * List of elements (directories names) for site lists
   * <I>(list may be incomplete,
   * better if they are read from $GISBASE/etc/element_list)</I>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 4.5 <I>Mapset Structure</I>
   */
  protected static String sites_dirs[]  = 
    { "site_lists" };
    
  /**
   * Attempts to emulate behaviour of g.list command in a way that its
   * output can be useful in Java applications. Mostly intended for providing
   * list of file names for user interface elements. This method directly
   * checks content of directories under GRASS database locations with 
   * respect to mapset search path. There is no certainty that algorithm
   * used provides correct results.
   *
   * @param type types of GRASS files to list (rasters, vectors, sites)
   *
   * @return Enumeration of unique names of GRASS files 
   *
   * @exception GRASSException Exception with an appropriate error message
   *            is thrown in case (1) of unpredictable situation and (2)
   *            I/O problem
   *
   */
  public Enumeration g_list(int type) throws GRASSException {
  
    String dirs[] = null;
    switch (type) {
      case (TYPE_RAST):  dirs = rast_dirs; break;
      case (TYPE_VECT):  dirs = vect_dirs; break;
      case (TYPE_SITES): dirs = sites_dirs; break;
      default : throw new GRASSException("Unknown data type in g_list: "+type);
    }
    
    String gisdbase = lg.G_gisdbase();
    String location = lg.G_location();
    String mapset   = lg.G_mapset();
    
    // creating af a mapset search path
    // better to read at the intialization stage
    Vector mapsets = new Vector( 5 );
    File spfile = new File( gisdbase +"/"+location+"/"+mapset, "SEARCH_PATH" );
    if ( spfile.exists() ) { // reading from search path file
      try {
        BufferedReader in = new BufferedReader(new FileReader( spfile ));
        String inp;
      
        while ((inp = in.readLine()) != null) mapsets.addElement( inp.trim() );
      
        in.close();
      } catch (IOException e) {
        throw new GRASSException( "Internal: "+e+" while building searchpath. May be Ok.");
      }
    } else {
      // default values if no SEARCH_PATH file found
      mapsets.addElement( mapset );
      mapsets.addElement( "PERMANENT" );
    }
    
    Hashtable name_list = new Hashtable();
    for (Enumeration s = mapsets.elements(); s.hasMoreElements(); ) {
      String mapset0 = (String)s.nextElement();
      for (int i=0; i < dirs.length; i++ ) {
        File file = new File( gisdbase +"/"+location+"/"+mapset0, dirs[i] );
	//System.err.println( file );
        if ( !file.exists() ) continue;
        String[] files = file.list();
        for (int j=0; j < files.length; j++) {
	  name_list.put( files[j], "" );
	  //System.err.println( "\t"+files[j] );
	}
      }
    }
    
    return name_list.keys();
  }

}



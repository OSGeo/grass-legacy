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

// 12.20.3 struct Colors //////////////////////////////////////
/**
 * Class to mimic behaviour of original struct Colors from gis.h
 * It holds red, green, blue intensities for raster categories.
 *
 * See the <B>GRASS 4.2 Programmer's Manual</B>, 
 * Section 12.20.3 <I>struct Colors</I>
 * 
 * @author  $Author$
 * @version $Revision$
 *
 * @see GRASS.libgis
 *
 */
public class Colors {

  /**
   * Handles memory location of the actual GRASS struct Colors.
   */
  private long addr = 0;
  
  /**
   * Constructs an empty object and allocates memory for GRASS struct Colors.
   */
  public Colors() {
    allocate();
    G_init_colors();
  }

  /**
   * Reads and creates color table for the raster or vector file in the
   * specified mapset. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_read_colors (name, mapset, colors)
   * char *name;
   * char *mapset;
   * struct Colors *colors;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.3.1 <I>Reading and Writing Raster Color File</I>
   *
   * @param name GRASS raster or vector file name
   *
   * @param mapset Name of mapset where raster or vector file should reside
   *
   * @exception GRASSException Exception with an appropriate error message 
   *            is thrown if some error occured while reading color
   *            table.
   *
   */
  public Colors(String name, String mapset) throws GRASSException {
    allocate();
    G_read_colors( name, mapset );
  }

  protected native void G_init_colors();
  
  // 12.10.3.1 Reading [not writing] Raster color file /////////////////////
  /**
   * Reads and creates color table for the raster or vector file in
   * the specified mapset. Not intended to be called by methods
   * outside the members of this class. Wraps GRASS library routine:
   *
   * <pre>
   * G_read_colors (name, mapset, colors)
   * char *name;
   * char *mapset;
   * struct Colors *colors;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.3.1 <I>Reading and Writing Raster Color File</I>
   *
   * @param name GRASS raster or vector file name
   *
   * @param mapset Name of mapset where raster or vector file should reside
   *
   * @return Object GRASS.Colors that contains color table of the
   *         specified raster or vector file
   *
   * @exception GRASSException Exception with an appropriate error message 
   *            is thrown if some error occured while reading color
   *            table.
   *
   */
  protected native void G_read_colors( String name, String mapset ) 
    throws GRASSException;

  /**
   * Used by other methods to allocate memory for GRASS struct Colors.
   * Must be never called from outside of methods of this class.
   */
  protected native void allocate();
  
  // 12.10.3.2 Lookup Raster Colors /////////////////////////////
  /**
   * Extracts color associated with category value. Wraps GRASS library 
   * routine:
   *
   * <pre>
   * G_get_color (cat, red, green, blue, colors)
   *    CELL cat; 
   *    int *red; 
   *    int *green; 
   *    int *blue; 
   *    struct Colors *colors;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.3.2 <I>Lookup Up Raster Colors</I>
   * 
   * @param cat category value
   *
   * @return Color associated with provided category value
   *
   * @see java.awt.Color
   *
   */
  public native java.awt.Color G_get_color(int cat);

  /**
   * Extracts color associated with category value. Wraps GRASS library 
   * routine:
   *
   * <pre>
   * G_get_color (cat, red, green, blue, colors)
   *    CELL cat; 
   *    int *red; 
   *    int *green; 
   *    int *blue; 
   *    struct Colors *colors;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.3.2 <I>Lookup Up Raster Colors</I>
   * 
   * @param cat category value
   *
   * @return Color associated with provided category value as an integer 
   *         value
   *
   * @see java.awt.Color
   *
   */
  public native int intColor(int cat);

  // for Internal use
  /**
   * Inquires for memory address of class's GRASS struct Colors.
   * Must be never called from outside of methods of this class.
   *
   * @return memory address of GRASS struct Colors
   *
   */
  protected long getAddr() {
    return addr;
  }
  
  // Cleaning after
  /**
   * Used by other methods to free memory of GRASS struct Colors.
   * Must be never called from outside of methods of this class.
   */
  protected native void G_free_colors();
  
  /**
   * Deallocates memory for GRASS struct Colors and calls finalize
   * method of the superclass.
   * Must be never called from outside of methods of this class.
   *
   * @exception Throwable if thrown by superclass finalizer
   */
  protected void finalize() throws Throwable {
    
    G_free_colors(); //explicitly release memory
    super.finalize();

  }
  
  /**
   * Returns a string representation of the object.
   * 
   * @return String that represents this object (only its location in memory)
   *
   */
  public String toString() {
    return getClass()+" at addr="+addr;
  }

}




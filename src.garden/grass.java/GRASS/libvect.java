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

/**
 * This class is intended to wrap functionality in Vector library of GRASS.
 *
 * @author  $Author$
 * @version $Revision$
 *
 * @see GRASS.libgis
 *
 */
public class libvect {

  /**
   * Pointer to GIS library object.
   *
   * @see GRASS.libgis
   */
  protected libgis lg;
  
  /**
   * Intializes this object by saving pointer to GRASS library object.
   * Loads libvect.so. libvect.so must be somewhere on LD_LIBRARY_PATH. 
   *
   * @param lg Previously initialized GRASS library
   *
   * @see GRASS.libgis
   *
   */
  public libvect( libgis lg ) {
    this.lg = lg;
  }

  /**
   * Opens the vector map name in mapset for reading.
   * 
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_open_old (Map, name, mapset)
   *    struct Map_info *Map;
   *    char *name, *mapset;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.3.</I> Opening and closing vector maps 
   *
   * @param name existing map name
   *
   * @param mapset mapset name where the map resides
   *
   * @exception GRASSException if an error was returned by the GRASS routine
   *
   */
  public native Mapinfo Vect_open_old(String name, String mapset)
    throws GRASSException;

  /**
   * Opens the vector map name in the current mapset for writing.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_open_new (Map, name)
   *    struct Map_info *Map;
   *    char *name;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.3.</I> Opening and closing vector maps 
   *
   * @param name new map name
   *
   * @exception GRASSException if an error was returned by the GRASS routine
   *
   */
  public native Mapinfo Vect_open_new(String name)
    throws GRASSException;

  /**
   * Allows you to specify at which level the map is to be opened.
   * It is recommended that it only be used to force opening at level one(1).
   * 
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_set_open_level (level)
   *    int level;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.3.</I> Opening and closing vector maps 
   *
   * @param level level to set
   *
   */
  public native void Vect_set_open_level( int level );
  
  /**
   * Loads libvect.so. libvect.so must be somewhere on LD_LIBRARY_PATH.
   */
  static {
    System.loadLibrary("vect");
  }
  
}



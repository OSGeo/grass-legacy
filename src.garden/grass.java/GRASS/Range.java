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
 * This class wraps GRASS library struct Range. 
 *
 * See the <B>GRASS 4.2 Programmer's Manual</B>, 
 * Section 12.20.5 <I>struct Range</I>
 * 
 * @author  $Author$
 * @version $Revision$
 *
 * @see GRASS.libgis
 *
 */
public class Range {

  /**
   * Handles memory location of the actual struct Range.
   */
  private long addr = 0;

  /**
   * Constructs an empty object and allocates memory for GRASS struct Range.
   */
  public Range() {
    allocate();
    G_init_range();
  }
  
  // 12.10.5 Raster Range File
  /**
   * Reads and creates range object for the raster file in the specified
   * mapset. Wraps GRASS library routine:
   *
   * <pre>
   * G_read_range (name, mapset, range)
   * char *name;
   * char *mapset;
   * struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   * @param name GRASS raster file name
   *
   * @param mapset Name of mapset where raster file should reside
   *
   * @return Object GRASS.Range that contains range information of the
   *         specified raster file
   *
   * @exception GRASSException Exception with an appropriate error message 
   *            is thrown if some error occured while reading raster Range
   *            information.
   *
   */
  public Range(String name, String mapset) throws GRASSException {
    allocate();
    G_read_range(name, mapset);
  }
  
  /**
   * Used by other methods to allocate memory for GRASS struct Range.
   * Must be never called from outside of methods of this class.
   */
  protected native void allocate();
  
  /**
   * Reads the range information for the raster file name in mapset 
   * into the range structure. Wraps GRASS library routine:
   *
   * <pre>
   * G_read_range (name, mapset, range)
   * char *name;
   * char *mapset;
   * struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   * @param name GRASS raster file name
   *
   * @param mapset Name of mapset where raster file should reside
   *
   * @exception GRASSException Exception with an appropriate error message 
   *            is thrown if some error occured while reading raster Range
   *            information.
   */
  protected native void G_read_range(String name, String mapset)
    throws GRASSException;

  /**
   * Writes the range information for the raster file name in the current 
   * mapset from the range structure. Wraps GRASS library routine:
   *
   * <pre>
   * G_write_range (name, range) write raster range file
   *          char *name;
   *  struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   * @param name GRASS raster file name
   *
   * @exception GRASSException Exception with an appropriate error message 
   *            is thrown if some error occured while reading raster Range
   *            information.
   */
  public native void G_write_range(String name) throws GRASSException;
  // this method was never tested !!

  /**
   * Initializes the range structure for updates.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_init_range (range)
   *      struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   */
  public native void G_init_range();

  /**
   * Compares the cat value with the minimum and maximum values in
   * the range structure, modifying the range if cat extends the range.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_update_range (cat, range)
   *            CELL cat;
   *            struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   * @param cat category value
   *
   */
  public native void G_update_range(int cat);

  /**
   * This routine updates the range data just like G_update_range, 
   * but for values from the row of cell values.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_row_update_range (cell, n, range)
   *         CELL *cell;
   *         int n;
   *         struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   * @param buf buffer of cell values
   *
   * @param n number of values to be used for update
   *
   */
  public void G_row_update_range(CELL buf, int n) {
    
    G_row_update_range_( buf.getAddr(), n );

  }

  /**
   * The actual method to update the range data just like G_update_range, 
   * but for values from the row of cell values. Must be never called by
   * any methods other than memebers of this class. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_row_update_range (cell, n, range)
   *         CELL *cell;
   *         int n;
   *         struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   * @param buf buffer of cell values
   *
   * @param n number of values to be used for update
   *
   */
  private native void G_row_update_range_(long addr, int n);

  /**
   * Extracts the minimum value from the range structure.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_get_range_min_max (range, min, max)
   *       struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   * @return the minimum value
   *
   */
  public native int min();

  /**
   * Extracts the maximum value from the range structure.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_get_range_min_max (range, min, max)
   *       struct Range *range;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.5 <I>Raster Range File</I>
   *
   * @return the maximum value
   *
   */
  public native int max();

  // Cleaning after
  /**
   * Used by other methods to free memory of GRASS struct Range.
   * Must be never called from outside of methods of this class.
   */
  protected native void free();
  
  /**
   * Deallocates memory for GRASS struct Range and calls finalize
   * method of the superclass.
   * Must be never called from outside of methods of this class.
   *
   * @exception Throwable if thrown by superclass finalizer
   */
  protected void finalize() throws Throwable {
    
    free(); //explicitly release memory
    super.finalize();

  }
  
  /**
   * Returns a string representation of the object.
   * 
   * @return String that represents this object 
   *
   */
  public String toString() {
    return getClass()+" at addr="+addr+" (min="+min()+", max="+max()+")";
  }

  
}

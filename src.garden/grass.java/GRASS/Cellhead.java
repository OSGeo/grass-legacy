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

// 12.20.1 struct Cell_head //////////////////////////////////////
/**
 * Class to mimic behaviour of struct Cell_head from gis.h.
 * The raster header data structure is used for two purposes.
 * It is used for raster header information for map layers. It
 * also used to hold region values. Wraps GRASS library 
 * struct:
 *
 * <pre>
 * struct Cell_head
 * {
 *     int format;     -- number of bytes per cell --
 *     int compressed; -- compressed(1) or not compressed(0) --
 *     int rows, cols; -- number of rows and columns --
 *     int proj;       -- projection --
 *     int zone;       -- zone --
 *     double ew_res;  -- east-west resolution -- 
 *     double ns_res;  -- north-south resolution --
 *     double north;   -- northern edge --
 *     double south;   -- southern edge --
 *     double east;    -- eastern edge --
 *     double west;    -- western edge --
 * };
 * </pre>
 *
 * See the <B>GRASS 4.2 Programmer's Manual</B>, 
 * Section 12.20.1 <I>struct Cell_head</I>
 *
 * @author  $Author$
 * @version $Revision$
 *
 * @see GRASS.libgis#G_get_set_window
 *
 */
public class Cellhead implements Cloneable {

  /**
   * Stores memory address of class's GRASS struct Colors.
   */
  private long addr = 0;
  
  /**
   * Constructs an empty object and allocates memory for Cellhead structure.
   */
  public Cellhead() {
    allocate();
  }
  
  /**
   * Used by other methods to allocate memory for Cellhead struct.
   * Must never be called from outside the methods of this class.
   */
  private native void allocate();
  
  // accessing/setting variables
  
  /**
   * Returns value of the field 'format' corresponding to the Cellhead.
   *
   * @return value of the field 'format'
   *
   */
  public native int format();
  
  /**
   * Sets value of the field 'format' corresponding to the Cellhead.
   *
   * @param format format value to set to
   *
   */
  public native void setFormat(int format);
  
  /**
   * Returns value of the field 'compressed' corresponding to the Cellhead.
   *
   * @return value of the field 'compressed'
   *
   */
  public native boolean compressed();
  
  /**
   * Sets value of the field 'compressed' corresponding to the Cellhead.
   *
   * @param compressed compressed value to set to
   *
   */
  public native void setCompressed(boolean compressed);
  
  /**
   * Returns value of the field 'rows' corresponding to the Cellhead.
   *
   * @return value of the field 'rows'
   *
   */
  public native int rows();
  
  /**
   * Sets value of the field 'rows' corresponding to the Cellhead.
   *
   * @param rows rows value to set to
   *
   */
  public native void setRows(int rows);
  
  /**
   * Returns value of the field 'cols' corresponding to the Cellhead.
   *
   * @return value of the field 'cols'
   *
   */
  public native int cols();
  
  /**
   * Sets value of the field 'cols' corresponding to the Cellhead.
   *
   * @param cols cols value to set to
   *
   */
  public native void setCols(int cols);
  
  /**
   * Returns value of the field 'proj' corresponding to the Cellhead.
   *
   * @return value of the field 'proj'
   *
   */
  public native int proj();
  
  /**
   * Sets value of the field 'proj' corresponding to the Cellhead.
   *
   * @param proj proj value to set to
   *
   */
  public native void setProj(int proj);
  
  /**
   * Returns value of the field 'zone' corresponding to the Cellhead.
   *
   * @return value of the field 'zone'
   *
   */
  public native int zone();
  
  /**
   * Sets value of the field 'zone' corresponding to the Cellhead.
   *
   * @param zone zone value to set to
   *
   */
  public native void setZone(int zone);
  
  /**
   * Returns value of the field 'ew_res' corresponding to the Cellhead.
   *
   * @return value of the field 'ew_res'
   *
   */
  public native double ew_res();
  
  /**
   * Sets value of the field 'ew_res' corresponding to the Cellhead.
   *
   * @param ew_res ew_res value to set to
   *
   */
  public native void setEw_res(double ew_res);
  
  /**
   * Returns value of the field 'ns_res' corresponding to the Cellhead.
   *
   * @return value of the field 'ns_res'
   *
   */
  public native double ns_res();
  
  /**
   * Sets value of the field 'ns_res' corresponding to the Cellhead.
   *
   * @param ns_res ns_res value to set to
   *
   */
  public native void setNs_res(double ns_res);
  
  /**
   * Returns value of the field 'north' corresponding to the Cellhead.
   *
   * @return value of the field 'north'
   *
   */
  public native double north();
  
  /**
   * Sets value of the field 'north' corresponding to the Cellhead.
   *
   * @param north north value to set to
   *
   */
  public native void setNorth(double north);
  
  /**
   * Returns value of the field 'south' corresponding to the Cellhead.
   *
   * @return value of the field 'south'
   *
   */
  public native double south();
  
  /**
   * Sets value of the field 'south' corresponding to the Cellhead.
   *
   * @param south south value to set to
   *
   */
  public native void setSouth(double south);
  
  /**
   * Returns value of the field 'east' corresponding to the Cellhead.
   *
   * @return value of the field 'east'
   *
   */
  public native double east();
  
  /**
   * Sets value of the field 'east' corresponding to the Cellhead.
   *
   * @param east east value to set to
   *
   */
  public native void setEast(double east);
  
  /**
   * Returns value of the field 'west' corresponding to the Cellhead.
   *
   * @return value of the field 'west'
   *
   */
  public native double west();
  
  /**
   * Sets value of the field 'west' corresponding to the Cellhead.
   *
   * @param west west value to set to
   *
   */
  public native void setWest(double west);
  
  // 12.10.1 Raster header file ////////////////////////////////////////////
  /**
   * This method fills in missing parts of the input cell header 
   * (or region). It also makes projection-specific adjustments. The 
   * cellhd structure must have its north, south, east, west, and proj 
   * fields set. If rflag is true, then the north-south resolution is 
   * computed from the number of rows in the cellhd structure. Otherwise 
   * the number of rows is computed from the north-south resolution in 
   * the structure, and similarly for cflag and the number of columns 
   * and the east-west resolution. Wraps GRASS library routine:
   *
   * <pre>
   * char *
   * G_adjust_Cell_head (cellhd, rflag, cflag) 
   *      struct Cell_head *cellhd;
   *      int rflag, cflag;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.1 <I>Raster Header File</I>
   *
   * @param rflag Flag to adjust north-south resolution from the number 
   *              of rows
   *
   * @param cflag Flag to adjust east-west resolution  from the number
   *              of columns
   *
   * @exception GRASSException Exception with appropriate error message
   *            is thrown if there was an error during execution
   *
   */
  public native void 
    G_adjust_Cell_head( boolean rflag, boolean cflag ) 
    throws GRASSException;
  
  // 12.7.2 The Active Program Region  /////////////////////////////////
  /**
   * Converts a column relative to a region to an easting.
   * Wraps GRASS library routine:
   *
   * <pre>
   * double
   * G_col_to_easting (col, region)
   *     double col;
   *     struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.2 <I>The Active Program Region</I><BR>
   *
   * <B>Note:</B> col+0.5 will return the easting for the
   * center of the column; col+0.0 will return the easting for the
   * western edge of the column; and col+1.0 will return the easting
   * for the eastern edge of the column.
   *
   * @param col column to find easting for
   *
   * @return easting for the specified column
   *
   */
  public native double G_col_to_easting( double col );

  /**
   * Converts a row relative to a region to a northing.
   * Wraps GRASS library routine:
   *
   * <pre>
   * double
   * G_row_to_northing (row, region)
   *     double row;
   *     struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.2 <I>The Active Program Region</I><BR>
   *
   * <B>Note:</B> row is a double: row+0.5 will return the northing
   * for the center of the row; row+0.0 will return the northing for
   * the northern edge of the row; and row+1.0 will return the
   * northing for the southern edge of the row.
   *
   * @param row row to find northing for
   *
   * @return northing for the specified row 
   *
   */
  public native double G_row_to_northing( double row );

  /**
   * Converts an easting relative to a region to a column.
   * Wraps GRASS library routine:
   *
   * <pre>
   * double
   * G_easting_to_col (east, region)
   *      double east; 
   *      struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.2 <I>The Active Program Region</I><BR>
   *
   * <B>Note:</B> the result is a double. Casting it to an integer will give
   * the column number.
   *
   * @param east easting to find column number for
   *
   * @return column number for the specified easting
   *
   */
  public native double G_easting_to_col( double east );

  /**
   * Converts a northing relative to a region to a row.
   * Wraps GRASS library routine:
   *
   * <pre>
   * double
   * G_northing_to_row (north, region)
   *      double row;
   *      struct Cell_head *region;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.7.2 <I>The Active Program Region</I>
   *
   * <B>Note:</B> the result is a double. Casting it to an integer
   * will give the row number.
   *
   * @param north northing to find row number for
   *
   * @return row number for the specified northing
   *
   */
  public native double G_northing_to_row( double north );

  // Cloning
  /**
   * Creates a new object of the same class as this object. It then
   * initializes each of the new object's fields by assigning it the
   * same value as the corresponding field in this object. No constructor
   * is called.
   *
   * @return Newly cloned Cellhead
   * 
   */
  public Object clone() {
    try {
      Cellhead ch_new = (Cellhead)super.clone();
      ch_new.reallocate();
      return ch_new;
    } catch (CloneNotSupportedException e) { 
      System.err.println("Something happened that must never happen");
      System.exit(-1);
      return null;
    }
  }
  
  /**
   * Assists cloning by allocating a new Cellhead structure
   * in memory and replicating its content. 
   * Must be never called from outside of methods of this class.
   */
  protected native void reallocate();
  
  // for Internal use
  /**
   * Inquires for memory address of classes's GRASS struct Colors.
   * Must be never called from outside of methods of this class.
   */
  protected long getAddr() {
    return addr;
  }
  
  // Cleaning after
  /**
   * Used by other methods to free memory of GRASS struct Colors.
   * Must be never called from outside of methods of this class.
   */
  protected native void free();
  
  /**
   * Frees memory for GRASS struct Colors and calls finalize
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
   * @return String that represents this object -- a rather long string
   * with all fields listed with their names and values.
   *
   */
  public String toString() {
    return getClass()+" at addr="+addr+
      "[format="     + format()     +
      " compressed=" + compressed() +
      " rows="       + rows()       +
      " cols="       + cols()       +
      " proj="       + proj()       +
      " zone="       + zone()       +
      " ew_res="     + ew_res()     +
      " ns_res="     + ns_res()     +
      " north="      + north()      +
      " sout="       + south()      +
      " east="       + east()       +
      " west="       + west()       +
      "]";
  }

}

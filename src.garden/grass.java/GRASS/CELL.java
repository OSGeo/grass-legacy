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
 * This class wraps GRASS library CELL*. Note that this CELL is a row 
 * of cells and not a single CELL. 
 *
 * See the <B>GRASS 4.2 Programmer's Manual</B>, 
 * Appendix B <I>The CELL Data Type</I>
 * 
 * @author  $Author$
 * @version $Revision$
 *
 * @see GRASS.libgis
 *
 */
public class CELL {

  /**
   * Handles memory location of the actual row of the CELLs.
   */
  private long addr = 0;
  
  /**
   * Number of cells in the Row.
   */
  protected int size;

  // Creating
  /**
   * Constructs an empty object and allocates memory for a row of CELLs.
   * Somewhat equivivalent to GRASS library routine:
   *
   * <pre>
   * CELL *cell;
   * cell = G_allocate_cell_buf( );
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.5 <I>Allocating Raster I/O Buffers</I>
   *
   * @param gl Previously initialized GRASS library
   *
   */
  public CELL() {
    G_allocate_cell_buf();
  }

  /**
   * Returns the size of this CELL buffer in cells. Buffer size is
   * determined at the moment of creation by calling G_window_cols()
   * routine.
   *
   * @return size of the buffer
   *
   */
  public int getSize() {
    return size;
  }
  
  /**
   * Allocates memory for a row of CELLs. 
   * Must be never called from outside methods of this class.
   * Wraps GRASS library routine:
   * 
   *
   * <pre>
   * CELL *cell;
   * cell = G_allocate_cell_buf( );
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.5 <I>Allocating Raster I/O Buffers</I>
   *
   */
  protected native void G_allocate_cell_buf();
  
  /**
   * Assigns each member of raster buffer to zero. Wraps GRASS
   * library routine:
   *
   * <pre>
   * G_zero_cell_buf(buf);
   * CELL *buf;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.5 <I>Allocating Raster I/O Buffers</I>
   *
   * @exception GRASSException is thrown if an attempt to call this
   *            method was done when this buffer size is not equall
   *            to G_window_cols()
   *
   */
  public native void G_zero_cell_buf() throws GRASSException;
  
  // Accessing
  /**
   * Inquires for category value in the buffer at the position pos.
   *
   * @param pos position in the cell buffer
   * 
   * @return category value at the specified position
   *
   * @exception GRASSException Exception with an appropriate error 
   *            message is thrown if specified position falls outside
   *            buffer size.
   *
   */
  public int cellAt(int pos) throws GRASSException {
    return cellAt_(addr, pos);
  }
  
  /**
   * Inquires for category value in the buffer at the position pos.
   * Must be never called from outside methods of this class.
   *
   * @param pos position in the cell buffer
   * 
   * @param addr memory address of CELL buffer
   * 
   * @return category value at the specified position
   *
   * @exception GRASSException Exception with an appropriate error 
   *            message is thrown if specified position falls outside
   *            buffer size.
   *
   */
  private native int cellAt_(long addr, int pos) throws GRASSException;
 
  /**
   * Sets category value in the buffer at the position pos.
   *
   * @param pos position in the cell buffer
   * 
   * @param val category value position to set position to
   * 
   * @exception GRASSException Exception with an appropriate error 
   *            message is thrown if specified position falls outside
   *            buffer size.
   *
   */
  public void setCellAt(int pos, int val) throws GRASSException {
    setCellAt_( addr, pos, val );
  }
  
  /**
   * Sets category value in the buffer at the position pos.
   * Must be never called from outside methods of this class.
   *
   * @param pos position in the cell buffer
   * 
   * @param addr memory address of CELL buffer
   * 
   * @param val category value position to set position to
   * 
   * @exception GRASSException Exception with an appropriate error 
   *            message is thrown if specified position falls outside
   *            buffer size.
   *
   */
  private native void setCellAt_(long addr, int pos, int val)
    throws GRASSException;

  /**
   * Allocates array and fills it with the content of the buffer.
   *
   * @return content of the buffer 
   */
  public int[] toArray() {
    try {
      return toArray( new int[ size ] );
    } catch (GRASSException e) {
      return null;
    }
  }

  /**
   * Fills the provided array with the content of the buffer. To save
   * memory allocate array with toArray() method and then use this
   * method to update it on each iteration. This method is more effective
   * then calling cellAt().
   *
   * @return content of the buffer 
   *
   * @param ar array to save buffer values to
   *
   * @exception GRASSException is thrown if size of the provided
   *            array is not equal to the size of the buffer
   */
  public native int[] toArray(int[] ar) throws GRASSException;

  /**
   * Fills the buffer with values from the provided array. This method
   * is more effective
   * then calling setCellAt().
   *
   * @param ar array to read buffer values from
   *
   * @exception GRASSException is thrown if size of the provided
   *            array is not equal to the size of the buffer
   */
  public native void fromArray(int[] ar) throws GRASSException;
  // was not tested!

  /**
   * Creates and fills an array with the content of the buffer converted
   * into integer color values. This method is convinient
   * for in-memory images
   *
   * @return content of the buffer 
   *
   * @param color Colors object used to convert original buffer values
   *
   * @param ar array to save color values to
   *
   * @see java.awt.Color
   */
  public int[] toColorArray(Colors colors) {
    try {
      return toColorArray( new int[ size ], colors );
    } catch (GRASSException e) {
      return null;
    }
  }

  /**
   * Fills the provided array with the content of the buffer converted into
   * integer color values. To save
   * memory allocate array with toColorArray() method and then use this
   * method to update it on each iteration. This method is convinient
   * for in-memory images
   *
   * @return content of the buffer 
   *
   * @param color Colors object used to convert original buffer values
   *
   * @param ar array to save color values to
   *
   * @exception GRASSException is thrown if size of the provided
   *            array is not equal to the size of the buffer
   *
   * @see java.awt.Color
   */
  public native int[] toColorArray(int[] ar, Colors colors)
    throws GRASSException;

  // for Internal use
  /**
   * Inquires for memory address of the CELL buffer.
   * Must be never called from outside of methods of this class.
   *
   * @return Memory address of the CELL buffer
   *
   */
  protected long getAddr() {
    return addr;
  }
  
  // Cleaning after
  /**
   * Used by other methods to free memory of CELL buffer.
   * Must be never called from outside of methods of this class.
   */
  protected native void free();
  
  /**
   * Frees memory for CELL buffer and calls the finalize
   * method of the superclass.
   * Must be never called from outside methods of this class.
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
   * @return String that represents this object (only its location in 
   *         memory and row length)
   *
   */
  public String toString() {
    return getClass()+" at addr="+addr+" for "+size+" elements";
  }

}


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
 * Wraps GRASS library struct line_pnts. 
 *
 * line_pnts is not documented in the <B>GRASS 4.2 Programmer's
 * Manual</B>, but see Section 13.5 <I>Data Structures (Vector
 * Library)</I> to learn about routines that deal with struct
 * line_pnts.
 * 
 * @author  $Author$
 * @version $Revision$
 *
 */
public class linepnts {

  /**
   * linepnts type LINE.
   */
  public final static int LINE = 0x01;

  /**
   * linepnts type AREA.
   */
  public final static int AREA = 0x02;

  /**
   * linepnts type DOT.
   */
  public final static int DOT  = 0x04;

  protected int type;

  /**
   * Handles memory location of the actual struct Cell_stats.
   */
  private long addr = 0;

  /**
   * Constructs an empty object and allocates memory for GRASS struct
   * line_pnts
   */
  public linepnts() {
    addr = Vect_new_line_struct();
  }
  
  /**
   * Constructs an empty object provided type and memory location.
   * Intended to be called only by methods in package GRASS.
   *
   * @param addr memory address of the previously allocated struct
   *             line_pnts 
   *
   * @param type 
   */
  protected linepnts( long addr, int type ) {
    this.addr = addr;
    this.type = type;
  }
  
  /**
   * Inquires for memory address of the line_pnts struct.
   * Must be never called from outside of methods of this class.
   *
   * @return Memory address of the CELL buffer
   *
   */
  protected long getAddr() {
    return addr;
  }

  /**
   * Inquires for the type of this object (LINE, AREA or DOT).
   *
   * @return type (LINE, AREA or DOT)
   *
   */
  public int getType() {
    return type;
  }

  /**
   * Sets type of this object (LINE, AREA or DOT).
   *
   * @param type type (LINE, AREA or DOT)
   *
   */
  public void setType(int type) {
    this.type = type;
  }

  /**
   * Inquires for the nukber of coordinate pairs in this object.
   *
   * @return number of coordinate pairs in this object
   *
   */
  public native int n_points();

  /**
   * Initialize line_pnts structure. Intended to be used only by methods
   * of this class.
   * Wraps GRASS library routine:
   *
   * <pre>
   * struct line_pnts *
   * Vect_new_line_struct ()
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.5.</I> Data Structures
   *
   */
  protected native long Vect_new_line_struct();

  /**
   * Copies coordinate pairs from the arrays of x and y coordinates
   * into linepnts object. Arrays nust be of an equal size. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_copy_xy_to_pnts (Points, x, y, n)
   *    struct line_pnts *Points;
   *    double *x, *y; int n;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.6.</I> Data Conversion
   *
   * @param x array of X coordinates to copy values from
   *
   * @param y array of Y coordinates to copy values fro
   *
   * @exception GRASSException is thrown if arrays are of unequal size.
   *
   */
  public native void Vect_copy_xy_to_pnts( double[] x, double[] y )
    throws GRASSException;

  /**
   * Copies coordinate pairs associated with linepnts object into two 
   * arrays. Arrays' sizes must be equal or bigger then number of points
   * in this object. If array size is bigger its reminder is left intact
   * so be carefull not to use it. Arrays can be allocated beforehand with
   * the command 
   * <pre>
   * linepnts lp = ....;
   *
   * double x = new double[ lp.n_points() ];
   * double y = new double[ lp.n_points() ];
   * </pre>
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_copy_pnts_to_xy (Points, x, y, n)
   *    struct line_pnts *Points;
   *    double *x, *y; int *n;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.6.</I> Data Conversion
   *
   * @param x array of X coordinates to copy values to
   *
   * @param y array of Y coordinates to copy values to
   *
   * @exception GRASSException is thrown if size of one of the arrays
   *            is smaller than number of coordinate pairs in this object..
   *
   */
  public native void Vect_copy_pnts_to_xy( double[] x, double[] y )
    throws GRASSException;

  /**
   * Free any memory created for a line_pnts structure. Called by the
   * finalize method.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_destroy_line_struct (Points)
   *    struct line_pnts *Points;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.5</I> Data Structures
   *
   */
  protected native void Vect_destroy_line_struct();
  
  /**
   * Frees memory for GRASS struct line_pnts and calls finalize
   * method of the superclass.
   *
   * @exception Throwable if thrown by superclass finalizer
   */
  protected void finalize() throws Throwable {
    
    Vect_destroy_line_struct(); //explicitly release memory
    super.finalize();

  }
  
  /**
   * Returns a string representation of the object.
   * 
   * @return String that represents this object (memory address, type
   *         and number of coordinate pairs).
   *
   */
  public String toString() {
    return getClass()+" at addr="+addr+" type="+type+" n_points="+n_points();
  }

}

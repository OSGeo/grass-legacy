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

import java.util.*;

/**
 * This class wraps GRASS library struct Cell_stats (Raster Histogram). 
 *
 * Cell_stats is not documented in the <B>GRASS 4.2 Programmer's Manual</B>, 
 * but see Section 12.10.6 <I>Raster Histograms</I> to learn about routines 
 * that deal with  struct Cell_stats.
 * 
 * @author  $Author$
 * @version $Revision$
 *
 */
public class Cellstats {

  /**
   * Handles memory location of the actual struct Cell_stats.
   */
  private long addr = 0;

  /**
   * Constructs an empty object and allocates memory for GRASS struct
   * Cell_stats.
   */
  public Cellstats() {
    allocate();
    G_init_cell_stats();
  }
  
  // 12.10.6 Raster Cell_stats File
  /**
   * Used by other methods to allocate memory for GRASS struct Cell_stats.
   * Must be never called from outside of methods of this class.
   */
  protected native void allocate();
  
  /**
   * Initializes the Cell_stats structure for updates.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_init_Cell_stats (Cell_stats)
   *      struct Cell_stats *Cell_stats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.6 <I>Raster Cell_stats File</I>
   *
   */
  private native void G_init_cell_stats();

  /**
   * This routine updates the Cell_stats data
   * with values from the row of cell values.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_update_cell_stats (data, n, Cell_stats)
   *         CELL *data;
   *         int n;
   *         struct Cell_stats *Cell_stats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.6 <I>Raster Histogram</I>
   *
   * @param buf buffer of cell values
   *
   * @param n number of values to be used for update
   *
   */
  public void G_update_cell_stats(CELL buf, int n) {
    
    G_update_cell_stats_( buf.getAddr(), n );

  }

  /**
   * The actual method to update the Cell_stats,
   * with values from the row of cell values. Must be never called by
   * any methods other than memebers of this class. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_update_cell_stats (cell, n, Cell_stats)
   *         CELL *cell;
   *         int n;
   *         struct Cell_stats *Cell_stats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.6 <I>Raster Histogram</I>
   *
   * @param buf buffer of cell values
   *
   * @param n number of values to be used for update
   *
   */
  private native void G_update_cell_stats_(long addr, int n);

  /**
   * This method allows a random query of the Cell_stats.
   * The count associated with the raster value cat is returned.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_find_cell_stat (cat, count, s)
   *       CELL cat;
   *       long *count; 
   *       struct Cell_stats *s;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.6 <I>Raster Histogram</I>
   *
   * @param cat category value
   *
   * @return count associated with category value
   *
   * @exception GRASSException if category value was not found in the 
   *            structure
   */
  public native long G_find_cell_stat( int cat ) throws GRASSException;

  // Cleaning after
  /**
   * Used by other methods to free memory of GRASS struct Cell_stats.
   * Must be never called from outside of methods of this class.
   */
  protected native void G_free_cell_stats();
  
  /**
   * Deallocates memory for GRASS struct Cell_stats and calls finalize
   * method of the superclass.
   *
   * @exception Throwable if thrown by superclass finalizer
   */
  protected void finalize() throws Throwable {
    
    G_free_cell_stats(); //explicitly release memory
    super.finalize();

  }
  
  /**
   * Returns a string representation of the object.
   * 
   * @return String that represents this object 
   *
   */
  public String toString() {
    return getClass()+" at addr="+addr;
  }

  // Methods that deal with browsing through Cell_stat entries 

  /**
   * This object is rewound (i.e., positioned at the first raster category)
   * so that sorted sequential retrieval can begin.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_rewind_cell_stats (s)
   *    struct Cell_stats *s;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.6 <I>Raster Histogram</I>
   *
   */
  public native void G_rewind_cell_stats();

  /**
   * Retrieves the next cat,count Pair. Returns null if there are no more
   * items. Must be called after G_rewind_cell_stats() was called.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_next_cell_stat (cat, count, s)
   *      CELL *cat;
   *      long *count;
   *      struct Cell_stats *s;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.10.6 <I>Raster Histogram</I>
   *
   * @return The cat-count value or null in no more categories left.
   *
   */
  public native Pair G_next_cell_stat();

  /**
   * Represents a single category-count value in Cell_stats struct.
   * There are no public constructors in this class as instances of
   * this class are not intended for creation anywhere out of the methods
   * of its parent class.
   */
  public class Pair {

    /**
     * Category number
     */
    public int cat;

    /**
     * Cell count
     */
    public long count;

    /**
     * Creates a new cellstats pair.
     *
     * @param cat category number
     *
     * @param count cell count for this category number
     *
     */
    private Pair( int cat, long count ) {
      this.cat = cat;
      this.count = count;
    }

  }

}

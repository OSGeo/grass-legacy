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
 * Wraps GRASS library struc Map_info which is used to access vector 
 * files in GRASS database.
 * This class does not have any public constructors. Use methods in
 * libvect to create objects of this class.
 *
 * @author  $Author$
 * @version $Revision$
 *
 * @see libvect
 *
 */
public class Mapinfo {

  /**
   * Stores memory address of class's GRASS struct Map_info.
   */
  private long addr;

  /**
   * Stores memory current offset in vector file (of no use for now).
   */
  protected long offset = 0;

  /**
   * Flag to indicate if file is open or not.
   */
  private boolean open = false;

  /**
   * Creates a new instance of the class. Intended to be called 
   * only by methods in libgis class.
   * 
   * @param addr memeory address of Map_info*
   *
   */
  protected Mapinfo( long addr ) {
    this.addr = addr;
    open = true;
  }

  /**
   * Inquiries file offset.
   *
   * @return file offset
   */
  public long getOffset() {
    return offset;
  }

  /**
   * Returns the number of the level at which a Map is opened at or 
   * -1 if Map is not opened.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_level (Map)
   *   struct Map_info *Map;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.7.</I> Miscellaneous (Vector Library)
   *
   * @return level at which a Map is opened at, -1 if Map is not opened
   *
   */
  public native int Vect_level();

  /**
   * This is the primary routine for reading through a vector map.
   * It simply reads the next line from the map into the linepnts object.
   * This method should not be used in conjunction with any other read_line
   * method.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_read_next_line (Map, Points)
   *    struct Map_info *Map;
   *    struct line_pnts *Points;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.4.</I> Reading and writing vector maps
   *
   * @return linepents object read or null on EOF
   *
   * @exception GRASSException if an error was returned by the GRASS routine
   *
   */
  public native linepnts Vect_read_next_line()
    throws GRASSException;

  /**
   * Returns total number of lines in the vector Map. Works for vector layers
   * of level 2 or higher.
   * Wraps GRASS library routine:
   *
   * <pre>
   * V2_num_lines (Map)
   *    struct Map_info *Map; 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.7.</I> Miscellaneous (Reading and writing vector maps)
   *
   * @return number of arcs in vector map
   *
   * @exception GRASSException if an error was returned by the GRASS routine
   *            (usually it means that open vector layer is not of Level 2)
   *
   */
  public native int V2_num_lines() throws GRASSException;

  /**
   * Returns total number of areas in the vector Map. Works for vector layers
   * of level 2 or higher.
   * Wraps GRASS library routine:
   *
   * <pre>
   * V2_num_areas (Map)
   *    struct Map_info *Map; 
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.7.</I> Miscellaneous (Reading and writing vector maps)
   *
   * @return number of areas in vector map
   *
   * @exception GRASSException if an error was returned by the GRASS routine
   *            (usually it means that open vector layer is not of Level 2)
   *
   */
  public native int V2_num_areas() throws GRASSException;

  /**
   * Creates a new linepnts structure with the list of points which describe
   * an area in clockwise order.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_get_area_points (Map, area, Points)
   *     struct Map_info *Map;
   *     int area;
   *     struct line_pnts *Points;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.7.</I> Miscellaneous (Reading and writing vector maps)
   *
   * @param int area number in interval 1..V2_num_areas() 
   *
   * @return linepents object read 
   *
   * @exception GRASSException if an error was returned by the GRASS routine
   *            (usually it means that open vector layer is not of Level 2
   *            or I/O problem)
   *
   */
  public native linepnts Vect_get_area_points(int area)
    throws GRASSException;

  /**
   * This routine will read a line from the vector map at the
   * specified line index in the map.  Refer to V2_num_lines for
   * number of lines in the map. This function is available at level 2
   * or higher.
   * Wraps GRASS library routine:
   *
   * <pre>
   * V2_read_line (Map, Points, line)
   *      struct Map_info *Map;
   *      struct line_pnts *Points;
   *      int line;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.4.</I> Reading and writing vector maps
   *
   * @param int area number in interval 1..V2_num_lines() 
   *
   * @return linepents object read or null on EOF
   *
   * @exception GRASSException if an error was returned by the GRASS routine
   *            (usually it means that open vector layer is not of Level 2
   *            or I/O problem)
   *
   */
  public native linepnts V2_read_line(int line)
    throws GRASSException;

  /**
   * Retrieves line's attribute number. This function is available at level 2
   * or higher.
   * Wraps GRASS library routine:
   *
   * <pre>
   * V2_line_att (Map, line)
   *      struct Map_info *Map;
   *      int line;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.7.</I> Miscellaneous (Reading and writing vector maps)
   *
   * @param int area number in interval 1..V2_num_lines() 
   *
   * @return line's attribute number
   *
   */
  public native int V2_line_att(int line);

  /**
   * Retrieves area's attribute number. This function is available at level 2
   * or higher.
   * Wraps GRASS library routine:
   *
   * <pre>
   * V2_area_att (Map, line)
   *      struct Map_info *Map;
   *      int area;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.7.</I> Miscellaneous (Reading and writing vector maps)
   *
   * @param int area number in interval 1..V2_num_areas() 
   *
   * @return area's attribute number
   *
   */
  public native int V2_area_att(int area);

  /**
   * Resets the read pointer to the beginning of the map.
   * This only affects the routine Vect_read_next_line.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_rewind (Map)
   *     struct Map_info *Map;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.4.</I> Reading and writing vector maps
   *
   */
  public native void Vect_rewind();

  /**
   * <font color=red><b>This method is broken! I will fix it in GRASS-JNI 
   * release for GRASS 5.</b></font></P>
   *
   * This routine will set a restriction on reading only those lines which
   * fall entirely or partially in the specified rectangular region.
   * Vect_read_next_line is the only routine affected by this, and it does
   * NOT cause line clipping. Constraints affect only this Mapinfo specified.
   * They do not affect any other Maps that may be open.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_set_constraint_region (Map, n, s, e, w)
   *    struct Map_info *Map;
   *    double n, s, e, w;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.4.</I> Reading and writing vector maps
   *
   * @param n coordinate of the northern edge
   *
   * @param s coordinate of the southern edge 
   *
   * @param e coordinate of the eastern edge 
   *
   * @param w coordinate of the western edge 
   *
   */
  public native void Vect_set_constraint_region
    (double n, double s, double e, double w);
  // Method was never tested!!!

  /**
   * This routine will set a restriction on reading only those lines which
   * match the types specified. This can be any combination of types bitwise
   * OR'ed together. For example: linepnts.LINE | linepnts.AREA would exclude
   * any DOT line types. Vect_read_next_line is the only routine affected 
   * by this. If type is set to -1, all lines will be read including deleted
   * or dead lines.
   * 
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_set_constraint_type (Map, type)
   *    struct Map_info *Map;
   *    int type;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 

   * Section <I>13.4.</I> Reading and writing vector maps
   *
   * @param type constraints type (see linepnts for type values)
   *
   * @see linepnts
   *
   */
  public native void Vect_set_constraint_type(int type);

  /**
   * Removes all constraints currently affecting this object.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_remove_constraints (Map)
   *    struct Map_info *Map;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.4.</I> Reading and writing vector maps
   *
   */
  public native void Vect_remove_constraints();

  /**
   * This method will write out a line. The type of line is one of: 
   * AREA, LINE, DOT (defined in linepnts)
   * 
   * Wraps GRASS library routine:
   *
   * <pre>
   * long
   * Vect_write_line (Map, type, Points)
   *    struct Map_info *Map;
   *    int type;
   *    struct line_pnts *Points;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.4.</I> Reading and writing vector maps
   *
   * @param type constraints type (see linepnts for type values)
   *
   * @param points object with feature geometry
   *
   * @return offset into the file where the line started.
   *
   * @exception GRASSException if an error code was returned by the
   *            GARSS library routine
   *
   */
  public long Vect_write_line( int type, linepnts points )
    throws GRASSException {

    return Vect_write_line_( type, points.getAddr() );
  }

  /**
   * This method will write out a line. The type of line is one of: 
   * AREA, LINE, DOT (defined in linepnts). Sets object field offset.
   * Must be never called from outside methods of this class.
   * Wraps GRASS library routine:
   *
   * <pre>
   * long
   * Vect_write_line (Map, type, Points)
   *    struct Map_info *Map;
   *    int type;
   *    struct line_pnts *Points;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.4.</I> Reading and writing vector maps
   *
   * @param type constraints type (see linepnts for type values)
   *
   * @param addr memory address of struct line_pnts
   *
   * @return offset into the file where the line started.
   *
   * @exception GRASSException if an error code was returned by the
   *            GARSS library routine
   *
   */
  private native long Vect_write_line_( int type, long addr )
    throws GRASSException;
  // Metod was never tested !!!

  /**
   * This routine closes an open vector map and cleans up the structures
   * associated with it. It has to be called before exiting the program 
   * or it is called automatically by the finalise() method. When used
   * in conjunction with Vect_open_new, it will cause the final writing of
   * the vector header before closing the vector map.
   * Wraps GRASS library routine:
   *
   * <pre>
   * Vect_close (Map)
   *    struct Map_info *Map;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>13.4.</I> Reading and writing vector maps
   *
   */
  public native void Vect_close();

  // finalize
  /**
   * Closes an open vector map if one was left open. 
   *
   * @exception Throwable if there were problems closing left vector map
   *            descriptor or thrown by finalizer of the parent method
   */
  protected void finalize() throws Throwable {
    if (open) Vect_close();
    super.finalize();
  }

  /**
   * Returns a string representation of the object.
   * 
   * @return String that represents this object -- memory address,
   * level, open flag and current offset.
   *
   */
  public String toString() {
    return getClass()+" at addr="+addr+", level="+Vect_level()+
      " open="+open+" on offset="+offset;
  }

}

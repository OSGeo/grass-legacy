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
 * This class wraps GRASS library struct Categories (Raster Histogram). 
 *
 * Categories is not documented in the <B>GRASS 4.2 Programmer's Manual</B>, 
 * but see Section 12.10.2 <I>Raster Categoriy File</I> to learn about 
 * routines that deal with struct Categories.
 * 
 * @author  $Author$
 * @version $Revision$
 *
 */
public class Categories {

  /**
   * Handles memory location of the actual struct Categories.
   */
  private long addr = 0;

  /**
   * Flag to show if G_init_cats was called.
   */
  private boolean initialized = false;

  /**
   * Constructs an empty object, allocates memory and initializes GRASS struct
   * Categories.
   */
  public Categories( int n, String title ) {
    allocate();
    G_init_cats( n, title );
  }
  
  /**
   * Constructs an empty object, allocation of memory WITHOUT initialization 
   * GRASS struct Categories.
   */
  protected Categories() {
    allocate();
  }
  
  // 12.10.6 Raster Categories File
  /**
   * Used by other methods to allocate memory for GRASS struct Categories.
   * Must be never called from outside of methods of this class.
   */
  protected native void allocate();
  
  /**
   * Returns the value of Categories->num field in struct Categories.
   * Probably it is the highest category number.
   *
   * @return value of Categories->num (highest category number?)
   *
   */
  public native int num();

  /**
   * This routine initializes the cats structure, and copies the title
   * into the structure. The number of categories is set initially to n.
   * Wraps  GRASS library routine.
   *
   * <pre>
   * G_init_cats (n, title, cats)
   *     CELL n; 
   *     char *title;
   *     struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.10.2.2</I>Querying and Changing the Categories Structure
   *
   * @param n number of categories
   *
   * @param title Categories title
   *
   */
  protected native void G_init_cats(int n, String title);

  /**
   * Writes the category file for the raster file name in the current mapset.
   * Wraps GRASS library routine
   *
   * <pre>
   * G_write_cats (name, cats)
   *     char *name;
   *     struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.10.2.1.</I> Reading and Writing the Raster Category File
   *
   * @param name raster layer name
   *
   * @exception GRASSException is thrown with an appropriate message if 
   * there was an error reading category file
   * 
   */
  public native void G_write_cats(String name)
    throws GRASSException;
  // This method was never tested!

  /**
   * Writes the category file for the vector file name in the current mapset.
   * Wraps GRASS library routine
   *
   * <pre>
   * G_write_cats (name, cats)
   *     char *name;
   *     struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.11.6</I> Vector Category File
   *
   * @param name vector layer name
   *
   * @exception GRASSException is thrown with an appropriate message if 
   * there was an error reading category file
   * 
   */
  public native void G_write_vector_cats(String name)
    throws GRASSException;
  // This method was never tested!

  /**
   * This method looks up category n and returns a string which is the
   * label for the category. If the category does not exist in cats, then
   * an empty string "" is returned. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * char *
   * G_get_cat (n, cats)
   *    CELL n;
   *    struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.10.2.2</I> Querying and Changing the Categories Structure
   *
   * @param n category value to look label for
   *
   * @return category label or empty string if category does not exist
   *
   */
  public native String G_get_cat( int cat );

  /**
   * Gets title from this category structure.
   * Wraps GRASS library function.
   *
   * <pre>
   * char*
   * G_get_cell_title(cats)
   *     struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.10.2.2. </I>Querying and Changing the Categories Structure
   *
   * @return categories title
   *
   */
  public native String G_get_cats_title();

  /**
   * Sets the title of this Categories structure.
   *
   * <pre>
   * G_set_cats_title (title, cats)
   *    char *title;
   *    struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.10.2.2.</I> Querying and Changing the Categories Structure
   *
   * @param title title to set
   *
   */
  public native void G_set_cats_title( String title );

  /**
   * The label is copied into the cats structure for category n.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_set_cat (n, label, cats)
   *     CELL n;
   *     char *label;
   *     struct Categories *cats;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section <I>12.10.2.2.</I>Querying and Changing the Categories Structure
   *
   * @param cat category to modify
   *
   * @param label new category label
   *
   */
  public native void G_set_cat( int cat, String label );

  // Cleaning after
  /**
   * Used by other methods to free memory of GRASS struct Categories.
   * Must be never called from outside of methods of this class.
   */
  protected native void G_free_cats();
  
  /**
   * Deallocates memory for GRASS struct Categories and calls finalize
   * method of the superclass.
   * Must be never called from outside of methods of this class.
   *
   * @exception Throwable if thrown by superclass finalizer
   */
  protected void finalize() throws Throwable {
    
    G_free_cats(); //explicitly release memory
    super.finalize();

  }
  
  /**
   * Returns a string representation of the object.
   * 
   * @return String that represents this object 
   *
   */
  public String toString() {
    return getClass()+" at addr="+addr+", title='"+G_get_cats_title()+"'";
  }

}




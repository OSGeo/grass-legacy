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
 * Wraps FILE* when one is used to access GRASS site lists.
 * This class does not have any public constructors. Use methods in
 * libgis to create objects of this class.
 *
 * @author $Author$
 * @version $Revision$
 *
 * @see libgis
 *
 */
public class FILE {

  /**
   * Memory address of FILE*.
   */
  protected long addr;

  /**
   * Flag to indicate if stream is open or not.
   */
  private boolean open = false;

  /**
   * Creates a new instance of the class. Intended to be called 
   * only by methods in libgis class.
   * 
   * @param addr memeory address of FILE*
   *
   */
  protected FILE( long addr ) {
    this.addr = addr;
    open = true;
  }

  /**
   * Reads the next site from the site list file. Returns null if there
   * are no more sites in file.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_get_site (fd, east, north, desc)
   *     FILE *fd;
   *     double *east, *north;
   *     char **desc;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.12.3.<I>Reading and Writing Site List Files</I>
   *
   * @return the next site from the file or null if no more sites are left
   *
   */
  public native Site G_get_site();

  /**
   * Writes a site into the site list file.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_put_site (fd, east, north, desc)
   *     FILE *fd;
   *     double east, north;
   *     char *desc;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.12.3. <I>Reading and Writing Site List Files</I>
   *
   * @param s site to to save
   *
   */
  public void G_put_site(Site s) {
    G_put_site( s.east, s.north, s.desc );
  }

  /**
   * Writes a new site with east and north coordinates and site description
   * descinto the site list file.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_put_site (fd, east, north, desc)
   *     FILE *fd;
   *     double east, north;
   *     char *desc;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.12.3. <I>Reading and Writing Site List Files</I>
   *
   * @param east east coordinate
   *
   * @param north north coordinate
   *
   * @param desc site description
   *
   */
  public native void G_put_site( float east, float north, String desc );
  // Method was never tested!

  /**
   * Closes previously opened FILE*. Sets open flag to false;
   * Wraps the folowing function from stdio.h
   *
   * <pre>
   * int fclose(FILE *stream)
   * </pre>
   *
   */
  public native void close();

  /**
   * Closes file if one was left open. 
   *
   * @exception Throwable if there were problems closing left file
   *            descriptor or thrown by finalizer of the parent method
   */
  protected void finalize() throws Throwable {
    if (open) close();
    super.finalize();
  }

}

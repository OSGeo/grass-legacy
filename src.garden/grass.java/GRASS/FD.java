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
 * Wraps file descriptors if ones are used to access GRASS cell files.
 * This class does not have any public constructors. Use methods in
 * libgis to create objects of this class.
 *
 * @author $Author$
 * @version $Revision$
 *
 * @see libgis
 *
 */
public class FD {

  /**
   * Actual file descriptor number.
   */
  private int fd;

  /**
   * Flag to indicate if file descripter is open or not.
   */
  private boolean open = false;

  /**
   * Pointer to libgis instance
   */
  private libgis gl;

  /**
   * Creates a new instance of the FD class. Intended to be called 
   * only by methods in libgis class.
   * 
   * @param gl pointer to libgis instance
   *
   * @param fd actual file descriptor
   *
   */
  protected FD( libgis gl, int fd) {
    this.gl = gl;
    this.fd = fd;
    open = true;
  }

  // 12.9.6 Reading Raster files ///////////////////////////////////////////
  /**
   * Reads the specified row from the raster into the cell buffer. 
   * Somewhat equivalent to GRASS library routine:
   *
   * <pre>
   * G_get_map_row (fd, cell, row)
   * int fd;
   * CELL *cell;
   * int row;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.6 <I>Reading Raster files</I>
   *
   * @param buf CELL buffer allocated with G_allocate_cell_buf()
   *
   * @param row row number in range 0..G_window_rows()
   *
   * @exception GRASSException with appropriate message if error occurred
   *            reading raster file
   *
   */
  public void G_get_map_row(CELL buf, int row) 
      throws GRASSException {

    checkRead( row );
    
    if ( G_get_map_row_(fd, buf.getAddr(), row) == -1 ) throw
      new GRASSException("Failed reading fd="+fd);
  }

  /**
   * Reads the specified row from the raster into the cell buffer. 
   * Must be never be called by any methods except for members of this class. 
   * File must be open on file descriptor fd (as returned by G_open_cell_old). 
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_get_map_row (fd, cell, row)
   * int fd;
   * CELL *cell;
   * int row;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.6 <I>Reading Raster files</I>
   *
   * @param fd File descriptor as returned by G_open_cell_old
   *
   * @param addr memory address of the allocated GRASS CELL structure
   *
   * @param row row number in range 0..G_window_rows()
   *
   * @return -1 if error has occured, non-negative value is returned 
   * otherwise
   *
   */
  private native int G_get_map_row_(int fd, long addr, int row); 

  /**
   * Reads the specified row from the raster  into the cell buffer
   * while masking is suppressed.
   * Somewhat equivalent to GRASS library routine:
   *
   * <pre>
   * G_get_map_row_nomask (fd, cell, row)
   * int fd;
   * CELL *cell;
   * int row;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.6 <I>Reading Raster files</I>
   *
   * @param buf cell buffer allocated with G_allocate_cell_buf()
   *
   * @param row row number in range 0..G_window_rows()
   *
   * @exception GRASSException with appropriate message if error while
   *            reading raster file occurred
   *
   */
  public void G_get_map_row_nomask(CELL buf, int row) 
      throws GRASSException {

    checkRead( row );
    
    if ( G_get_map_row_nomask_(fd, buf.getAddr(), row) == -1 ) throw
      new GRASSException("Failed reading fd="+fd);
  }

  /**
   * Reads the specified row from the raster into the cell buffer
   * while masking is suppressed. 
   * File must be open on file descriptor fd (as returned by G_open_cell_old).
   * Must be never called by any methods except for members of this class. 
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_get_map_row_nomask (fd, cell, row)
   * int fd;
   * CELL *cell;
   * int row;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.6 <I>Reading Raster files</I>
   *
   * @param fd File descriptor as returned by G_open_cell_old
   *
   * @param addr memory address of the allocated GRASS CELL structure
   *
   * @param row row number in range 0..G_window_rows()
   *
   * @return -1 if error has occurred; return non-negative value otherwise
   *
   */
  private native int G_get_map_row_nomask_(int fd, long addr, int row); 

  public void G_put_map_row(CELL buf)
    throws GRASSException {

    if ( G_put_map_row_(fd, buf.getAddr()) == -1 ) throw
      new GRASSException("Failed writing fd="+fd);

  }
  // Method was not tested

  protected native int G_put_map_row_(int fd, long addr);

  public void G_put_map_row_random(CELL buf, int row, int col, int ncells)
    throws GRASSException {

    checkRead( row );
    
    if ( G_put_map_row_random_(fd, buf.getAddr(), row, col, ncells) == -1 ) 
      throw new GRASSException("Failed writing fd="+fd);

  }
  // Method was not tested

  protected native int G_put_map_row_random_
      (int fd, long addr, int row, int col, int ncells);

  /**
   * Internal check for correctness of the provided file descriptor and
   * row number. 
   * Not intended to be used by classes outside this class members.
   *
   * @param row row number in range 0..G_window_rows()
   *
   * @exception GRASSException Exception is raised with an appropriate
   *            error message in case where either file descriptor is not
   *            found in the list of opened file descriptors or where
   *            row number falls out of range 0..G_window_rows()
   *
   */
  protected void checkRead( int row ) throws GRASSException {
    if ( !open ) throw
      new GRASSException("Attempt to read from not opened file, fd="+fd);

    if ( row < 0 || row > gl.G_window_rows() ) throw
      new GRASSException("Row value out of range ("+row+
			 " > "+gl.G_window_rows()+")");
  }
  
  // 12.9.8 Closing Raster Files ///////////////////////////////////////////
  /**
   * Closes previously opened raster file. 
   * Somewhat equivalent to GRASS library routine:
   *
   * <pre>
   * G_close_cell (fd)
   *           int fd;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.8 <I>Closing Raster Files</I>
   *
   */
  public void G_close_cell() {
    open = false;
    G_close_cell_( fd );
  }

  /**
   * Closes previously opened raster file. Must never be used by
   * any methods other than members of this class.
   * Wraps GRASS library routine:
   *
   * <pre>
   * G_close_cell (fd)
   *           int fd;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.8 <I>Closing Raster Files</I>
   *
   * @param Fd   File descriptor to close.
   *
   */
  private native void G_close_cell_(int fd);

  /**
   * Closes previously opened raster file. If open for writing, the raster
   * file is not created and the temporary file created when the raster
   * file was opened is removed.
   * Somewhat equivalent to GRASS library routine:
   *
   * <pre>
   * G_unopen_cell (fd)
   *           int fd;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.8 <I>Closing Raster Files</I>
   *
   */
  public void G_unopen_cell() {
    open = false;
    G_unopen_cell_( fd );
  }

  /**
   * Closes previously opened raster file. Must never be used by
   * any methods other than members of libgis class. If open for writing,
   * the raster file is not created and the temporary file created when
   * the raster file was opened is removed. Wraps GRASS library routine:
   *
   * <pre>
   * G_unopen_cell (fd)
   *           int fd;
   * </pre>
   *
   * See the <B>GRASS 4.2 Programmer's Manual</B>, 
   * Section 12.9.8 <I>Closing Raster Files</I>
   *
   * @param Fd   File descriptor to close.
   *
   */
  private native void G_unopen_cell_(int fd);

  /**
   * Closes file descriptor if one was left open. Note that
   * this method uses G_unopen_cell to close the file
   * descriptor so any newly opened layers are delete. Call
   * G_close_cell in your proghram explicitly to save new
   * layers.
   *
   * @exception Throwable if there were problems closing left file
   *            descriptor or thrown by finalizer of the parent method
   */
  protected void finalize() throws Throwable {
    if (open) G_unopen_cell_(fd);
    super.finalize();
  }

}

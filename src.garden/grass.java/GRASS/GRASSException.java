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
 * Signals that exception in GRASS library has occurred. 
 *
 * @author  $Author$
 * @version $Revision$
 */
public class GRASSException extends Exception {

  /**
   * Construct GRASSException with no specific message.
   */
  public GRASSException() {
  }

  /**
   * Construct GRASSException with specified message.
   *
   * @param msg detailed message
   */
  public GRASSException(String msg) {
    super(msg);
  }

}

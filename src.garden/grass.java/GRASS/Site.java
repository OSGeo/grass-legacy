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
 * Represents a site (a point with x,y coordinates and description)
 * from the GRASS database.
 *
 * @author $Author$
 * @version $Revision$
 *
 */
public class Site {

  /**
   * easting (x)
   */
  public float east;

  /**
   * northing (y)
   */
  public float north;

  /**
   * Site description
   */
  public String desc;

  /**
   * Creates a new site.
   *
   * @param east easting
   *
   * @param north northing
   *
   * @param desc site description
   *
   */
  public Site( float east, float north, String desc ) {
    this.east  = east;
    this.north = north;
    this.desc  = desc;
  }

  /**
   * Returns a String representations of the object.
   *
   * @return string representation of the object
   *
   */
  public String toString() {
    return getClass()+": east="+east+" north="+north+" desc='"+desc+"'";
  }

}

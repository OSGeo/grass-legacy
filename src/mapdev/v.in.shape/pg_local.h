/******************************************************************************
 * pg_local.h
 * protoptypes for local functions required for PostgreSQL processing.

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 11th. Mar. 2000
 * Last updated 11th. Mar. 2000
 *

* This file is part of GRASS GIS. It is free software. You can 
* redistribute it and/or modify it under the terms of 
* the GNU General Public License as published by the Free Software
* Foundation; either version 2 of the License, or (at your option)
* any later version.

* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.

 ******************************************************************************/
 

int FielddDumpASCII(fieldDescript *fd1, FILE *fp, const int nfields);
int PgDumpFromFieldD( fieldDescript *fd1, int normal_user,
		      const int nfields, const char *table_name );
int PgDumpFromDBF (char *infile, int normal_user);
int DBFDumpASCII(DBFHandle psDBF, FILE *fp);

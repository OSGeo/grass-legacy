What Are Vector Data?

In GRASS vector format, area, linear, and point features are represented
as the series of \arcs\ forming their perimeters.  An arc (aka, \vector\)
is stored as a series of x,y coordinate pairs.  The two end-points of an
arc are called \nodes\.  Two consecutive x,y pairs define an arc segment
(aka, line segment).

Arcs, either singly or in combination with other arcs, form higher-level
\map features\ like linear features (e.g., roads or streams) and area
features (e.g., soil types or timber cutting units).  Arcs that form
linear features are sometimes called \lines\, and arcs that outline areas
are called \area\ edges or area lines.  Areas are also called \polygons\.
In GRASS vector format, polygons are not stored explictly;  they are
reconstructed by finding the series of arcs that form their perimeters.

A GRASS vector data file is generally associated with other GRASS
supporting files describing category values, category labels, and file
topology.  Together, these files constitute a vector \map layer\.  Read
the information under "GRASS Database Directory Structure," located back
at the menu, if you wish further information on the location and content
of vector data and supporting files.

Typically, data are \digitized\, imported, and exported in vector format
and are analyzed in their raster format.  Data can be displayed and
output in either format.



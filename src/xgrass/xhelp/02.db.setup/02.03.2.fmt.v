What Are Vector Data?

In GRASS vector format, area, linear, and point features are represented
as the series of @glossary("arcs",16.glossary/arc.def) forming their perimeters.  An arc (aka, @glossary("vector",16.glossary/vector.def))
is stored as a series of x,y coordinate pairs.  The two end-points of an
arc are called @glossary("nodes",16.glossary/node.def).  Two consecutive x,y pairs define an arc segment
(aka, line segment).

Arcs, either singly or in combination with other arcs, form higher-level
@glossary("map features",16.glossary/mapfeat.def) like linear features (e.g., roads or streams) and area
features (e.g., soil types or timber cutting units).  Arcs that form
linear features are sometimes called @glossary("lines",16.glossary/line.def), and arcs that outline areas
are called @glossary("area",16.glossary/area.def) edges or area lines.  Areas are also called @glossary("polygons",16.glossary/polygon.def).
In GRASS vector format, polygons are not stored explictly;  they are
reconstructed by finding the series of arcs that form their perimeters.

A GRASS vector data file is generally associated with other GRASS
supporting files describing category values, category labels, and file
topology.  Together, these files constitute a vector @glossary("map layer",16.glossary/maplayer.def).  Read
the information under "GRASS Database Directory Structure," located back
at the menu, if you wish further information on the location and content
of vector data and supporting files.

Typically, data are @glossary("digitized",16.glossary/digitize.def), imported, and exported in vector format
and are analyzed in their raster format.  Data can be displayed and
output in either format.



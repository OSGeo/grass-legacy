                             RESOLUTION

In raster data format, data are stored on a grid containing rectangular
@glossary("cells",16.glossary/cell.def) of uniform size.  The cell is the smallest unit in which
information can be stored.  Cell dimensions are defined by the user.

Each cell within the grid can be assigned a data value and a
@glossary("category label",16.glossary/catlab.def) describing the type of data found there.  Because each
cell covers an area rather than a single point, this data value may be
a generalization of all the actual data values found within this area on
the earth.  Whether the data values assigned to each cell represent a
generalization, or represent all of the actual data found within the
cell, will depend on the cell's dimensions and the specific nature of
the data.  The user should define cell dimensions carefully.

Grid cell size is linked to map @glossary("scale",16.glossary/scale.def) and @glossary("resolution",16.glossary/res.def).  Smaller grid
cell size allows more data to be stored for a given geographic @glossary("area",16.glossary/area.def).
Therefore, smaller grid cells are associated with a larger @glossary("scale",16.glossary/scale.def) and
a finer @glossary("resolution",16.glossary/res.def) than are larger grid cells. The cell resolution
represents the size (area) of the piece of the earth to be represented
by one cell in a raster map layer.

For a graphic example illustrating the impact of different cell
resolutions, see @ref("Raster Map Layer Resolution: An Example",02.db.setup/02.07.1.sample).

Each cell is also associated with a specific geographic x,y coordinate
location.  In GRASS, the user specifies cell dimensions in the x
(east-west) and y (north-south) directions, for each map.  It is
permissible to make the east-west resolution different from the
north-south resolution.

Once the resolution of a new raster map layer is set, the geographic
information available in that map layer is set.  The user may subsequently
use the GRASS @man("g.region") command to change the working resolution of a
raster map layer, but the original resolution of the map remains intact
and available to the user regardless of any resampling performed as a
result of changes in the cell resolution designation in the current
geographic region.  Increasing the resolution of an existing raster
map layer will only subdivide the information from the existing cells.
It is important to understand that increasing the cell resolution of
an existing raster map layer cannot actually access more data than were
originally present.

For further information read:
@ref("Cartographic Issues in the Development of a Digital GRASS Database",18.biblio/sr.n-90.16).


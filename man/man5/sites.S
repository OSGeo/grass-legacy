.TH sites.S
.SH NAME
\fIsites.S\fR \- Description of the GRASS \fIs.menu\fR "S" interface option.
.SH DESCRIPTION
This describes the GRASS \fIs.menu\fR "S" statistical package
interface option.  See also the manual entry for \fIs.menu\fR.
.SH SPECIAL ADJUSTMENT
Due to the fact that "S" references all subscripts beginning
with 1, and the GIS data begins with 0, it was necessary to
add 1 to the category numbers.  Therefore, category value 0
("no data") becomes category value 1, category 1 becomes category 2,
etc.
.SH "S DATA STRUCTURES"
Below are descriptions of the data structures created by the interface.
As a general comment, within "S", simply typing the data structure
name will display the values contained in the structure.
You may also find it interesting to display the data structures
using the "S" function dput().
.SH "cat.histo, (cat.#.histo)"
This structure contains the histogram for the categories in each
map layer.
The histogram contains the number of cells of each category
which occur in the user's geographic region (see wind.n, et al., below).
It is a 2-dimensional integer array (matrix).
The first subscript references the map layer.
The second subscript references the category value.

examples:
.in +3

.nf
cat.histo[2,5] is the cell count for layer 2, category 5.
cat.histo[2,]  is the full histogram for layer 2.
.in -3
.fi

Note:  since more than one layer may occur in the data, and the
number of categories in each layer varies, it was necessary to
create this structure with sufficient dimension to hold the
largest category value for all layers. 
Histogram data for categories which do not occur
are set to NA ("S" notation for "no data").
However, there is also an individual histogram structure
for each layer:  cat.1.histo, cat.2.histo, etc.
These are simple vectors.

.SH cat.name
This structure contains the names for the categories in each
map layer.
It is a 2-dimensional character array (matrix).
The first subscript references the map layer.
The second subscript references the category.

Examples:
.in +3

.nf
cat.name[2,5] is the name of category 5 for layer 2.
cat.name[2,]  are all category names for layer 2.
.in -3
.fi

Note:  since more than one layer may occur in the data, and the
number of categories in each layer varies, it was necessary to
create this structure with sufficient dimension to hold the
largest category value for all layers.
Names for categories which do not occur are set to ''.
However, there is also an
individual category name structure
for each layer:  cat.1.name, cat.2.name, etc.
These are simple vectors.
.SH site.data
This structure contains the data for each site.
It is a 3-dimensional integer array.
The first subscript references the categories
which occurred at the site.
The second subscript references the site.
The third subscript references the layer.

Examples:
.in +3

.nf
site.data[,5,3] is the data for site 5 in layer 3.
site.data[,2,]  is the data for site 2 in all layers.
site.data[,,1]  is all site data for layer 1.
.in -3
.fi

Note: the size of the first dimension will be the number of cells in
a site,
the size of the second dimension will be the number of sites,
the size of the third dimension will be the number of layers.
.SH site.mode
Since "S" does not provide a statistical mode function, this structure
contains the most frequently occurring category for each site in each layer.
It is a 2-dimensional integer array (matrix).
The first subscript references the site.
The second subscript references the layer.

Examples:
.in +3

.nf
site.mode[5,3] is the mode for site 5 in layer 3.
site.mode[2,]  are the modes for site 2 in all layers.
site.mode[,1]  are all site modes for layer 1.
.in -3
.fi

Note:  this 'mode' is not the strict definition of the mode.
Since category 0 (which in "S" is category 1)
represents "no data" in the GIS databases,
it was excluded from the mode calculations
(essentially as if it had been NA).
For example, if the data for a site are
1 1 1 1 2 2 3 2 3 1 1, the mode will be 2.
.SH layer.name
This structure contains the raster map layer names.
It is a 2-dimensional array (i.e., a matrix).
The first subscript references the map layer.
The second subscript selects either the map layer name,
or the map layer title.


Examples:
.in +3

.nf
layer.name[3,1] is the name of layer 3.
layer.name[3]   is also the name of layer 3.
layer.name[2]   is the name of layer 2.
layer.name[3,2] is the title for layer 3.
.in -3
.fi

.SH location
This is a simple character vector giving the GRASS location
from which the data was extracted.

.SH mapset
This is a simple character vector giving the GRASS mapset.

.SH nlayers
This is a simple integer giving the number of map layers.

.SH nsites
This is a simple integer giving the number of sites.
.SH site.e
This is a simple integer vector giving the geographic easting for each site.
.SH site.n
This is a simple integer vector giving the geographic northing for each
site.
.SH site.name
This is a simple character vector giving the description for each
site.
.SH sitelist
This is a simple character vector giving the
name and description of the site lists file from which the sites
were taken.
.SH wind.n, wind.s, wind.w, wind.e
These are simple real numbers giving the
north, south, west, east
of the mapset's current geographic region.
.SH wind.res
This is a simple real number giving the
GRASS database resolution (in meters).
.sp 3
.SH "S MACROS"
You may find the following "S" macros helpful when referencing
the 'site.data' and 'site.mode' structures,
since they allow you to specify parameters:


.nf
MACRO site.data(site, layer)
({
	site.data[ , site, layer]
})
END

MACRO site.mode(site, layer)
({
	site.mode[site, layer]
})
END
.fi
.SH MACRO USAGE
To select the site.data for all sites for layer 2:

.ti +5
?site.data(layer=2)

to select the site.data for site 4 in all layers:

.ti +5
?site.data(site=4)

to select the site.mode for site 10 in layer 1:

.ti +5
?site.mode(layer=1,site=10)

.SH "SORRY, BUT ..."
These macros are not provided by the interface.
To use these macros, you will have to
type them into a text file and then,
from "S", issue the command:

.ti +5
define("<file>")
.SH "SEE ALSO"
.I s.db.rim,
.I s.in.ascii,
.I s.menu,
.I s.out.ascii,
.I s.surf.idw,
.I sites.format,
.I sites.occur,
.I sites.report


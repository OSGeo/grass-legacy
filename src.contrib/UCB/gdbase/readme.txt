gdbase is a data query system for use with GRASS map layers.  It uses
simple ASCII text files for data, and creates new GRASS map layers
based on information stored in the data files.  It was written by Sue
Huse with assistance by James Ganong at University of California,
Berkeley.


For questions or problems contact:

Sue Huse
Center for Environmental Design Research
390 Wurster Hall
University of California
Berkeley, California 94720
sue@ced.berkeley.edu

INSTALLATION

The software is distributed is a compressed tar file called gdbase.tar.Z.
To expand the file: 

gunzip gdbase.tar.gz | tar -xvf -

This will create a directory called grass.gdbase in the current directory.
In addition to the gdbase tools, s.reclass is needed for use by
s.gdbase and is included in gdbase.tar.gz

Use gmake4.1 or gmake4.2 to install gdbase into your GRASS system.



INSTRUCTIONS FOR USE

To use the gdbase tools, you must have an executable version of gawk (the 
public domain version of awk) in your path.  

To use the interactive version of gdbase, you must also have the tcltk
program wish in your path.

The gdbase commands are:
r.gdbase, v.gdbase and s.gdbase.  Based on the analysis option, each
can either reclass a file based on specified rules (option reclass) or
create a reclass based on unique combinations of specified fields
(option unique).  The display option works similar to the reclass, but
automatically displays the raster as an overlay to your current
monitor (you may use the d.erase button for cleaning), and does not 
save the raster map.


Database file:

Database files are to be placed in a dbase/ element directory in $LOCATION
(i.e., parallel to cell/ dig/, etc).  gdbase will find all dbase files
within you current mapset search path.

Database files are created independently of gdbase.  They are colon
delimited text files.  
Line 1 is a user prompt for the name of map layers appropriate for use 
with the data.  The database file can have any name. It does not need 
to follow the same naming conventions as the cell and dig files.  
The second line will be the exact data field names.  Field names cannot
include spaces or colons.  The first field name (and column) must be 
the attribute number of the corresponding grass maplayer.  The second 
column should be the category name.  All subsequent columns are user
defined.  There is no specific requirement for number of columns or
for size of columns, although there may be limitations to gawks ability
to handle excessive strings.  
All following lines are data records.  One line represents one GRASS
category.  You do not have to include a line for all categories.  The
lines should be the att number, cat name and then the data fields, 
once again separated by colons.  Data fields CAN have spaces, although
the field names can not.

Example:

raster and vector layers: groundwater@PERMANENT
att:cat:county:depth_to_gw:municipal_uses:agricultural_uses:
1:Petaluma Valley:Sonoma:20:1:0
2:Suisun-Fairfield Valley:Solano:15:0:1
3:Castro Valley:Alameda:30:1:1
10:Merced Valley:San Mateo:25:1:1


Invoking *.gdbase commands:
To use gdbase interactively, type the command without arguments.  A
TCLTK window will appear in which you set your input files and the
create the query rules.  These include an existing database file, an
existing raster map that is to be reclassed (should be the map that
corresponds to the database file), and the name for the new reclass
map.  You have the option of saving the rules to a file for future
use.  You may have any number of query rules, each of which will become
a new category in the output reclass file. Specify an output category
label and number for each rule.

To use db.* non-interactively the syntax is:

r.gdbase input=existing_raster_file output=new_reclass_file database=dbase_file rules=rules_file analysis=[display|reclass|unique].  

You may substitute "i" for input, "o" for output, etc.  The rules
file must be in your current directory, or you must specify its path.
The rules file should be set up on sets of three lines.  The first
of a set is the new attribute number, the second is the new category
name, and the third is the query rule.  Blank lines between triplets
are optional.

Example rules file:
1
Alameda County
county = "Alameda"

2
deeper than 15 feet
depth_to_gw > 15


Query Syntax:
The syntax consists of basic logic statements.  Available operators are:

and or not < > = + - / * ( )
The data field names must be spelled exactly as they appear in the dbase
file.  Text fields must also be spelled exactly.

To query a text field, the text must be surrounded by double quotes:
county = "Alameda" 
or 
cat = "Suisun-Fairfield Valley"

The numerical operators should be in standard notation:
depth_to_gw = 15
or 
agricultural_uses > 0

More complicated queries can be created using and, or, not and parentheses:
(county = "Alameda" or county = "San Mateo") and depth_to_gw > 25.


NOTE:  Be careful if your individual query rules overlap.  For instance
if your new category 1 is "Alameda" and you new category 2 is
depth_to_gw > 20.  Both these will find groundwater basin 3.  In these
cases the later rules will take precedence over all previous rules.
This is because gdbase invokes GRASS reclass programs for which that is
the priority.

The unique analysis works in a different way from the other commands.  
Rather than specifying a query rule, the user specifies the fields on
which to sort.  gdb.unique will then create a new map showing the unique
combinations of those fields.  For instance in the groundwater database 
above, specifying county would yield 4 output raster categories, one for 
each county.  Specifying the municipal_uses would yield only one new 
category.  Specifying municipal_uses and agricultural_uses would yield 
3 new categories (1:0, 0:1, 1:1).

The query syntax is one line listing the field names separated by
spaces.  The command-line version requires a one line rules file
as well.  If you want to sort on all columns, except the att and
cat columns, simply use "all" rather than typing each field name

Examples:
county
municipal_uses agricultural_uses
all

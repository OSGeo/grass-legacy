
char *helptext[] =
{ "   RESET LOCATION: You must enter yes, y, no, or n in this field.\n\
   If  you enter yes or y then, once you hit the <ESC> key, you will\n\
   be allowed to use the mouse-pointer to indicate  a  new  location\n\
   for  the  label.  If you enter no or n then you will not be given\n\
   the chance to indicate a new location.",

"   LOCATION, Northing Coordinate: This field is the northing  coor-\n\
   dinate  of  the labels location.  This field is usually filled-in\n\
   by a coordinate from the mouse-pointer,  but  since  you  entered\n\
   help  in this field you have wiped out the northing coordinate of\n\
   the label.  You may enter a new  northing  coordinate  by  typing\n\
   one,  or by entering yes in the RESET LOCATION field, hitting the\n\
   <ESC> key, and clicking on the desired location with  the  mouse-\n\
   pointer.",

"   LOCATION, Easting Coordinate: This field is the easting  coordi-\n\
   nate  of the labels location.  This field is usually filled-in by\n\
   a coordinate from the mouse-pointer, but since you  entered  help\n\
   in  this  field  you have wiped out the easting coordinate of the\n\
   label.  You may enter a new easting coordinate by typing,  or  by\n\
   entering  yes in the RESET LOCATION field, hitting the <ESC> key,\n\
   and clicking on the desired location with the mouse-pointer.",

"   PLACEMENT: This field determines which  part  of  the  label  to\n\
   which the location field refers.  This may be specified as:\n\n\
      lower left     (lower left corner of the text)\n\
      lower right    (lower right corner of the text)\n\
      lower center   (bottom center of the text)\n\
      upper left     (upper left corner of the text)\n\
      upper right    (upper right corner of the text)\n\
      upper center   (top center of the text)\n\
      center         (center of the text)",

"   LOCATION, X-Offset: This value is optional.  The X-Offset allows\n\
   a finer adjustment of label placement.  The X-Offset will shift\n\
   the label position east, if positive, or west, if negative.", 

"   LOCATION, Y-Offset: This value is optional.  The Y-Offset allows\n\
   a finer adjustment of label placement.  The Y-Offset will shift\n\
   the label position south, if positive, or north, if negative.", 

"   TEXT: You can enter up to four  lines  of  text  in  these  four\n\
   fields.   For more information on moving around the fields of the\n\
   data entry screen see the manual entry for VASK.",
   
"   SKIP: You must enter yes, y, no, or n in  this  field.   If  you\n\
   enter yes or y then the label information that you enter into the\n\
   data entry screen will be retained, but will not be displayed  on\n\
   the  screen or in the output of the Pmap, paint, or Dpaint.labels\n\
   programs.  This field allows you to temporarily turn off some la-\n\
   bels, if you so desire.",
  
"   FONT: This field determines the font to be used for the label text.\n\
   Following is a list of valid fonts names:\n\n\
      cyrilc      gothgbt      gothgrt      gothitt     greekc\n\
      greekcs     greekp       greeks       italicc     italiccs\n\
      italict     romanc       romancs      romand      romanp\n\
      romans      romant       scriptc      scripts",

"   TEXT SIZE: This field determines the size of  the  letters.  The\n\
   size  specifies  the  vertical height of the letters in meters on\n\
   the ground. Thus text will grow or shrink depending on the  scale\n\
   at which the map is drawn.",

"   TEXT WIDTH: This field determines  the  line  thickness  of  the\n\
   letters.   The  normal  text  width  should  be set to 1.  Larger\n\
   numbers  can  be  used  to  simulate  bold  face.  (d.labels  and\n\
   Dpaint.labels ignore this value and always uses 1, but the speci-\n\
   fied width will show up in the output of paint and Pmap).",
   
"   HIGHLIGHT WIDTH: Specifies how far from the text lines (in  pix-\n\
   els) the highlight color should extend.  Highlight colors are not\n\
   shown on the graphics display, they are only shown in the  output\n\
   out paint and Pmap.",

"   TEXT COLOR: This selects the text  color.  In the d.labels program\n\
   colors can be specified in four different ways:\n\n\
   1) By color name:    aqua      black     blue    brown    cyan\n\
                        gray      green     grey    indigo   magenta\n\
                        orange    purple    red     violet   white\n\
                        yellow\n\n\
   2) As red, green,  blue  percentages. For example: .5 .4 .7\n\
      (This form is not supported by the d.labels  or  Dpaint.labels\n\
      programs but will show up in the output of the paint or Pmap\n\
      programs.)\n\n\
   3) By printer color number to get the exact printer color.\n\
      (This form is not supported by the d.labels  or  Dpaint.labels\n\
      programs but will show up in the output of the paint or Pmap\n\
      programs.)\n\n\
   4) Specify none to suppress the lettering.",
   
"   HIGHLIGHT COLOR: The text can be highlighted in another color so\n\
   that  it  appears to be in two colors. The text is drawn first in\n\
   this color at a wider line width, and then redrawn  in  the  text\n\
   color  at the regular line width.  Highlight colors are not shown\n\
   on the graphics display, the programs d.label  and  Dpaint.labels\n\
   ignore  the  highlight color, but they are shown in the output of\n\
   the paint and Pmap programs.  See the help-text for the TEXT COLOR\n\
   field for information on specifying a color.", 

"   BACKGROUND COLOR: Text may be boxed in a solid color by specify-\n\
   ing a background color (see TEXT COLOR above to specify a color).\n\
   Specify none for no background.",

"   BORDER COLOR: Select a color for the  border  around  the  back-\n\
   ground.  Specify none to suppress the border.",

"   OPAQUE TO VECTORS: yes/no. This field  only  has  meaning  if no\n\
   background color is selected.  yes will prevent vector lines from\n\
   entering the background.  no will allow vector lines to enter the\n\
   background." };

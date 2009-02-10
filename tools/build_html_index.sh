#!/bin/sh

#generates HTML man pages docs/html/index.html
# Markus Neteler, 2003, 2004, 2005, 2006


## TODO: better fix this in include/Make/Html.make, see bug #5361

#exclude following list of modules from help index:
EXCLUDEMODS="\
v.topo.check \
i.ask \
i.find \
helptext.html \
r.watershed.ram \
r.watershed.seg \
gis.m \
wxGUI.* \
d.paint.labels \
p.out.vrml \
r.cats"

# these modules don't use G_parser()
check_for_desc_override()
{
    case "$BASENAME" in
	g.parser)
	    SHORTDESC="Provides automated parser, GUI, and help support for GRASS scipts."
	    ;;
	r.mapcalc)
	    SHORTDESC="Raster map calculator."
	    ;;
	r.li.daemon)
	    SHORTDESC="Support module for r.li landscape index calculations."
	    ;;
    esac
}


############# nothing to configure below ############

# regex-proof module skip list
EXCLUDEHTML="`echo "$EXCLUDEMODS" | tr ' ' '|' | sed -e 's+\.+\\\\.+g' -e 's+|+\\\\|+g'`"

#fetch the ARCH for store the files:
ARCH="`cat ../include/Make/Platform.make | grep '^ARCH'  | sed 's+ ++g' | cut -d'=' -f2`"
GEMDIR="../gem"
HTMLDIR="../dist.$ARCH/docs/html"
GRASSVERSION=`cat ../dist.$ARCH/etc/VERSIONNUMBER`
GRASS_MMVER=`cut -d . -f 1-2 ../dist.$ARCH/etc/VERSIONNUMBER`
MACOSX=`echo $ARCH | grep -i darwin`


write_html_header()
{
# $1: filename
# $2: page title
# $3: is it main index

echo "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">
<html>
<head>
 <title>$2</title>
 <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">
 <meta name=\"Author\" content=\"GRASS Development Team\">" > "$1"
if [ "$3" ] && [ "$MACOSX" ] ; then
	echo " <meta name=\"AppleTitle\" content=\"GRASS GIS $GRASSVERSION Help\">
 <meta name=\"AppleIcon\" content=\"GRASS-$GRASS_MMVER/grass_icon.png\">
 <meta name=\"robots\" content=\"anchors\">" >> "$1"
fi
echo " <link rel=\"stylesheet\" href=\"grassdocs.css\" type=\"text/css\">
</head>
<body bgcolor=\"#FFFFFF\">

<!-- this file is generated by tools/build_html_index.sh -->

<img src=\"grass_logo.png\" alt=\"GRASS logo\"><hr align=center size=6 noshade>

<h2>GRASS GIS $GRASSVERSION Reference Manual</h2>

<p>
<em>Geographic Resources Analysis Support System</em>, commonly referred to as
<a href=\"http://grass.osgeo.org\">GRASS</a>, 
is a Geographic Information System (GIS) used for geospatial data management
and analysis, image processing, graphics/maps production, spatial modeling,
and visualization. GRASS is currently used in academic and commercial settings
around the world, as well as by many governmental agencies and environmental
consulting companies.</p>

<p>This reference manual details the use of modules distributed with
Geographic Resources Analysis Support System (GRASS), an open source (GNU
GPL'ed), image processing and geographic information system (GIS).</p>

<BR><BR>
<table align="center">
<tr>
<td valign=\"top\">
<h3>Quick Introduction</h3>

<!-- the files grass6.html & helptext.html file live in lib/init/ -->
<ul>
<li><a href=\"grass6.html\">GRASS startup manual page</a></li>
<li><a href=\"helptext.html\">How to start with GRASS</a></li>
<li>Introductions
 <ul>
  <li><a href=\"rasterintro.html\">2D raster map processing</a></li>
  <li><a href=\"raster3dintro.html\">3D raster map (voxel) processing</a></li>
  <li><a href=\"imageryintro.html\">Image processing</a></li>
  <li><a href=\"databaseintro.html\">Database management</a></li>
  <li><a href=\"vectorintro.html\">Vector map processing and network analysis</a></li>
  <li><a href=\"projectionintro.html\">Projections and spatial transformations</a></li>
  <li><a href=\"sql.html\">SQL support</a></li>
  <li><a href=\"displaydrivers.html\">Display drivers</a></li>
  <li><a href=\"variables.html\">GRASS variables and environment variables</a></li>
 </ul>
 <li><a href=\"gem/index.html\">The GRASS Extensions Manager (GEM)</a></li>
</ul>
<P>
</td>

" >> "$1"
}

write_html_footer()
{
# $1: filename
# $2: help index url
echo "<BR><BR>
<hr>
<p><a href=\"$2\">Help Index</a> | <a href=\"full_index.html\">Full Index</a><br>
&copy; 2003-2009 <a href=\"http://grass.osgeo.org\">GRASS Development Team</a></p>
</body>
</html>" >> "$1"
}

expand_module_class_name()
{
# $1: module class
 if [ "$1" = "d" ]  ; then 
    echo "display"  
  elif [ "$1" = "db" ] ; then 
    echo "database" 
  elif [ "$1" = "g" ]  ; then 
    echo "general" 
  elif [ "$1" = "i" ]  ; then 
    echo "imagery" 
  elif [ "$1" = "m" ]  ; then 
    echo "misc" 
  elif [ "$1" = "ps" ] ; then 
    echo "postscript" 
  elif [ "$1" = "p" ]  ; then 
    echo "paint" 
  elif [ "$1" = "r" ]  ; then 
    echo "raster" 
  elif [ "$1" = "r3" ]  ; then 
    echo "raster3D" 
  elif [ "$1" = "s" ]  ; then 
    echo "sites" 
  elif [ "$1" = "v" ]  ; then 
    echo "vector" 
  else 
    echo "$1"
 fi
}

#are we in the tools/ dir?
ls build_html_index.sh 2> /dev/null
if [ $? -eq 1 ] ; then
 echo "ERROR: this script must be run from the tools/ directory"
 exit 1
fi

FULLINDEX=full_index.html

################

#copy over CSS:
cp -f grassdocs.css $HTMLDIR/
mkdir -p $HTMLDIR/nviz/
cp -f grassdocs.css $HTMLDIR/nviz/
chmod a+r $HTMLDIR/grassdocs.css $HTMLDIR/nviz/grassdocs.css
#copy over GRASS logo:
cp -f grass_logo.png $HTMLDIR/
cp -f grass_logo.png $HTMLDIR/nviz/
chmod a+r $HTMLDIR/grass_logo.png $HTMLDIR/nviz/grass_logo.png
if [ "$MACOSX" ] ; then
  cp -f grass_icon.png $HTMLDIR/
  chmod a+r $HTMLDIR/grass_icon.png
fi
#copy over GEM docs:
mkdir -p $HTMLDIR/gem
cp -f $GEMDIR/docs/GEM-Manual/*.html $HTMLDIR/gem/
cp -f $GEMDIR/docs/GEM-Manual/img* $HTMLDIR/gem/
chmod a+r $HTMLDIR/gem/*.html $HTMLDIR/gem/img*

#process all HTML pages:
cd $HTMLDIR

#get list of available GRASS modules:
CMDLIST=`ls -1 *.*.html | grep -v "$FULLINDEX" | grep -v index.html | \
  grep -v gis.m.html | grep -v "\($EXCLUDEHTML\)" | cut -d'.' -f1 | sort -u | \
  grep -v photo`
CMDLISTNO=`echo $CMDLIST | wc -w | awk '{print $1}'`

#write main index:
echo "Generating HTML manual pages index (help system)..."
write_html_header "$FULLINDEX" "GRASS GIS $GRASSVERSION Reference Manual: Full index"
echo "</tr></table>" >> "$FULLINDEX"
echo "<BR><h3>Full command index:</h3>" >> "$FULLINDEX"
echo "<table border=0>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;d.*  </td><td>display commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;db.* </td><td>database commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;g.*  </td><td>general commands</td></tr>" >> "$FULLINDEX"
#echo "<tr><td>&nbsp;&nbsp;g3.* </td><td>general3D commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;i.*  </td><td>imagery commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;m.*  </td><td>miscellaneous commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;ps.* </td><td>postscript commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;r.*  </td><td>raster commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;r3.* </td><td>raster3D commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;v.*  </td><td>vector commands</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;<a href=\"gis.m.html\">gis.m</a> </td><td>GUI frontend to GIS menus and display</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;<a href=\"nviz.html\">nviz</a> </td><td>visualization suite</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;<a href=\"wxGUI.html\">wxGUI</a> </td><td>wxPython-based GUI frontend</td></tr>" >> "$FULLINDEX"
echo "<tr><td>&nbsp;&nbsp;<a href=\"xganim.html\">xganim</a> </td><td>raster map slideshow</td></tr>" >> "$FULLINDEX"
echo "</table>" >> "$FULLINDEX"

#generate main index of all modules:
echo "<BR><BR>[ " >> "$FULLINDEX"
k=0
for i in $CMDLIST
do
  k=`expr $k + 1`
  echo -n "<b><a href=\"#$i\">$i.*</a></b>" >> "$FULLINDEX"
  if [ $k -lt $CMDLISTNO ] ; then
     echo -n " | " >> "$FULLINDEX"
  fi
done

echo " ]
<BR>
" >> "$FULLINDEX"


#for all module groups:
for i in $CMDLIST
do 
  echo "<a name=\"$i\"></a>" >> "$FULLINDEX"
  echo "<BR><BR><h3>$i.* commands:</h3>" >> "$FULLINDEX"
  echo "<table>" >> "$FULLINDEX"

  #for all modules:  
  for i in `ls -1 $i.*.html | grep -v "\($EXCLUDEHTML\)"`
  do
    BASENAME=`basename $i .html`
    SHORTDESC="`cat $i | awk '/NAME/,/SYNOPSIS/' | grep '<em>' | cut -d'-' -f2- | sed 's+^ ++g' | grep -vi 'SYNOPSIS' | head -n 1`"
    check_for_desc_override
    echo "<tr><td valign="top"><a href=\"$i\">$BASENAME</a></td> <td>$SHORTDESC</td></tr>" >> "$FULLINDEX"
  done
  echo "</table>" >> "$FULLINDEX"
done

write_html_footer "$FULLINDEX" index.html
# done full index

#next write separate module pages:
#for all module groups:
for k in $CMDLIST
do 
  MODCLASS=`expand_module_class_name $k`
  FILENAME=$MODCLASS.html

  write_html_header "$FILENAME" "GRASS GIS $GRASSVERSION Reference Manual: $MODCLASS"
  echo "</tr></table>"  >> "$FILENAME"

  echo "<b>$MODCLASS commands:</b>" >> "$FILENAME"
  echo "<table>" >> "$FILENAME"
  #for all modules:
  for k in `ls -1 $k.*.html | grep -v "\($EXCLUDEHTML\)"`
  do
    BASENAME=`basename $k .html`
    SHORTDESC="`cat $k | awk '/NAME/,/SYNOPSIS/' | grep '<em>' | cut -d'-' -f2- | sed 's+^ ++g' | grep -vi 'SYNOPSIS' | head -n 1`"
    check_for_desc_override
    echo "<tr><td valign="top"><a href=\"$k\">$BASENAME</a></td> <td>$SHORTDESC</td></tr>" >> "$FILENAME"
  done
  
  echo "</table>" >> "$FILENAME"

  write_html_footer "$FILENAME" index.html
done

#next write main page:
FILENAME=index.html
write_html_header "$FILENAME" "GRASS GIS $GRASSVERSION Reference Manual" 1

#modules:
echo "<td valign=\"top\">" >> "$FILENAME"
echo "<h3>Manual sections:</h3>" >> "$FILENAME"
echo "<ul>" >> "$FILENAME"
#for all module groups:
for k in $CMDLIST
do 
  MODCLASS=`expand_module_class_name $k`
  echo "<li><a href=\"$MODCLASS.html\">$MODCLASS commands</a></li>" >> "$FILENAME"
done

#extra stuff for 'nviz' and 'xganim' and GUIs:
echo "<li><a href=\"gis.m.html\">gis.m</a> and <a href=\"d.m.html\">d.m</a> GIS managers</li>" >> "$FILENAME"
echo "<li><a href=\"nviz.html\">nviz</a> visualization and animation tool</li>" >> "$FILENAME"
echo "<li><a href=\"wxGUI.html\">wxGUI</a> wxPython-based GUI frontend</li>" >> "$FILENAME"
echo "<li><a href=\"xganim.html\">xganim</a> tool  for animating a raster map series</li>" >> "$FILENAME"
	      
echo "</ul>" >> "$FILENAME"
echo "</td></tr>" >> "$FILENAME"
echo "</table>" >> "$FILENAME"

#insert a special comment so that GEM will know where to merge docs of extensions
echo >> "$FILENAME"
echo "<!-- GEM Extensions StartHTML. Do not delete or change this comment! -->" >> "$FILENAME"
echo >> "$FILENAME"


#############
write_html_footer "$FILENAME" index.html

#############
echo "Generated HTML docs in $HTMLDIR/index.html"
echo "----------------------------------------------------------------------"
echo "Following modules are missing the 'description.html' file in src code:"
for i in `find . -name "*.*.html" | sort | grep -v "$FULLINDEX"  | grep -v 'index.html' | grep -v "$EXCLUDEHTML"`
do
  if grep 'DESCRIPTION' $i >/dev/null 2>&1 ; then 
    :
  else
    echo `echo $i | sed 's?./??' | sed 's/.html//'`
  fi
done
echo "----------------------------------------------------------------------"

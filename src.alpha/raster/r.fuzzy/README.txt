


r.fuz programs

DELCLAUX François
UMR HydroSciences Montpellier 
IRD
BP 5045  34032 Montpellier Cedex    FRANCE

email : delclaux@mpl.ird.fr

-------------------------------------------------------------------------------------

Tested and updates for inclusion in GRASS 4.3 update package 1 by Bruce Byars,
GRASS Development Team, Baylor University.  January, 2001.  This is part of the
OFFICIAL GRASS 4.3 Distribution.


I. INTRODUCTION

r.fuz programs have been developed for integrating fuzzy treatments in the
GRASS GIS environment, and more specifically for introducing spatial fuzzy
rule-based modelling. The fuzzy package consists of a set of three modules.

First, the program r.fuz.set is used to fuzzify existing raster maps, i.e. to assign to
a raster layer one or several linguistic (fuzzy) variables, each one being defined by a
group of fuzzy sets. The second module, r.fuz.map, allows the user to create some
basic fuzzy information such as spatial distribution of membership function values,
fuzzy set intersection maps, etc. Lastly, the third program, r.fuz.rule, is the spatial
fuzzy rule model : it operates on each pixel of several input raster layers to produce
an output raster map according to a set of inference rules. These rules, which can be
considered as the result of an expert knowledge, are coded in the following way
(for example) :

IF (  [map1 is low]  AND [map2 is medium]  ) THEN  [map3 is high]

A full description of flags, parameters and inputs of the commands can be
found using the g.manual command in the GRASS environment.

Due to the history of the package development, these programs operate under
GRASS4 environment. Unfortunately, they are not planed to be upgraded towards
GRASS5 for a short while !



II.  DIRECTORY STRUCTURE

The r.fuz programs are stored in the r.fuzzy directory whose the structure is
 described as follows:



          |-data-------
          |
          |            |-include---
          |            |
          |-fuzzy------|-lib-------|-LIB.files-
          |            |
          |            |-src-------
          |
          |-htm--------
          |
          |-man--------|-man4------
          |
  r.fuzzy-|            
          |            |
          |-r.fuz.map--|-cmd-------
          |            |
          |            |-inter-----
          |
          |            
          |            |
          |-r.fuz.rule-|-cmd-------
          |            |
          |            |-inter-----
          |
          |            
          |            |
          |-r.fuz.set--|-cmd-------
          |            |
          |            |-inter-----
          |
          |-Gmakefile
          |-README.txt 
          |-clean.sh   


directory list

- data directory : it contains ASCII files (fuzzy variables, rules)
which can be used in the spearfish database in order to test the commands ;

- fuzzy directory : it contains the fuzzy library source files (src),
include  files (include) and library binary (lib). The library
libfuzzy.a is used at the time of r.fuz program linking ;

- man directory : it contains the man information of the r.fuz programs.
The source code is under man4 ;

- r.fuz.set, r.fuz.map and r.fuz.rule  directories : these
directories contain the program codes in the two user's modes for each 
command : interactive (inter) or command (cmd).

file list
- Gmakefile : the GRASS makefile, which can be found at each level
of the global file structure, contains the required information for 
compiling and building binaries ;

README.txt : this present file.

NOTE: OBJ.files directories will be created upong compilation and contain the
object modules resulting of the compilation for your system architecture 


III.  INSTALLATION

These compilation steps have been successfully tested on Linux (RedHat 6.0) 
and Sun (Solaris 2.6, 7, and 8).  Installation requires the following steps :

- copy r.fuzzy.tar.gz into your base GRASS source code directory ;

- run the commands  gunzip r.fuzzy.tar.gz and tar  xvf  r.fuzzy.tar
 This will place the source code in the src.alpha/raster/r.fuzzy directory
 for compilation

- run  the command  gmake4.3 (or similar) that is used to compile your
 GRASS programs. The binaries are directly copied in the
 $GISBASE/etc/bin/contrib/cmd and
 $GISBASE/etc/bin/contrib/inter directories. 

- run the MAKELINKS program to ensure that the new modules are linked and
 ready to use.

- copy the r.fuzzy/man/man4 files in the GRASS source man/man4
 directory ; in the directory GRASS source man, run the command
 gmake4.3 (or similar) to install the manual pages ;

- the first time a user runs r.fuz.set, a directory named fuzzy is created
 under the current MAPSET. So, for handling fuzzified raster maps with
 the g.copy, g.rename and g.remove commands, you have to update the
 element_list file in  $GISBASE/etc directory by adding a record
 such as fuzzy:fuzzy at the end of the cell paragraph.

element_list file in $GISBASE/etc

# @(#)Element_List	2.4  12/2/87
# this file specifies the database elements that 
# are processed by RENAME, REMOVE, COPY, and LIST
#
# format:
# main_element:alias:description:menu text
#    support_element:description
#
cell:rast:raster:raster files
  cellhd:header
  cats:category
  colr:color
  hist:history
  cell_misc:misc
  fuzzy:fuzzy
dig:vect:vector:binary vector files
  dig_att:attributes
  dig_plus:topology
  dig_cats:category
  dig_misc:misc
  reg:point registration
icons:icon:icon:paint icon files
paint/labels:labels:label:paint label files
site_lists:sites:site list:site list files
windows:region:region definition:region definition files
group:group:imagery group:imagery group files




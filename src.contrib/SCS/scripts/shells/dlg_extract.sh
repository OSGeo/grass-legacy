#          @(#)dlg_extract     jaf     26 Sep 1988
#
#   Program to manipulate and extract specific contiguous
#   files from a USGS DLG tape in "optional" format
#

DLG_DIR=dlg
STOP=0
START_POS=0
input=0
if [ -z "$GISRC" ]
then
   echo ""
   echo ""
   echo ""
   echo "GRASS3 not running !!!"
   echo ""
   echo ""
   echo ""
   exit 1
fi
clear
echo "
      USGS OPTIONAL FORMAT DLG Extract
      IF USING TAPE - 
          BE SURE TAPE IS MOUNTED AND ONLINE USING CORRECT BPI


      Enter Following Information as Printed on USGS Customer Report

"
until [ $input = 1 ]
do
$GISBASE/etc/echo -n  "Enter DEVICE or FILE_NAME for input : "
read DEVICE
if [ -r $DEVICE ]
then input=1
else echo "           DEVICE/FILE not available "
     input=0
     DEVICE=
fi
done
echo ""
$GISBASE/etc/echo -n  "Enter TOTAL NUMBER of FILES : "
read TOT_FILES
echo ""
$GISBASE/etc/echo -n  "Enter LRECL: "
read LRECL
echo ""
$GISBASE/etc/echo -n  "Enter BLKSIZE:  "
read BLOCKSIZE
until [ $STOP = 1 ] 
do
  clear
  $GISBASE/etc/echo -n  "Enter NUMBER OF FILE TO EXTRACT: "
  read FILENO
 if [ $FILENO -lt $START_POS ]
      then exit
 fi
 if [ $FILENO -gt $TOT_FILES ]
      then exit
 fi
  SKIPNO=`expr $FILENO - $START_POS - 1`
 if [ $SKIPNO -lt 0 ]
      then exit
 fi
  echo ""
  $GISBASE/etc/echo -n  "Enter No. of Records: "
  read RECNO
  echo ""
  $GISBASE/etc/echo -n  "Enter name for output file: "
  read OUTFILE
  if [ ! -d "$GISDBASE/$LOCATION_NAME/$MAPSET/$DLG_DIR" ]
      then mkdir $GISDBASE/$LOCATION_NAME/$MAPSET/$DLG_DIR;
  fi;
  echo "The parameters for the file transfer are: "
  echo "Skipping $SKIPNO FILES TO EXTRACT FILE NUMBER $FILENO"
  echo "LRECL=$LRECL   BLKSIZE=$BLOCKSIZE  Number of Records to read: $RECNO"
  echo ""
  echo ""
  $GISBASE/etc/echo -n  "ARE THESE CORRECT [y/n]: "
  read ANS
  case $ANS in
     y|Y) if [ $SKIPNO -gt 0 ];
           then mt fsf $SKIPNO
          fi ;;
       *) ;;
  esac
echo "  dd if=$DEVICE of=$GISDBASE/$LOCATION_NAME/$MAPSET/$DLG_DIR/$OUTFILE \
 ibs=$BLOCKSIZE cbs=$LRECL obs=$LRECL conv=addnl count=$RECNO"
  echo "DLG_EXTRACT Complete"
  $GISBASE/etc/echo -n  "Do you want to EXTRACT another FILE [y/n]: "
  read CONT
  case $CONT in
       y|Y) START_POS=$FILENO;;
       *) STOP=1;;
  esac
done 



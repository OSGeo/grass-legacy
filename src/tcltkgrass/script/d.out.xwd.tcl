#
#   written by Michael Barton 24 March 2004 
#
#	adds tcltkgrass module support to xwd output
#
#--------------------------------------------------
     

#run xwd

xwd -out `echo $2 | sed s/output=//`  $1

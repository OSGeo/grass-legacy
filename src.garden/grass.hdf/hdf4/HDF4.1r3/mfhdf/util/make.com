$! --------------------------------------------------------------------------
$! For making GETOPT.OBJ on VMS if you don't have MMS.
$! --------------------------------------------------------------------------
$!
$! $Id: make.com,v 1.1 1999-12-29 15:13:10 markus Exp $
$
$ ccc := cc /opt/nodebug/nolist
$
$ ccc GETOPT.C
$

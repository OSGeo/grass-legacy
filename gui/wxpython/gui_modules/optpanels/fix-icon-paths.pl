#!/usr/bin/perl

## Use this to change absolute icon paths in glade-generated code to portable stuff
## use like this: perl this_file glade-emitted.py > portable-equivalent.py

## Guesses the hardcoded path from the existence of a wxBitmap call

# Be careful below: make sure os is imported but don't do it twice
$prelude = <<ENDP;
if os.getenv("GRASS_ICONPATH"):
    icons = os.environ["GRASS_ICONPATH"]
    if icons[-1] != '/':
        icons.append('/')
else:
    icons = os.getenv("GISBASE") + "/etc/gui/icons/"
ENDP

$out="";
$glade_path="";
$has_import_os = 0;
$in_headers = 1;
while(<>){
  /import.*\bos\b/ and $has_import_os = 1;
  /^(class|def)/ and do{ 
    print "import os\n" if !$has_import_os;
    print $prelude if $in_headers;
    $in_headers = 0;
    };
  /wx.Bitmap\("([^"]+)",/ && $glade_path eq "" and do {
    $glade_path=$1;
    $glade_path =~ s|/[^/]+$|/|; # Strip off the filename
    };
  s|"$glade_path|icons+"|;
  print;
}

# --Daniel Calvelo 200703

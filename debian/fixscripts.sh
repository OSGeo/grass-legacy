#!/bin/sh

# this script tries to locate all the GRASS scripts than have something
# that makes lintian complain and fix them.

CURDIR=$(pwd)

# make these scripts executable
for x in etc/water/seg
do
    chmod +x $CURDIR/debian/tmp/usr/lib/grass/$x
done

# silence bogus lintian complaint about interpreter-not-absolute
for x in script_get_line \
    script_play \
    script_tools \
    script_file_tools \
    nviz2.2_script
do
  f=$CURDIR/debian/tmp/usr/lib/grass/etc/nviz2.2/scripts/$x
  sed 's.!nviz.!/usr/lib/grass/bin/nviz.' $f >foo && cat foo >$f
done

for x in panel_label.tcl \
    panel_mkdspf.tcl \
    panel_scale.tcl
do
  f=$CURDIR/debian/tmp/usr/lib/grass/etc/nviz2.2/scripts/$x
  sed 's%!../glnviz.new/nvwish%!/usr/lib/grass/etc/nviz2.2/glnviz/nvwish%' $f >foo && cat foo >$f
done
rm foo

# silence lintian warning executable-not-elf-or-script
# add shebang to scripts that need it
for x in etc/copy
do
  f=$CURDIR/debian/tmp/usr/lib/grass/$x
  cp $f foo; echo "#!/bin/sh" >$f; cat foo >>$f
done
rm foo

# silence executable-not-elf-or-script lintian warning
# most tcl scripts don't need to be executable
for x in etc/nviz2.2/scripts/panel_kanimator.tcl \
    etc/nviz2.2/scripts/panel_scale.tcl \
    etc/nviz2.2/scripts/structlib.tcl \
    etc/nviz2.2/scripts/panel_label.tcl \
    etc/nviz2.2/scripts/attPopup.tcl \
    etc/nviz2.2/scripts/attIsosurfPopup.tcl \
    etc/nviz2.2/scripts/panel_pos.tcl \
    etc/nviz2.2/scripts/panel_vol.tcl \
    etc/nviz2.2/scripts/panelIndex \
    etc/dm/labels.tcl \
    etc/dm/cmd.tcl \
    etc/dm/menu.tcl \
    etc/nviz2.2/scripts/colorPopup.tcl \
    etc/nviz2.2/scripts/multimapBrowser.tcl \
    etc/nviz2.2/scripts/panel_animation.tcl \
    etc/nviz2.2/scripts/script_support.tcl \
    etc/nviz2.2/scripts/panel_vect.tcl \
    etc/nviz2.2/scripts/panel_sdiff.tcl \
    etc/nviz2.2/scripts/assoc.tcl \
    etc/nviz2.2/scripts/unique.tcl \
    etc/nviz2.2/scripts/panel_main.tcl \
    etc/dm/grassabout.tcl \
    etc/nviz2.2/scripts/position_procs.tcl \
    etc/nviz2.2/scripts/panel_lights.tcl \
    etc/dm/d.m.tcl \
    etc/nviz2.2/scripts/panel_query.tcl \
    etc/v.digit/cats.tcl \
    etc/epsg_option.tcl \
    etc/nviz2.2/scripts/panel_query_orig.tcl \
    etc/nviz2.2/scripts/nviz_init.tcl \
    etc/dm/vector.tcl \
    etc/nviz2.2/scripts/cutplane_channels.tcl \
    etc/gis_set.tcl \
    etc/nviz2.2/scripts/panel_tst.tcl \
    etc/nviz2.2/scripts/tclIndex \
    etc/nviz2.2/scripts/send_support.tcl \
    etc/nviz2.2/scripts/panel_surf.tcl \
    etc/nviz2.2/scripts/widgets.tcl \
    etc/dm/group.tcl \
    etc/dm/tool.tcl \
    etc/nviz2.2/scripts/panel_color.tcl \
    etc/v.digit/toolbox.tcl \
    etc/nviz2.2/scripts/extra_bindings.tcl \
    etc/nviz2.2/scripts/panel_query_pg.tcl \
    etc/dm/print.tcl \
    etc/nviz2.2/scripts/panel_cutplane.tcl \
    etc/nviz2.2/scripts/panel_site.tcl \
    etc/nviz2.2/scripts/config.tcl \
    etc/nviz2.2/scripts/filemapBrowser.tcl \
    etc/dm/tree.tcl \
    etc/nviz2.2/scripts/mapBrowser.tcl \
    etc/nviz2.2/scripts/queue.tcl \
    etc/dm/raster.tcl \
    etc/v.digit/settings.tcl \
    etc/nviz2.2/scripts/wirecolorPopup.tcl \
    etc/nviz2.2/scripts/fileBrowser.tcl
do
    chmod -x $CURDIR/debian/tmp/usr/lib/grass/$x
done

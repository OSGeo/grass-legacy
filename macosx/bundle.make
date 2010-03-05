# extra binaries and data to bundle into the GRASS.app package
# 
# use ${INSTALL} to copy binaries.
# use ${INSTALL_DATA} for headers and other non-executables.
# use ${MAKE_DIR_CMD} to create a subfolder if needed.
# use ${LN} and ${LN_DIR} to symlink files and folders respectively.
# 
# Destinations include: bin and lib and should be prefixed by
# ${INST_DIR_TARGET}/
# Includes should not normally be needed.
# 
# For libraries, make sure to make symlinks if the symlink is the linked name.
# ie, libfoo.1.0.dylib is the library file, but libfoo.1.dylib is the link name:
#   ${LN} libfoo.1.0.dylib ${INST_DIR_TARGET}/lib/libfoo.1.dylib
# If you're not sure, make all symlinks for a library.
# 
# use one line per file after the "bundle-macosx:" line, indented with a tab.
# ie:
#	${INSTALL} /usr/local/bin/gpsbabel ${INST_DIR_TARGET}/bin
#	${INSTALL} /usr/local/pgsql/lib/libpq.5.1.dylib ${INST_DIR_TARGET}/lib
#	${LN} libpq.5.1.dylib ${INST_DIR_TARGET}/lib/libpq.5.dylib

WXPREFIX=/usr/local/lib/wxPython-unicode-2.8.10.1
bundle-macosx:
	${INSTALL} ${WXPREFIX}/lib/libwx_macud-2.8.0.dylib ${INST_DIR_TARGET}/lib
	${INSTALL} ${WXPREFIX}/lib/libwx_macud_gl-2.8.0.dylib ${INST_DIR_TARGET}/lib
	${INSTALL} ${WXPREFIX}/lib/libwx_macud_stc-2.8.0.dylib ${INST_DIR_TARGET}/lib
	${INSTALL} ${WXPREFIX}/lib/libwx_macud_gizmos-2.8.0.dylib ${INST_DIR_TARGET}/lib
	${INSTALL} ${WXPREFIX}/lib/libwx_macud_gizmos_xrc-2.8.0.dylib ${INST_DIR_TARGET}/lib
	cp -Rfp ${WXPREFIX}/lib/python2.5/site-packages/wx-2.8-mac-unicode/wx ${INST_DIR_TARGET}/etc/python
	cp -Rfp ${WXPREFIX}/lib/python2.5/site-packages/wx-2.8-mac-unicode/wxPython ${INST_DIR_TARGET}/etc/python
	cp -fp ${WXPREFIX}/lib/python2.5/site-packages/wxversion.py ${INST_DIR_TARGET}/etc/python
	sed -i '' -e 's/^GRASS_WXBUNDLED=.*/GRASS_WXBUNDLED=1/' ${INST_DIR_TARGET}/grass.sh

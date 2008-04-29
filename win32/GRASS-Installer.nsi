;----------------------------------------------------------------------------------------------------------------------------

;GRASS Installer for Windows
;Written by Marco Pasetti

;----------------------------------------------------------------------------------------------------------------------------

;Script and frequently changed version veriables

;Select if you are building a "SVN Trunk Version" or a "Release Tarball Version" of GRASS
;Change INSTALLER_TYPE variable to Trunk or Release

;Modify SVN_REVISION if needed

!define INSTALLER_TYPE "Release"
!define SVN_REVISION "31095"

;----------------------------------------------------------------------------------------------------------------------------

;Version variables that may need to be modified

!define VERSION_NUMBER "6.3.0"

;----------------------------------------------------------------------------------------------------------------------------

;Version variables
;Don't modify the following variables

!define NAME "GRASS"
!define COMPLETE_NAME "${NAME} ${VERSION_NUMBER}"
!define PUBLISHER "GRASS Development Team"
!define WEB_SITE "http://grass.osgeo.org/"
!define WIKI_PAGE "http://grass.osgeo.org/wiki/Main_Page"

;----------------------------------------------------------------------------------------------------------------------------

!if ${INSTALLER_TYPE} == "Trunk"
	!define INSTALLER_NAME "WinGRASS-Trunk-r${SVN_REVISION}-Setup.exe"
	!define DISPLAYED_NAME "${NAME} Trunk Revision ${SVN_REVISION}"
	!define PACKAGE_FOLDER ".\GRASS-Trunk-Package"
!else
	!define INSTALLER_NAME "Win${NAME}-${VERSION_NUMBER}-Setup.exe"
	!define DISPLAYED_NAME "${COMPLETE_NAME}"
	!define PACKAGE_FOLDER ".\GRASS-Release-Package"
!endif

;----------------------------------------------------------------------------------------------------------------------------

;General Definitions

;Name of the application shown during install
Name "${DISPLAYED_NAME}"

;Name of the output file (installer executable)
OutFile "${INSTALLER_NAME}"

;Default installation folder
InstallDir "C:\GRASS"

;Request application privileges for Windows Vista
RequestExecutionLevel user

;Tell the installer to show Install and Uninstall details as default
ShowInstDetails show
ShowUnInstDetails show

;----------------------------------------------------------------------------------------------------------------------------

;NSIS Includes

;Include Modern UI
!include "MUI2.nsh"

;Include Logic Library
!include 'LogicLib.nsh'

;----------------------------------------------------------------------------------------------------------------------------

;StrReplace
;Replaces all ocurrences of a given needle within a haystack with another string
;Written by dandaman32
 
Var STR_REPLACE_VAR_0
Var STR_REPLACE_VAR_1
Var STR_REPLACE_VAR_2
Var STR_REPLACE_VAR_3
Var STR_REPLACE_VAR_4
Var STR_REPLACE_VAR_5
Var STR_REPLACE_VAR_6
Var STR_REPLACE_VAR_7
Var STR_REPLACE_VAR_8
 
Function StrReplace
	Exch $STR_REPLACE_VAR_2
	Exch 1
	Exch $STR_REPLACE_VAR_1
	Exch 2
	Exch $STR_REPLACE_VAR_0
		StrCpy $STR_REPLACE_VAR_3 -1
		StrLen $STR_REPLACE_VAR_4 $STR_REPLACE_VAR_1
		StrLen $STR_REPLACE_VAR_6 $STR_REPLACE_VAR_0
		loop:
			IntOp $STR_REPLACE_VAR_3 $STR_REPLACE_VAR_3 + 1
			StrCpy $STR_REPLACE_VAR_5 $STR_REPLACE_VAR_0 $STR_REPLACE_VAR_4 $STR_REPLACE_VAR_3
			StrCmp $STR_REPLACE_VAR_5 $STR_REPLACE_VAR_1 found
			StrCmp $STR_REPLACE_VAR_3 $STR_REPLACE_VAR_6 done
			Goto loop
		found:
			StrCpy $STR_REPLACE_VAR_5 $STR_REPLACE_VAR_0 $STR_REPLACE_VAR_3
			IntOp $STR_REPLACE_VAR_8 $STR_REPLACE_VAR_3 + $STR_REPLACE_VAR_4
			StrCpy $STR_REPLACE_VAR_7 $STR_REPLACE_VAR_0 "" $STR_REPLACE_VAR_8
			StrCpy $STR_REPLACE_VAR_0 $STR_REPLACE_VAR_5$STR_REPLACE_VAR_2$STR_REPLACE_VAR_7
			StrLen $STR_REPLACE_VAR_6 $STR_REPLACE_VAR_0
			Goto loop
		done:
	Pop $STR_REPLACE_VAR_1 ; Prevent "invalid opcode" errors and keep the
	Pop $STR_REPLACE_VAR_1 ; stack as it was before the function was called
	Exch $STR_REPLACE_VAR_0
FunctionEnd
 
!macro _strReplaceConstructor OUT NEEDLE NEEDLE2 HAYSTACK
	Push "${HAYSTACK}"
	Push "${NEEDLE}"
	Push "${NEEDLE2}"
	Call StrReplace
	Pop "${OUT}"
!macroend
 
!define StrReplace '!insertmacro "_strReplaceConstructor"'

;----------------------------------------------------------------------------------------------------------------------------

Function CheckInstDir

	Var /GLOBAL INSTDIR_TEST
	Var /GLOBAL INSTDIR_LENGHT	
	Var /GLOBAL INSTDIR_TEST_LENGHT
	Var /GLOBAL WARNING_MESSAGE
	
	StrCpy $WARNING_MESSAGE "WARNING: you are about to install GRASS into a directory that has spaces$\r$\n"
	StrCpy $WARNING_MESSAGE "$WARNING_MESSAGEin either its name or the path of directories leading up to it.$\r$\n"
	StrCpy $WARNING_MESSAGE "$WARNING_MESSAGESome functionalities of GRASS might be hampered by this. We would highly$\r$\n"
	StrCpy $WARNING_MESSAGE "$WARNING_MESSAGEappreciate if you tried and reported any problems, so that we can fix them.$\r$\n"
	StrCpy $WARNING_MESSAGE "$WARNING_MESSAGEHowever, if you want to avoid any such issues, we recommend that you$\r$\n"
	StrCpy $WARNING_MESSAGE "$WARNING_MESSAGEchoose a simple installation path without spaces, such as: C:\GRASS.$\r$\n"
	
	${StrReplace} "$INSTDIR_TEST" " " "" "$INSTDIR"
	
	StrLen $INSTDIR_LENGHT "$INSTDIR"
	StrLen $INSTDIR_TEST_LENGHT "$INSTDIR_TEST"
	
	${If} $INSTDIR_TEST_LENGHT < $INSTDIR_LENGHT	
		MessageBox MB_OK|MB_ICONEXCLAMATION "$WARNING_MESSAGE"
	${EndIf}
	
FunctionEnd

;----------------------------------------------------------------------------------------------------------------------------

;Interface Settings

!define MUI_ABORTWARNING
!define MUI_ICON ".\Extras\install_grass.ico"
!define MUI_UNICON ".\Extras\uninstall_grass.ico"
!define MUI_HEADERIMAGE_BITMAP_NOSTETCH ".\Extras\InstallHeaderImage.bmp"
!define MUI_HEADERIMAGE_UNBITMAP_NOSTRETCH ".\Extras\UnInstallHeaderImage.bmp"
!define MUI_WELCOMEFINISHPAGE_BITMAP ".\Extras\WelcomeFinishPage.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP ".\Extras\UnWelcomeFinishPage.bmp"

;----------------------------------------------------------------------------------------------------------------------------

;Installer Pages

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE ".\Extras\GPL.TXT"
!insertmacro MUI_PAGE_DIRECTORY
Page custom CheckInstDir
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH

;----------------------------------------------------------------------------------------------------------------------------

;Language files

!insertmacro MUI_LANGUAGE "English"

;----------------------------------------------------------------------------------------------------------------------------
	
;Installer Sections

Section "GRASS" SecGRASS

	SectionIn RO
	
	;Set to try to overwrite existing files
	SetOverwrite try

	;Set the install path
	SetOutPath "$INSTDIR"
	
	;add GRASS files
	File /r ${PACKAGE_FOLDER}\*.*	
	File .\Extras\grass.ico
	File .\Extras\grass_web.ico
	File .\Extras\GRASS-WebSite.url
	File .\Extras\README.html	
	
	;Set GIS_DATABASE directory
	SetShellVarContext current
	Var /GLOBAL GIS_DATABASE	
	StrCpy $GIS_DATABASE "$DOCUMENTS\GIS DataBase"
	
	;Create GIS_DATABASE directory
	CreateDirectory "$GIS_DATABASE"
	
	;Install demolocation into GIS_DATABASE directory
	SetOutPath "$GIS_DATABASE\demolocation"
	File /r .\Extras\demolocation\*.*
	
	;Create uninstaller
	WriteUninstaller "$INSTDIR\Uninstall.exe"
	
	;Registry Key Entries
	
	;HKEY_LOCAL_MACHINE Install entries
	;Set the Name, Version and Revision of GRASS + PublisherInfo + InstallPath	
	WriteRegStr HKLM "Software\GRASS" "Name" "${NAME}"
	WriteRegStr HKLM "Software\GRASS" "VersionNumber" "${VERSION_NUMBER}"
	WriteRegStr HKLM "Software\GRASS" "Revision" "${SVN_REVISION}"
	WriteRegStr HKLM "Software\GRASS" "Publisher" "${PUBLISHER}"
	WriteRegStr HKLM "Software\GRASS" "WebSite" "${WEB_SITE}"
	WriteRegStr HKLM "Software\GRASS" "InstallPath" "$INSTDIR"
	
	;HKEY_LOCAL_MACHINE Uninstall entries
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS" "DisplayName" "${COMPLETE_NAME}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS" "UninstallString" "$INSTDIR\uninstall.exe"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS" "DisplayVersion" "${VERSION_NUMBER}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS" "DisplayIcon" "$INSTDIR\grass.ico"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS" "EstimatedSize" 1
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS" "HelpLink" "${WIKI_PAGE}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS" "URLInfoAbout" "${WEB_SITE}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS" "Publisher" "${PUBLISHER}"
  
	;create Desktop Shortcut
	SetShellVarContext all
	CreateShortCut "$DESKTOP\${COMPLETE_NAME}.lnk" "$INSTDIR\grass63.bat" "" "$INSTDIR\grass.ico" ""\
	SW_SHOWNORMAL "" "Launch ${COMPLETE_NAME}"
 
	;create Windows Start Menu Shortcuts
	SetShellVarContext all
	CreateDirectory "$SMPROGRAMS\GRASS"
	CreateShortCut "$SMPROGRAMS\GRASS\${COMPLETE_NAME}.lnk" "$INSTDIR\grass63.bat" "" "$INSTDIR\grass.ico" ""\
	SW_SHOWNORMAL "" "Launch ${COMPLETE_NAME}"
	CreateShortCut "$SMPROGRAMS\GRASS\MSYS.lnk" "$INSTDIR\msys\msys.bat" "" "$INSTDIR\msys\m.ico" ""\
	SW_SHOWNORMAL "" "Open MSYS Console"
	CreateShortCut "$SMPROGRAMS\GRASS\Web Site.lnk" "$INSTDIR\GRASS-WebSite.url" "" "$INSTDIR\grass_web.ico" ""\
	SW_SHOWNORMAL "" "Visit GRASS Web Site"
	CreateShortCut "$SMPROGRAMS\GRASS\ReadMe.lnk" "$INSTDIR\README.html" "" "" ""\
	SW_SHOWNORMAL "" "View ${COMPLETE_NAME} README File"
	CreateShortCut "$SMPROGRAMS\GRASS\Uninstall.lnk" "$INSTDIR\Uninstall.exe" "" "" ""\
	SW_SHOWNORMAL "" "Uninstall ${COMPLETE_NAME}"
	
	;create grass63.bat
	ClearErrors
	FileOpen $0 $INSTDIR\grass63.bat w
	IfErrors done_create_grass63.bat
	FileWrite $0 '@echo off$\r$\n'
	FileWrite $0 'rem #########################################################################$\r$\n'
	FileWrite $0 'rem #$\r$\n'
	FileWrite $0 'rem # File dynamically created by NSIS installer script;$\r$\n'
	FileWrite $0 'rem # Written by Marco Pasetti;$\r$\n'
	FileWrite $0 'rem #$\r$\n'
	FileWrite $0 'rem #########################################################################$\r$\n'
	FileWrite $0 'rem #$\r$\n'
	FileWrite $0 'rem # GRASS Initialization$\r$\n'
	FileWrite $0 'rem #$\r$\n'
	FileWrite $0 'rem #########################################################################$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem *******Environment variables***********$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Set GRASS Installation Directory Variable$\r$\n'
	FileWrite $0 'set GRASSDIR=$INSTDIR$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Directory where your .grassrc6 file will be stored$\r$\n'
	FileWrite $0 'set HOME=%USERPROFILE%$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Name of the wish (Tk) executable$\r$\n'	
	FileWrite $0 'set GRASS_WISH=wish.exe$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Path to the shell command$\r$\n'	 
	FileWrite $0 'set GRASS_SH=%GRASSDIR%\msys\bin\sh.exe$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Set Path to utilities (libraries and bynaries) used by GRASS$\r$\n'
	FileWrite $0 'set PATH=%GRASSDIR%\msys\bin;%GRASSDIR%\extrabin;%GRASSDIR%\extralib;%PATH%$\r$\n'
	FileWrite $0 'set PATH=%GRASSDIR%\tcl-tk\bin;%GRASSDIR%\sqlite\bin;%PATH%$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Set Path to MSIE web browser$\r$\n'	
	FileWrite $0 'set GRASS_HTML_BROWSER=%PROGRAMFILES%/Internet Explorer/iexplore.exe$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Path to the proj files (notably the epsg projection list)$\r$\n'	
	FileWrite $0 'set GRASS_PROJSHARE=%GRASSDIR%\proj$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'set WINGISBASE=%GRASSDIR%$\r$\n'
	FileWrite $0 '"%WINGISBASE%\etc\init.bat" %*'
	FileClose $0
	done_create_grass63.bat:
	
	;Set UNIX_LIKE GRASS Path
	Var /GLOBAL UNIX_LIKE_DRIVE
	Var /GLOBAL UNIX_LIKE_PATH
  
	StrCpy $UNIX_LIKE_DRIVE "$INSTDIR" 3
	StrCpy $UNIX_LIKE_PATH "$INSTDIR" "" 3
  
	;replace "\" with "/" in $UNIX_LIKE_DRIVE
	${StrReplace} "$UNIX_LIKE_DRIVE" "\" "/" "$UNIX_LIKE_DRIVE"
  
	;replace ":" with "" in $UNIX_LIKE_DRIVE
	${StrReplace} "$UNIX_LIKE_DRIVE" ":" "" "$UNIX_LIKE_DRIVE"
	
	;replace "\" with "/" in $UNIX_LIKE_PATH
	${StrReplace} "$UNIX_LIKE_PATH" "\" "/" "$UNIX_LIKE_PATH"

	;Set USERNAME
	Var /GLOBAL USERNAME
  
	SetShellVarContext current
	StrCpy $USERNAME "$PROFILE" "" 3
	${StrReplace} "$USERNAME" "Documents and Settings\" "" "$USERNAME"

	;Create $INSTDIR\msys\home and $INSTDIR\msys\home\$USERNAME directories
	CreateDirectory $INSTDIR\msys\home
	CreateDirectory $INSTDIR\msys\home\$USERNAME
  
	;create $INSTDIR\msys\home\$USERNAME\grass63
	ClearErrors
	FileOpen $0 $INSTDIR\msys\home\$USERNAME\grass63 w
	IfErrors done_create_grass63
	FileWrite $0 '#! /bin/sh$\r$\n'
	FileWrite $0 '#########################################################################$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '# File dynamically created by NSIS installer script;$\r$\n'
	FileWrite $0 '# Written by Marco Pasetti;$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '#########################################################################$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '# $$Id: grass.src 22360 2007-01-22 22:41:45Z markus $$$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '# MODULE:   	GRASS Initialization$\r$\n'
	FileWrite $0 '# AUTHOR(S):	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th$\r$\n'
	FileWrite $0 '# PURPOSE:  	The source file for this shell script is in$\r$\n'
	FileWrite $0 '#   	    	lib/init/grass.src and is the grass startup script. It$\r$\n'
	FileWrite $0 '#   	    	requires a source file because the definition of GISBASE$\r$\n'
	FileWrite $0 '#   	    	is not known until compile time and is substituted from the$\r$\n'
	FileWrite $0 '#   	    	Makefile. Any command line options are passed to Init.sh.$\r$\n'
	FileWrite $0 '# COPYRIGHT:    (C) 2000-2005 by the GRASS Development Team$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '#               This program is free software under the GNU General Public$\r$\n'
	FileWrite $0 '#   	    	License (>=v2). Read the file COPYING that comes with GRASS$\r$\n'
	FileWrite $0 '#   	    	for details.$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '#########################################################################$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '# Modified by Marco Pasetti$\r$\n'
	FileWrite $0 '# added export PATH instructions to let GRASS work from$\r$\n'
	FileWrite $0 '# MSYS environment in dynamic NSIS installation$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '#########################################################################$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'trap "echo '
	FileWrite $0 "'User break!' ; "
	FileWrite $0 'exit" 2 3 9 15$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 '# Set the GISBASE variable$\r$\n'
	FileWrite $0 'GISBASE=/$UNIX_LIKE_DRIVE$UNIX_LIKE_PATH$\r$\n'
	FileWrite $0 'export GISBASE$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 '# Set the PATH variable$\r$\n'
	FileWrite $0 'PATH="$$GISBASE/extrabin:$$GISBASE/extralib:$$PATH"$\r$\n'
	FileWrite $0 'PATH="$$GISBASE/sqlite/bin:$$GISBASE/tcl-tk/bin:$$PATH"$\r$\n'
	FileWrite $0 'export PATH$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'exec "$$GISBASE/etc/Init.sh" "$$@"'
	FileClose $0
	done_create_grass63:
	
	;Set GIS_DATABASE Path
	Var /GLOBAL GIS_DATABASE_PATH
  
	;replace \ with / in $GIS_DATABASE
	${StrReplace} "$GIS_DATABASE_PATH" "\" "/" "$GIS_DATABASE"
  
	;create $PROFILE\.grassrc6
	SetShellVarContext current
	ClearErrors
	FileOpen $0 $PROFILE\.grassrc6 w
	IfErrors done_create_.grassrc6
	FileWrite $0 'GISDBASE: $GIS_DATABASE_PATH$\r$\n'
	FileWrite $0 'LOCATION_NAME: demolocation$\r$\n'
	FileWrite $0 'MAPSET: PERMANENT$\r$\n'
	FileClose $0	
	done_create_.grassrc6:
	
	CopyFiles $PROFILE\.grassrc6 $INSTDIR\msys\home\$USERNAME
                 
SectionEnd

Section /O "North Carolina Data Set" SecNorthCarolina 
  
  	SetOutPath "$GIS_DATABASE"
	AddSize 293314
	
	Var /GLOBAL NORTH_CAROLINA_DOWNLOAD_PATH
	
	StrCpy $NORTH_CAROLINA_DOWNLOAD_PATH "http://grass.osgeo.org/sampledata/nc_spm_07_2007_dec20.tar.gz"

	InitPluginsDir
	NSISdl::download $NORTH_CAROLINA_DOWNLOAD_PATH "$GIS_DATABASE\nc_spm_07_2007_dec20.tar.gz"
	Pop $R0 ;Get the return value
	StrCmp $R0 "success" +3
		MessageBox MB_OK "Download $R0. GRASS will be installed without North Carolina GRASS Sample Database"

	InitPluginsDir
	untgz::extract "$GIS_DATABASE\nc_spm_07_2007_dec20.tar.gz" "$GIS_DATABASE"
	Pop $0
	StrCmp $0 "success" ok
  		DetailPrint "$0" ;print error message to log
	ok:
	
	Rename "$GIS_DATABASE\nc_spm_07" "$GIS_DATABASE\North-Carolina"
	Delete "$GIS_DATABASE\nc_spm_07_2007_dec20.tar.gz"

SectionEnd

Section /O "South Dakota Data Set" SecSouthDakota  
  
  	SetOutPath "$GIS_DATABASE"
	AddSize 42171
	
	Var /GLOBAL SOUTH_DAKOTA_DOWNLOAD_PATH
	
	StrCpy $SOUTH_DAKOTA_DOWNLOAD_PATH "http://grass.osgeo.org/sampledata/spearfish_grass60data-0.3.tar.gz"

	InitPluginsDir
	NSISdl::download $SOUTH_DAKOTA_DOWNLOAD_PATH "$GIS_DATABASE\spearfish_grass60data-0.3.tar.gz"
	Pop $R0 ;Get the return value
	StrCmp $R0 "success" +3
		MessageBox MB_OK "Download $R0. GRASS will be installed without South Dakota GRASS Sample Database"

	InitPluginsDir
	untgz::extract "$GIS_DATABASE\spearfish_grass60data-0.3.tar.gz" "$GIS_DATABASE"
	Pop $0
	StrCmp $0 "success" ok
  		DetailPrint "$0" ;print error message to log
	ok:
	
	Rename "$GIS_DATABASE\spearfish60" "$GIS_DATABASE\South-Dakota"
	Delete "$GIS_DATABASE\spearfish_grass60data-0.3.tar.gz"

SectionEnd

;----------------------------------------------------------------------------------------------------------------------------

;Descriptions
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
	!insertmacro MUI_DESCRIPTION_TEXT ${SecGRASS} "Install ${DISPLAYED_NAME}"
	!insertmacro MUI_DESCRIPTION_TEXT ${SecNorthCarolina} "Download and install North Carolina GRASS sample database"
	!insertmacro MUI_DESCRIPTION_TEXT ${SecSouthDakota} "Download and install South Dakota GRASS sample database"
!insertmacro MUI_FUNCTION_DESCRIPTION_END

;----------------------------------------------------------------------------------------------------------------------------

;Uninstaller Section

Section "Uninstall"

	;remove files
	Delete "$INSTDIR\Uninstall.exe"
	Delete "$INSTDIR\grass.ico"
	Delete "$INSTDIR\grass_web.ico"
	Delete "$INSTDIR\GPL.TXT"
	Delete "$INSTDIR\AUTHORS"
	Delete "$INSTDIR\CHANGES"
	Delete "$INSTDIR\COPYING"	
	Delete "$INSTDIR\grass63.bat"	
	Delete "$INSTDIR\GRASS-WebSite.url"	
	Delete "$INSTDIR\README.html"
	Delete "$INSTDIR\REQUIREMENTS.html"
	
	;remove folders
	RMDir /r "$INSTDIR\bin"
	RMDir /r "$INSTDIR\bwidget"
	RMDir /r "$INSTDIR\docs"
	RMDir /r "$INSTDIR\driver"
	RMDir /r "$INSTDIR\etc"
	RMDir /r "$INSTDIR\extrabin"
	RMDir /r "$INSTDIR\extralib"
	RMDir /r "$INSTDIR\fonts"
	RMDir /r "$INSTDIR\include"
	RMDir /r "$INSTDIR\lib"
	RMDir /r "$INSTDIR\msys"
	RMDir /r "$INSTDIR\proj"
	RMDir /r "$INSTDIR\scripts"
	RMDir /r "$INSTDIR\sqlite"
	RMDir /r "$INSTDIR\tcl-tk"
	
	;if empty, remove install folder
	RMDir "$INSTDIR"
	
	;remove Desktop ShortCut
	SetShellVarContext all
	Delete "$DESKTOP\${COMPLETE_NAME}.lnk"
	
	;remove Programs Start ShortCut
	SetShellVarContext all
	RMDir /r "$SMPROGRAMS\GRASS"
	
	;remove .grassrc6 file
	SetShellVarContext current
	Delete "$PROFILE\.grassrc6"	

	;remove Registry Entries
	DeleteRegKey HKLM "Software\GRASS"
	DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GRASS"

SectionEnd
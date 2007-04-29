import wx
import os,sys
try:
   from subprocess import *
except:
   from compat import subprocess
   from compat.subprocess import *

try:
   import subprocess
except:
   CompatPath = os.getenv("GISBASE") + "/etc/wx"
   sys.path.append(CompatPath)
   from compat import subprocess


class SetDefaultFont:
        """
        Opens a file selection dialog to select default font
        to use in all GRASS displays
        """

        def __init__(self, parent):
                #get system type
                cmd = "uname -s"
                try:
                        p = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
                        system = p.stdout.read()
                        if p.stdout < 0:
                                print >> sys.stderr, "Child was terminated by signal", p.stdout
                        elif p.stdout > 0:
                                #print >> sys.stderr, p.stdout
                                pass
                except OSError, e:
                        print >> sys.stderr, "Execution failed:", e

                system = system.strip('\n')
                system = system.strip()

                if system == "Darwin":
                        fontpath = os.path.join("Library","Fonts")
                else:
                        fontpath = ''

                dlg = wx.FileDialog(
                    parent, message="Choose font file",
                    defaultDir=fontpath,
                    style=wx.OPEN | wx.CHANGE_DIR
                    )

                if dlg.ShowModal() == wx.ID_OK:
                    # get the font.
                    font = dlg.GetPath()
                    # set environmental font variable
                    os.environ["GRASS_FONT"] = font

                dlg.Destroy()




import  Gism
import wx

import os,sys


class GRASSStart(wx.Frame):
    def __init__(self, parent, id, title):
        wx.Frame.__init__(self, parent, id, title, wx.DefaultPosition,
                wx.Size(400, 400))

        # select grass database
        self.gisbase = sys.argv[1]
        
        # icon
        self.SetIcon(wx.Icon(os.path.join(self.gisbase,"etc","dm","grass.gif"),
            wx.BITMAP_TYPE_GIF))

        # boxes
        boxV = wx.BoxSizer(wx.VERTICAL)
        boxH = wx.BoxSizer(wx.HORIZONTAL)

        # heading
                        #parent, id, pos, size, style, name
        ipanel = wx.Panel(self, -1, (0,0), (100, 127),  style=wx.NO_BORDER)
        self.picture = wx.StaticBitmap(ipanel)
        self.picture.SetBitmap(wx.Bitmap(os.path.join(sys.argv[1],"etc","gintro.gif")))
        message = "Welcome to GRASS GIS Version 6.3.cvs\n"+\
                  "The world's leading open source GIS\n\n"+\
                  "Select existing project location and mapset"+\
                  "or define a new location"
        welcome = wx.StaticText(ipanel, -1, message, (0,150), style=wx.ALIGN_CENTRE)
        welcome.SetFont(wx.Font(10, wx.NORMAL, wx.NORMAL, wx.NORMAL))

        # grassdbase
        gpanel = wx.Panel(self, -1, (0,0), (0, 0),  style=wx.NO_BORDER)

        # add stuff to vertical box
        boxV.Add(boxH,1)
        # add stuff to horizontal box
        boxH.Add(ipanel,1)
        # add stuff to main frame
        self.SetSizer(boxV)

        self.Center()

    def OnQuit(self, event):
        self.Close()


class MyApp(wx.App):
    def OnInit(self):
        frame = GRASSStart(None, -1, 'Welcomme to GRASS GIS!')
        frame.Show(True)
        return True


if __name__ == "__main__":

    try: 
        if os.path.isdir(sys.argv[1]):
            os.environ['GISBASE'] = sys.argv[1]
        else:
            raise OSError, "No GRASS directory"
    except StandardError, e:
        print "GISBASE has to be set"
        print "Usage:"
        print " %s path/to/gisbase" % (sys.argv[0])

    app = MyApp()
    app.MainLoop()

/*
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
    USA
*/
/*
 * Copyright (C) 1998 The Information-technology Promotion Agency, Japan(IPA), LGPL
 * $Id$
 */
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import GRASS.*;

/**
 * Class for testing if library works 
 *
 * @author  $Author$
 * @version $Revision$
 */
public class check2 extends Frame {

  public static void main(String args[]) {

    if (args.length == 4) {
      new check2( args );
    } else {
      System.err.println("Usage:\n\tjava check2"+
			 " GISBASE GISDBASE LOCATION_NAME MAPSET");
      System.exit(-1);
    }
    
  }

  protected libgis gl = null;
  protected libfun lf = null;
  protected MapCanvas mapCanvas;

  check2(String args[]) {

    try {
    
      gl = new libgis(
        this.getClass().getName(),
        args[0],  // GISBASE
        args[1],  // GISDBASE
        args[2],  // LOCATION_NAME
        args[3]   // MAPSET
      );

      lf = new libfun( gl );

      // bring a frame on screen
      MenuBar mb = new MenuBar();
      Menu m = new Menu("File");

      MenuItem openMenuItem = new MenuItem("Open...");
      openMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
	  LayerDialog ld = new LayerDialog( check2.this, "Layers" );
	  ld.setVisible( true );
	}
      });
      m.add(openMenuItem);

      m.add(new MenuItem("-"));

      MenuItem exitMenuItem = new MenuItem("Exit");
      exitMenuItem.addActionListener( new ActionListener() {
        public void actionPerformed(ActionEvent e) {
	  System.exit(0);
	}
      });
      m.add(exitMenuItem);
      
      mb.add(m);
      setMenuBar(mb);

      mapCanvas = new MapCanvas();
      add(mapCanvas);

      setSize(800, 600);
      setVisible(true);
      addWindowListener(new WindowAdapter() {
	public void windowClosing(WindowEvent we) {
	  System.exit(0);
	}
      });
      
    } catch (Exception e) {
      System.err.println("Exception caught: "+e);
      System.exit(-1);
    }

  }

  class MapCanvas extends Canvas {

    protected String layer  = null;
    protected String mapset = null;
    protected FD fd = null;
    protected Colors cl = null;
    protected Cellhead ch = null;

    protected Dimension img_d = null;
    protected Image image = null;

    public void update(Graphics g) {
    
      Dimension d = getSize();

      if ( (image == null) 
	   || (d.width != img_d.width)
	   || (d.height != img_d.height) ) {

	img_d = d;
	startDisplay( layer );
      }

      if (image != null) g.drawImage(image, 0, 0, this);
    }

    public void paint(Graphics g) {
      update(g);
    }

    public void startDisplay(String layer) {

      try {
	if (layer == null) return;

	if (ch == null) { // first time image
	  ch = gl.G_get_set_window();
	}

	if (this.layer != layer) { // reading of a new layer
	  mapset = gl.G_find_cell( layer, "" );
	  cl = new Colors( layer, mapset );
	  if (fd != null) fd.G_close_cell();
	  fd = gl.G_open_cell_old( layer, mapset );
	}

	int w = img_d.width;
	int h = img_d.height;
	ch.setRows( h ); 
	ch.setCols( w );
	ch.G_adjust_Cell_head(true, true);
	gl.G_set_window(ch); 
	
	System.out.println("Displaying "+layer);
	setTitle( layer + "@" + mapset );
	
	CELL buf = new CELL();
	int[] pix = new int[w * h];
	int[] colrs = buf.toColorArray( cl );
	
	for (int r = 0; r < h; r++) {
	  fd.G_get_map_row( buf, r );
	  buf.toColorArray( colrs, cl );
	  System.arraycopy( colrs, 0, pix, r * w, w );
	}
	
	image = createImage(new MemoryImageSource(w, h, pix, 0, w));
	repaint();

	this.layer = layer;

      } catch (GRASSException e) {
	System.err.println("Reading grass layer "+layer+" failed due to:"+e);
	System.exit(1);
      }

    }

  }

  class LayerDialog extends Dialog implements ActionListener {

    Button show, cancel;
    List list;

    LayerDialog(Frame f, String title) {

      super(f, title, true);

      list = new List(10, false);

      try {

	for (Enumeration e=lf.g_list(lf.TYPE_RAST); e.hasMoreElements(); ) 
	  list.add( (String)e.nextElement() );
	add("Center", list);

      } catch (GRASSException e) {
	System.err.println("Failed to build a list of "+
			   "available Layers because:\n"+e);
	System.exit(1);
      }

      show   = new Button("Show");
      show.addActionListener(this);
      cancel = new Button("Cancel");
      cancel.addActionListener(this);

      Panel buttons = new Panel();
      buttons.add(show);
      buttons.add(cancel);
      add("South", buttons);

      pack();
    }

    public void actionPerformed(ActionEvent event) {

      Object source = event.getSource();
      if (source == show) 
	mapCanvas.startDisplay( list.getSelectedItem() );

      setVisible(false);

    }

  }

}


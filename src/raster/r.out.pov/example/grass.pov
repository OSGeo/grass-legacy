// Persistence of Vision Raytracer
// r.out.pov test
// Author: Klaus Meyer, GEUM.tec GbR
// 
// This pov-file is a test for height-fields
// Height-field (*.tga) can be generated with
// GRASS r.out.pov.
// The options within files should work when
// r.out.pov is used with the option hftype=1
// htype=1 means that heights are normalized
//
// please mailto GEUM.tec@geum.de if you have
// any interesting ideas how to use POV in
// conjunction with a GIS (Geographic Information
// system).

#include "shapes.inc"
#include "colors.inc"
#include "textures.inc"

background { colour White }

camera {  // look straight to north
   location <0.0, 1.0, -0.6>
   direction <0.0, 0.0, 1.0>
   up <0.0, 1.0, 0.0>
   look_at <0.0, 0.0, 0.0>
//   fisheye
//   perspective
}


// Define a couple of colors for the light sources.
#declare MainLight = color red 0.8 green 0.8 blue 0.8
#declare FillLight = color red 0.23 green 0.23 blue 0.25
// Light source (main)
light_source { <0.0, 1000.0, -100.0> color MainLight }
// Light source ( shadow filler )
light_source { <500.0, 300.0, 600.0> color FillLight }

height_field  {

   tga "elev4.tga"    // x 0-1.0 y 0-1.0 z 0-1.0
   smooth

/* with the following you can drape an image...
    texture
     {
       pigment
         {
           image_map { tga "testps.tga" }
           rotate <90, 0, 0>
         }
     }
*/
//   water_level 0.375
//     pigment { White }
pigment {
        // this time we don't want to drape an image
        // we want to use a gradient color
        gradient y
		color_map {
			[0.0 color Blue] // red 1.0 green 0.5 blue 0.0]
			[0.25 color Blue] //red 0.5 green 0.5 blue 0.0]
			[0.26 color Blue]//red 0.5 green 0.75 blue 0.25]
			[0.500 color Green]//red 0.25 green 0.25 blue 0.25]
			[0.9 color Red]//red 0.75 green 0.75 blue 0.75]
			[1.0 color Red]//red 1.0 green 1.0 blue 1.0]
		}
	}

   finish {
	  crand 0.05         // dither  - not used often, but this image needs it.
	  ambient 0.2        // Very dark shadows
	  diffuse 0.8        // Whiten the whites
	  phong 0.3          // shiny
	  phong_size 90.0    // with tight highlights
	  specular 0.8
	  roughness 0.05
   }
   translate <-0.5, 0.0, -0.5>  // Center the image by half
   scale < 1, 0.1, 1 >  // flatten the heights

}
// end of file

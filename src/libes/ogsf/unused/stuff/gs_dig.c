/*
Given a mouse click on the screen and a defined size, in cells,
use the los intersection with first surface to define the center
of a [square, round] hole which extends to the next surface, if there is 
one, otherwise remains open.  A click on an area of surface that has
already been exposed should use the same boundaries to dig to the next
surface.

Each surface should have associated with it two types of hole polygons,

1) dig boundary - area where surface is not to be drawn
2) exposed boundary - area below (above?) surface with identical dig boundary

Think about: how to handle surfaces protruding through other surfaces
(how click should be interpreted as dig instruction), how walls should be
drawn (user options?)
*/

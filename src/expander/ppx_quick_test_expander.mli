open Ppxlib

val expand
  :  loc:location
  -> pattern:pattern
  -> expression:expression
  -> attributes:attributes
  -> structure_item

val enclose_impl : location -> structure * structure

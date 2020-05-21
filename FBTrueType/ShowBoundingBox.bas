#include once "FBTrueType.bi"
' low level
' get properties of a glyph and show the bounding box
' NOTE: for some font styles it's OK if boundig boxes overlaps

dim as FontProps  fprops
dim as GlyphProps gprops

screenres 640,480

'const fontfile = "GeosansLight.ttf"
const fontFile = "DejaVuSans-Oblique.ttf"
chdir exepath()

' load the font
var font = FontLoad(fontfile)
if font<0 then
  print "error: loading " & fontfile & " " & ErrorText(font)
  beep : sleep : end 1
end if


dim as long cx=1,cy,yLine=1

' get (scaled) properties for a font height of 50 pixels
FontPorperties(font, 50, fProps)

' I use ASCII code here but you can use unicode also (if the font has the glyphs)
for char as long= 33 to 127
  ' get glyph index from char (the glyph index inside the font)
  dim as long index1 = GlyphIndex(font, char)
  ' not all chars have a renderable glyph
  if index1<>GLYPH_NOT_FOUND then
    ' get glyph index from neighbor char (if any)
    dim as long index2 = GlyphIndex(font,char+1)
    if index2=GLYPH_NOT_FOUND then index2=0
    ' get (scaled) properties of the glyph
    if GlyphProperties(font, fProps, gProps, index1,index2) = 0 then
      ' if not fits on screen move the y cursor to the next font line and reset cursor x position
      if cx+gProps.w>639 then cy += fProps.advanceHeight : cx=1
      ' draw only the bounding box of the glyph
      line (cx,cy + gProps.y)-step(gProps.w, gProps.h),,B
      ' if you comment out the next line you can imagine what will be drawn in the bounding box 
      draw string (cx + 2,cy + 2 + gprops.y),chr(char)
      ' move to next x position + kerning advanced (if the glyph has a neighbor) 
      cx += gProps.advanceWidth + iif(index2<>0,gProps.kernAdvance,0)
      'cx += gProps.w
    end if
  end if
next
sleep


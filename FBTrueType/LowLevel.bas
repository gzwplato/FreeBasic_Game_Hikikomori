#include once "FBTrueType.bi"

' (low level)
' Internal I create an 8-bit FBGFX image and render the glyph antialias.
' The 8-bit image represent an alpha channel and can be combined with a 32-bit images

dim as FontProps  fProps
dim as GlyphProps gProps
dim as integer maxw=800,maxh=600
screenres maxw,maxh,32
'const fontfile = "pocketcalcuatlor.ttf"
'const fontfile = "Circus.ttf"
const fontfile = "DejaVuSans-Oblique.ttf"
'const fontfile = "GeosansLight.ttf"
chdir exepath()

' load the font
var font = FontLoad(fontfile)
if font<0 then
  print "error: loading: " & fontfile & " " & ErrorText(font)
  beep : sleep : end 1
end if

dim as long cx=1,cy
windowtitle  fontfile

' get (scaled) properties for a font height defined in pixels
FontPorperties(font, 26, fProps)
' use unicode
for char as long = 32 to 2550
  ' get glyph index from char (the glyph index inside the font)
  dim as long index1 = GlyphIndex(font, char)
  ' not all chars have a renderable glyph
  if index1<>GLYPH_NOT_FOUND then
    ' get glyph index from neighbor char (if any)
    dim as long index2 = GlyphIndex(font,char+1)
    if index2 = GLYPH_NOT_FOUND then index2=0
    ' get (scaled) properties of the glyph
    if GlyphProperties(font, fProps, gProps, index1,index2) = 0 then
      ' if not fits on screen move the y cursor to the next font line and reset cursor x position
      if cx+gProps.w>=maxw then cy += fProps.advanceHeight : cx=1
      ' render the glyph in a 8-bit image (alphachannel)
      var AlphaChannel = GlyphImageCreate(font, fProps, gProps,index1)
      if AlphaChannel then
        var glyph = ImageCreate(gProps.w,gProps.h,RGB(255*rnd,255*rnd,255*rnd))
        ' replace the alpha channel from the 32-bit image with the rendered glyph
        put glyph,(0,0),AlphaChannel,ALPHA
        ' put the result on screen
        put (cx,cy + gProps.y),glyph,ALPHA
        ImageDestroy glyph
        ' the temporary AlphaChannel is a normal FBGFX Image and must be deallocated as well
        ImageDestroy AlphaChannel
      end if
      ' move to next x position + kerning advance (if the glyph has a neighbor) 
      cx += gProps.advanceWidth ' + gProps.kernAdvance
    end if
  end if
next
sleep


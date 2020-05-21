#include once "FBTrueType.bi"

' test of:
' load a true type font
' get font properties based on height in pixel
' get glyph index from ASCI char
' free the loaded font

const fontfile = "GeosansLight.ttf"
chdir exepath()

var font = FontLoad(fontfile)
if font<0 then
  print "error: loading " & fontfile & " " & ErrorText(font)
  beep : sleep : end 1
end if

dim as FontProps fProps
' get properties for this font in 32 pixel height
FontPorperties(font,32,fProps)
with fProps
  print "scale: " & .scale & " ascent: " & .ascent & " descent: " & .descent & " line gap: " & .linegap
end with
print
dim as long index
for char as long=asc("a") to asc("c")
  index = GlyphIndex(font, char)
  if index<>GLYPH_NOT_FOUND then
    print chr(char) & " asci = " & char & " = " & index & " glyph index"
  end if
next
print 
for char as long=asc("0") to asc("9")
  index = GlyphIndex(font, char)
  if index<>GLYPH_NOT_FOUND then
    print chr(char) & " asci = " & char & " = " & index & " glyph index"
  end if
next

FontFree(font)
sleep


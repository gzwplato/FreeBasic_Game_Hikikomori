#include once "FBTrueType.bi"

' test the ceated draw string bitmap

' !!! set the font bitamp you created before !!!
const FontFile = "DejaVuSans-Oblique.bmp"

' optinal set the path to your bitmap font(s)
chdir exepath()

screenres 640,480,32

var font = LoadFontBitmap(FontFile)
if font = 0 then
  print "error: can't load " & FontFile & "' !"
  beep : sleep : end 1
end if  

dim as string text = "Draw String compatible font bitmap."

' test it
draw string (10,10),text,,font,alpha
draw string (10,30),"getTextWidth(font, text): " & getTextWidth(font, text),,font,alpha

sleep


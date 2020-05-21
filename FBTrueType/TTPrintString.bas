#include once "FBTrueType.bi"

' test of TTPrint with string

dim as integer maxw=640,maxh=480
screenres maxw,maxh,32
'const fontfile = "pocketcalcuatlor.ttf"
'const fontfile = "Circus.ttf"
'const fontfile = "GeosansLight.ttf"
const fontfile = "DejaVuSans-Oblique.ttf"
chdir exepath()

' load the font
var font = FontLoad(fontfile)
if font<0 then
  print "error: loading: " & fontfile & " " & ErrorText(font)
  beep : sleep : end 1
end if

dim as integer y = 10
for i as integer = 0 to 11
  TTPrint font,32,y, "This is a simple test !",rgb(i*16,128,255-i*16),16+i*4
  y+=16+i*4
next
sleep


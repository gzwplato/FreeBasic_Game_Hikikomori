#include once "FBTrueType.bi"

' create a ASCII code font bitmap for the draw string command
' Note: FBTrueType can draw unicode fonts but draw string not.
' The draw string image header limits the char number to a ubyte
' so 255 are the highest possible char number !
dim shared as FontProps fProps
function getFontBitmapProperties(byval font        as long, _
                                 byval fontHeight  as integer, _
                                 byref firstChar   as integer, _
                                 byref lastChar    as integer, _
                                 byref imageWidth  as integer, _
                                 byref imageHeight as integer) as boolean
  dim as GlyphProps gProps
  firstChar   = -1
  lastChar    = 0
  imageWidth  = 0
  imageHeight = 0
  if FontPorperties(font, fontHeight, fProps) then return false
  for char as long = 0 to 255
    dim as long index1 = GlyphIndex(font, char)
    if (index1<>GLYPH_NOT_FOUND) andalso (GlyphProperties(font, fProps, gProps, index1,0)=0) then
      if firstChar=-1 then firstChar=char
      lastChar+=1 ' count the char
      if gProps.h+gProps.y > imageHeight then imageHeight=gProps.h+gProps.y
       'imageWidth += gProps.advanceWidth + 1
       imageWidth += iif(gProps.w>0, gProps.w, gProps.advanceWidth)
    elseif firstChar=-1 then
      ' we can ignore the missing char
    else
      ' if a char in ASCII range are missed add a 0 pixel width dummy char
      lastChar   += 1 ' add the dummy char
    end if
  next
  ' no Glyph found !
  if lastChar = 0 then return false
  ' make the char counter as the last char number
  lastChar += firstChar-1
  ' add one font header line
  imageHeight += 1
  return true
end function

'
' main
'
dim as GlyphProps gProps

' !!! set any true type font file !!!
const fontFile = "DejaVuSans-Oblique.ttf"
chdir exepath()

' load the font
var font = FontLoad(fontfile)
if font<0 then
  print "error: loading: " & fontfile & " " & ErrorText(font)
  beep : sleep : end 1
end if

' !!! set any font size !!!
dim as integer fontHeight = 24

dim as integer firstChar,lastChar,imageWidth,imageHeight,headerPos
' get draw string image properties from ttf font and pixel height
if getFontBitmapProperties(font,fontHeight,firstChar,lastChar,imageWidth,imageHeight)=false then
  print "error: can't get ASCII properties from: " & fontfile & " !"
  beep : sleep : end 1
end if  

dim as integer w
screeninfo w
screenres w,imageHeight*3,32
windowtitle "create font bitmap " & fontFile & " " & ImageWidth & " x " & imageHeight & " first: " & firstChar & " last: " & lastChar
' !!! set the font color here !!!
dim as ulong fontColor = RGBA(255,255,255,0)
var fontImage = ImageCreate(ImageWidth,ImageHeight,fontColor)

dim as ubyte ptr ImageHeader = fontImage

' write the draw string bitmap header
ImageHeader+=32
ImageHeader[headerPos] = 0         : headerPos+=1
ImageHeader[headerPos] = firstChar : headerPos+=1
ImageHeader[headerPos] = lastChar  : headerPos+=1

dim as long cx,cy=1 ' row 0 is the font header

for char as long = firstChar to lastChar
  dim as long index1 = GlyphIndex(font, char)
  if (index1<>GLYPH_NOT_FOUND) andalso (GlyphProperties(font, fProps, gProps, index1,0) = 0) then
    var AlphaChannel = GlyphImageCreate(font, fProps, gProps,index1)
    if AlphaChannel then
      put fontImage,(cx, cy + gProps.y),AlphaChannel,ALPHA
      ImageDestroy AlphaChannel
    end if
    'ImageHeader[headerPos]=gProps.advanceWidth + 1 
    ImageHeader[headerPos]=iif(gProps.w>0,gProps.w,gProps.advanceWidth)
    cx += ImageHeader[headerPos] 
  else
    ' we add a 0 pixel width dummy char
    ImageHeader[headerPos]=0
  end if
  headerPos += 1
next

' save the ttf file as font bitmap
dim as string bmpFile = left(fontFile,len(fontFile)-4) & ".bmp"
bsave bmpFile,fontImage
' plot it on screen
put (0,0),fontImage,ALPHA
' and test it
draw string (10,imageHeight*1),"Draw String compatible TrueType font.",,fontImage,alpha
draw string (10,imageHeight*2),"Font Bitmap saved as: " & chr(34) & bmpFile & chr(34),,fontImage,ALPHA

sleep


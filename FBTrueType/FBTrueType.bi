#ifndef __FBTrueType_bi__
#define __FBTrueType_bi__

#include once "crt.bi"

#ifdef __FB_WIN32__
 #ifndef __FB_64BIT__
  #libpath "lib/win32"
 #else
  #libpath "lib/win64"
 #endif   
#else
 #libpath "lib/lin"
#endif

#ifndef __FB_64BIT__
# inclib "FBTrueType-32-static"
#else
# inclib "FBTrueType-64-static"
#endif

' error codes (<0) returned by FontLoad()
#define FONT_NOT_LOADED    (-1)
#define FONT_NOT_SUPPORTED (-2)
' maximal 64 font ids can be used at once (should be enough)
#define NO_FREE_FONTID     (-3)
' error code if a font id are not legal (not in range of 0-63 or font was not loaded before)
#define WRONG_FONTID       (-4)
' error code if a value are out of range (e.g. pixelheight <=0 ...)
#define WRONG_VALUE        (-5)
' error code if a unicode char was not found in font
#define GLYPH_NOT_FOUND    (-6)

function ErrorText(byval errcode as long) as string
  select case errcode
  case FONT_NOT_LOADED    : return "font not loaded"
  case FONT_NOT_SUPPORTED : return "font not supported" 
  case NO_FREE_FONTID     : return "no free font id (64 fonts in use)"
  case WRONG_FONTID       : return "font id is illegal"
  case WRONG_VALUE        : return "wrong value (parameter out of range)"
  case GLYPH_NOT_FOUND    : return "no glyph for unicode in font"
  case else               : return "no error"
  end select
end function


' font properties
type FontProps
  as single scale
  as long   advanceHeight ' space to the next cursor y position (ascent - descent + linegap)
  as long   ascent        ' the space between base line to top of the font
  as long   descent       ' space from bottom of the font to base line (negativ)
  as long   linegap       ' space from bottom of the font to next font top line
end type

' glyph properties
type GlyphProps
  as long advanceWidth    ' the space to the next cursor x position
  as long kernAdvance     ' the kerning can be added to advanceWidth if the glyph has a neighbor glyph
  as long leftSideBearing ' the space from current x position to the most left of the glyph
  as long y               ' the absolute y position of the glyph (from font ascent)
  as long w,h             ' width and height of the visibe part of the glyph
end type


extern "C"

' init the library and prepare 64 font id's (it's not strictly required)
declare sub      FontInit()
' free all loaded fonts at once (not strictly required)
declare sub      FontDestroy()

' ########
' # Font #
' ########
' load a *.ttf file from memory (returns a new font id or error code)
declare function FontCopy(byval filedata as any ptr) as long
' load a *.ttf file from filesystem (returns a new font id or error code)
declare function FontLoad(byval filename as const zstring ptr) as long
' free a loaded font (mark the font ID as illegal WRONG_FONTID)
declare sub      FontFree(byref font as long)

' get the bounding box of all possible characters (return 0 on success or error code)
declare function FontBoundingBox(byval font as long, _
                                 byref x0 as long, byref y0 as long, _
                                 byref x1 as long, byref y1 as long) as long

' get font properties based on height in pixels (return 0 on success or error code)
declare function FontPorperties(byval font as long, byval HeightInPixel as long, byref props as FontProps) as long

' #########
' # Glyph #
' #########
' get glyph index return the glyph position inside the font 
declare function GlyphIndex(byval font as long, byval char as long) as long
' returns true if the glyph has a renderable shape
declare function GlyphHasShape(byval font as long, byval gindex as long) as boolean
' get the bounding box of the visible shape of a glyph (return 0 on success or error code)
declare function GlyphBoundingBox(byval font as long, byval glyphIndex as long, _
                                  byref x0 as long, byref y0 as long, _
                                  byref x1 as long, byref y1 as long) as long

' get glyph properties based on glyph index 1 and optional the neighbor glyph index 2
' fill the glypprops members (return 0 on success or error code)
declare function GlyphProperties(byval font as long, _
                                 byref fProps as const FontProps, byref gProps as GlyphProps, _
                                 byval glyphIndex1 as long, byval glyphIndex2 as long=0) as long
' render the glyph antialias return a 8-bit FBGFX image or NULL on error
declare function GlyphImageCreate(byval font as long, _
                                  byref fProps as const FontProps, _
                                  byref gProps as const GlyphProps, _
                                  byval glyphindex as long) as any ptr
' you can use ImageDestroy also
declare sub      GlyphImageDestroy(byref img as any ptr)

end extern

' print a string with truetype font
sub TTPrint overload(byval font as long, _
                     byval x as integer,byval y as integer, _
                     byref txt as string, _
                     byval col as ulong=rgb(255,255,255), _
                     byval size as integer=24)
  dim as FontProps  fProps
  dim as GlyphProps gProps
  dim as integer maxw,maxh,bytes,cx=x,cy=y
  if screenptr()=0 then return
  screeninfo maxw,maxh,,bytes
  if bytes<>4 then return
  if size<4 then return
  var nChars=len(txt) : if nChars<1 then return
  if FontPorperties(font, size, fprops) then return
  nChars-=1
  for i as integer = 0 to nChars
    var char = txt[i]
    if char<33 then
      if char=32 then cx+=size\4
    else
      var index1 = GlyphIndex(font, char)
      if index1<>GLYPH_NOT_FOUND then
        dim as long index2 = iif(i<nChars,GlyphIndex(font,txt[i+1]),0)
        if index2=GLYPH_NOT_FOUND then index2=0
        if GlyphProperties(font, fProps, gProps, index1,index2) = 0 then
          if cx+gProps.w>=maxw then cy += fProps.advanceHeight : cx=x
          var AlphaChannel = GlyphImageCreate(font, fProps, gProps,index1)
          if AlphaChannel then
            var glyph = ImageCreate(gProps.w,gProps.h,col)
            put glyph,(0,0),AlphaChannel,ALPHA
            put (cx,cy + gProps.y),glyph,ALPHA
            ImageDestroy glyph
            ImageDestroy AlphaChannel
          end if
          cx += gProps.advanceWidth + gProps.kernAdvance
        end if
      end if
    end if
  next
end sub

' print a wstring with truetype font
sub TTPrint (byval font as long, _
             byval x as integer, byval y as integer, _
             byref txt as wstring, _
             byval col as ulong=rgb(255,255,255), _
             byval size as integer=24)
  dim as FontProps  fProps
  dim as GlyphProps gProps
  dim as integer maxw,maxh,bytes,cx=x,cy=y
  if screenptr()=0 then return
  screeninfo maxw,maxh,,bytes
  if bytes<>4 then return
  if size<4 then return
  var nChars=len(txt) : if nChars<1 then return
  if FontPorperties(font, size, fprops) then return
  nChars-=1
  for i as integer = 0 to nChars
    var char = txt[i]
    if char<33 then
      if char=32 then cx+=size\4
    else
      var index1 = GlyphIndex(font, char)
      if index1<>GLYPH_NOT_FOUND then
        dim as long index2 = iif(i<nChars,GlyphIndex(font,txt[i+1]),0)
        if index2=GLYPH_NOT_FOUND then index2=0
        if GlyphProperties(font, fProps, gProps, index1,index2) = 0 then
          if cx+gProps.w>=maxw then cy += fProps.advanceHeight : cx=x
          var AlphaChannel = GlyphImageCreate(font, fProps, gProps,index1)
          if AlphaChannel then
            var glyph = ImageCreate(gProps.w,gProps.h,col)
            put glyph,(0,0),AlphaChannel,ALPHA
            put (cx,cy + gProps.y),glyph,ALPHA
            ImageDestroy glyph
            ImageDestroy AlphaChannel
          end if
          cx += gProps.advanceWidth + gProps.kernAdvance
        end if
      end if
    end if
  next
end sub

' #########
' # tools #
' #########
' tools for the draw string command
function LoadFontBitmap(byref fontbmpfile as string) as any ptr
  dim as long w,h
  dim as any ptr img
  var hFile = FreeFile()
  if open(fontbmpfile,for binary,access read, as #hFile) then
    ' WindowTitle "error: can't read " & fontbmpfile & "' !"
  else
    ' get width and height of the font bitmap.
    get #hFile,19,w
    get #hFile,  ,h : h=abs(h)
    close #hFile
    img = ImageCreate(w,h)
    if bload(fontbmpfile,img) then
      ' WindowTitle "error: can't load " & fontbmpfile & "' !"
      ImageDestroy img : img=0
    end if  
  end if
  return img
end function

function getFontInfo(byval fontImg as const any ptr, _
                     byref fontHeight as integer, _
                     byref firstChar  as integer, _
                     byref lastChar   as integer, _
                     byref widthTable as ubyte ptr) as boolean
  dim as ubyte ptr FontHeader
  if fontImg=0 then return false
  ' is it as an legal image
  if imageinfo(fontImg,,fontHeight,,,FontHeader) then return false
  dim as integer fontID
  fontID     = FontHeader[0]
  firstChar  = FontHeader[1]
  lastChar   = FontHeader[2]
  widthTable =@FontHeader[3]
  ' is it font version 0 and n has chars
  if (fontID<>0) orelse (fontHeight<4) orelse ((lastChar-firstChar)<1) then return false
  fontHeight-=1
  return true
end function

function getFontHeight(byval fontImg as const any ptr) as integer
  dim as integer fontHeight
  dim as integer firstChar
  dim as integer lastChar
  dim as ubyte ptr widthTable
  if getFontInfo(fontImg, _
                 fontHeight, _
                 firstChar,_
                 lastChar,_
                 widthTable)=false then return 0
  return fontHeight                 
end function

function getTextWidth(byval fontImg as const any ptr,byref text as const string) as integer
  dim as integer PixelWidth
  dim as integer fontHeight
  dim as integer firstChar
  dim as integer lastChar
  dim as ubyte ptr widthTable
  dim as integer textWidth
  dim as integer nChars = len(text)
  if nChars>0 then
   if getFontInfo(fontImg, _
                  fontHeight, _
                  firstChar,_
                  lastChar,_
                  widthTable) then
     for i as integer = 0 to nChars-1
       dim as integer char = text[i]
       if char<firstChar orelse char>lastChar then
        ' char out of range
       else
         textWidth += widthTable[char-firstChar]
       end if
     next  
   end if
  end if 
  return textWidth
end function


#endif ' __FBTrueType_bi__

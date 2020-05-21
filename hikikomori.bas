#include once "./FBTrueType/FBTrueType.bi"
#include "./fbsound-1.1/inc/fbsound_dynamic.bi"
randomize timer

sub slow (text as String )
  DIM as integer speed(0 to 4) => {50,100,20,1000,1500}
  for i as integer = 1 to len(text)
  print mid(text, i, 1);
SLEEP 100'speed(INT(RND*ubound(speed))) 
next
end sub

sub txtfile(f as string)
cls
	dim as string buffer
	dim h as long = freefile()
	open f for binary as #h
	buffer = space(lof(1))
	get #h,,buffer
	close #h
	

	slow buffer

end sub



Sub sound(f As String)
	Dim as boolean ok
	ok=fbs_Init()
	Dim as integer hWave
	fbs_Load_WAVFile(f,@hWave)
	fbs_Play_Wave(hWave, 3)
	if inkey<>"" then
      fbs_Destroy_Wave(@hWave)
   endif
end sub

sub opening()

screenres 800, 600, 32
sound("sabrina.wav") 
Dim As Any Ptr bild
Dim As string datei
Dim As Integer breite, hoehe

datei = "./hikpic.bmp"
breite = 800
hoehe = 600

bild = ImageCreate(breite, hoehe, 0)
BLoad datei, bild
Put (0, 0), bild, PSet

Sleep

ImageDestroy(bild)

var font = FontLoad("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf")

dim as string word = "A GAME BY RON77"
dim s as string = "HIKIKOMORY"

TTPrint font, 300 , 150, s,rgb(0, 255, 255), 50

ttprint font , 250, 200, word, rgb(255,0,255), 50


sleep

end sub

sub main()
screen 19
txtfile("startpoint.txt")

end sub

opening()
main()
sleep

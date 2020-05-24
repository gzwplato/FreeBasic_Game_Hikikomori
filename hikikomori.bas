#include once "./FBTrueType/FBTrueType.bi"
#include "./fbsound-1.1/inc/fbsound_dynamic.bi"
randomize timer
dim shared as integer timePass
redim shared newsbla(0) as string

'append to the string array the string item
SUB sAppend (arr() AS STRING, item AS STRING)
    REDIM Preserve arr(LBOUND(arr) TO UBOUND(arr) + 1) AS STRING
    arr(UBOUND(arr)) = item
end sub
'append to the integer array the integer item
SUB nAppend (arr() AS INTEGER, item AS INTEGER)
    REDIM Preserve arr(LBOUND(arr) TO UBOUND(arr) + 1) AS INTEGER
    arr(UBOUND(arr)) = item
end sub

sub cp (row as integer, s as string)

locate row, (100 - len(s)) / 2:print s

end sub

function _
  getKeys( _
    byref keysToCatch as const string ) _
  as string
  
  dim as string _
    k
  
  do
    k => inkey()
    
    sleep( 1, 1 )
  loop until( inStr( keysToCatch, k ) )
  
  '' Clear keyboard buffer
  do while( len( inkey() ) > 0 )
    sleep( 1, 1 )
  loop
  
  return( k )
end Function



Function dates1 (months As Integer) As String
  dim as const string _
    monthNames(0 To 11) => { _
      "January", _
      "February", _
      "March", _
      "April", _
      "May", _
      "June", _
      "July", _
      "August", _
      "September", _
      "October", _
      "November", _
      "December" }
    
    dim as integer _
      m => ( months + 11 ) mod 12
    
    return( monthNames( m ) & "," & str( int( ( months - 1 ) / 12 ) + 1997 ) )
END Function

sub slow (text as String )
  DIM as integer speed(0 to 4) => {50,100,20,1000,1500}
  for i as integer = 1 to len(text)
  print mid(text, i, 1);
SLEEP 100'speed(INT(RND*ubound(speed))) 
next
end sub

sub news()
dim l as string, h as integer
h = freefile()
dim k as string
cls
open "news.txt" for input as #h
while not eof(h)
line input #h, l
sappend newsbla(), l
wend
close #h
do
print newsbla(int(rnd* (ubound(newsbla)+1)))
sleep 3000
k = inkey()
loop until k = chr(27)
end sub

sub txtfile(f as string)
cls
	dim as string buffer
	dim h as long = freefile()
	open f for binary as #h
	buffer = space(lof(1))
	get #h,,buffer
	close #h
	

	'~ slow buffer
	print buffer
end sub



Sub sound(f As String, t as integer)
	Dim as boolean ok
	ok=fbs_Init()
	Dim as integer hWave
	fbs_Load_WAVFile(f,@hWave)
	fbs_Play_Wave(hWave, t)
	if inkey<>"" then
      fbs_Destroy_Wave(@hWave)
   endif
end sub


sub music()
cls
sound("./fbsound-1.1/data/fbsloop44.wav", 5)
txtfile("music.txt")
sleep
end sub


sub opening()

screenres 800, 600, 32
sound("sabrina.wav", 2) 
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

TTPrint font, 300 , 150, s,rgb(150, 255, 255), 50

ttprint font , 250, 200, word, rgb(255,80,255), 50


sleep

end sub

sub main()
screen 19
txtfile("startpoint.txt")
sleep
dim k as string
DO
exitif:
cls
cp 2, "DATE: " & dates1(timePass)
cp 4, "WHAT DO YOU WANT TO DO?"
cp 6, "1. GO TO SLEEP..."
cp 8, "2. PLAY A COMPUTER GAME..."
cp 10, "3. LISTEN TO MUSIC..."
cp 12, "4. CHECK E-MAIL..."
cp 14, "5. GO OUTSIDE..."
cp 16, "6. JOIN A CHAT-ROOM ON THE NET..."
cp 18, "7. READ A BOOK..."
cp 20, "8. GO AND END IT ALL!!!..."
cp 22, "9. LISTEN TO THE NEWS..."
cp 24, "PRESS ESC TO EXIT GAME..."
k = getkeys("123456789" + CHR(27))
if k = "9" then
news()
elseif k = "3" then
music()
elseif k ="1" then
timePass += 1
goto exitif
end if




timePass += 1
LOOP UNTIL k = chr(27)

end sub

opening()
main()
sleep

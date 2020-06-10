#include once ".\FBTrueType\FBTrueType.bi"
#include ".\fbsound-1.1\inc\fbsound_dynamic.bi"
'#include once "vlc/vlc.bi"
randomize timer
dim shared as integer timePass
redim shared newsbla(0) as string
dim shared as integer hWave
dim shared counter as integer 
dim shared songcounter as integer

type PERSON
	public:
	dim intext as string
	CONST punctuation = "?!,.:;<>(){}[]"
	dim Greeting AS STRING
	dim You AS STRING
	dim Script AS String
	dim kCnt AS INTEGER
	dim rCnt AS INTEGER
	dim wCnt AS INTEGER
	dim NoKeyFoundIndex AS INTEGER
	REDIM keywords(0) AS STRING
	redim replies(0) AS STRING
	redim wordIn(0) AS STRING
	redim wordOut(0) AS STRING
	REDIM rStarts(0) AS INTEGER
	redim rEnds(0) AS INTEGER
	redim rIndex(0) AS INTEGER
	
	
	declare SUB sAppend (arr() AS STRING, item AS STRING)
	declare SUB nAppend (arr() AS INTEGER, item AS INTEGER)
	declare SUB LoadArrays (scriptFile AS STRING)
	declare FUNCTION isolatePunctuation (s AS STRING) as string
	declare FUNCTION joinPunctuation (s AS STRING) as String
	declare FUNCTION GetReply (rply2 as string, switch as integer) as string
	declare SUB speakTotext (lines as string)
end type



'sub playvideo(f as string)
'cls
'dim as string TEST = f
''chdir exepath() 
'var instance = libvlc_new (0, NULL)
'var media    = libvlc_media_new_path (instance,TEST)
'var player   = libvlc_media_player_new_from_media (media)
'libvlc_media_release(media)
'libvlc_media_player_play(player)
'dim as long w,h,l,timeout=2000 ' 2 seconds
'print "wait on start ..."
'while w=0 andalso h=0 andalso l=0 andalso timeout>=0
'  w = libvlc_video_get_width(player)
'  h = libvlc_video_get_height(player)
'  l = libvlc_media_player_get_length(player)
'  sleep 100 : timeout-=100
'wend
'if timeout<0 then
'  libvlc_media_player_release(player)
'  libvlc_release(instance)
'  ?:?:?
'  print "play back not started !"
'  print "error log: in console ?"
'  beep:sleep:end
'end if
'print "size: " & w & " x " & h & " length: " & l\1000
'print "playing ..."
'print "sleep press any key"
'sleep
'libvlc_media_player_stop(player)
'libvlc_media_player_release(player)
'libvlc_release(instance)

'end sub

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

'append to the string array the string item
SUB PERSON.sAppend (arr() AS STRING, item AS STRING)
    REDIM Preserve arr(LBOUND(arr) TO UBOUND(arr) + 1) AS STRING
    arr(UBOUND(arr)) = item
END SUB

'append to the integer array the integer item
SUB PERSON.nAppend (arr() AS INTEGER, item AS INTEGER)
    REDIM Preserve arr(LBOUND(arr) TO UBOUND(arr) + 1) AS INTEGER
    arr(UBOUND(arr)) = item
END SUB

' pull data out of some script file
SUB PERSON.LoadArrays (scriptFile AS STRING)
    DIM startR AS INTEGER, endR AS INTEGER, ReadingR AS INTEGER, temp AS INTEGER
    DIM fline AS STRING, kWord AS STRING
    OPEN scriptFile FOR INPUT AS #1
    WHILE Not EOF(1)
        LINE INPUT #1, fline
        SELECT CASE LEFT(fline, 2)
           CASE "g:": Greeting = Trim(MID(fline, 3))
           CASE "y:": You = Trim(MID(fline, 3))
           CASE "c:": Script = Trim(MID(fline, 3))
            CASE "s:"
                wCnt = wCnt + 1: temp = INSTR(fline, ">")
                IF temp THEN
                    this.sAppend wordIn(), " " + Trim(MID(fline, 3, temp - 3)) + " "
                    this.sAppend wordOut(), " " + Trim(MID(fline, temp + 1)) + " "
                END IF
            CASE "r:"
                rCnt = rCnt + 1
                this.sAppend replies(), Trim(MID(fline, 3))
                IF NOT ReadingR THEN
                    ReadingR = -1
                    startR = rCnt
                END IF
            CASE "k:"
                IF ReadingR THEN
                    endR = rCnt
                    ReadingR = 0
                END IF
                IF rCnt THEN
                    kCnt = kCnt + 1
                    kWord = Trim(MID(fline, 3))
                    this.sAppend keywords(), " " + kWord + " "
                    this.nAppend rStarts(), startR
                    this.nAppend rIndex(), startR
                    this.nAppend rEnds(), endR
                    IF kWord = "nokeyfound" THEN NoKeyFoundIndex = kCnt
                END IF
            CASE "e:": EXIT WHILE
        END SELECT
    WEND
    CLOSE #1
    IF ReadingR THEN 'handle last bits
        endR = rCnt
        kCnt = kCnt + 1
        this.sAppend keywords(), "nokeyfound"
        this.nAppend rStarts(), startR
        this.nAppend rIndex(), startR
        this.nAppend rEnds(), endR
        NoKeyFoundIndex = kCnt
    END IF
END SUB


FUNCTION PERSON.isolatePunctuation (s AS STRING) as string
    'isolate punctuation so when we look for key words they don't interfere
    DIM b AS STRING, i AS INTEGER
    b = ""
    FOR i = 1 TO LEN(s)
        IF INSTR(punctuation, MID(s, i, 1)) > 0 THEN b = b + " " + MID(s, i, 1) + " " ELSE b = b + MID(s, i, 1)
    NEXT
    isolatePunctuation = b
END FUNCTION

FUNCTION PERSON.joinPunctuation (s AS STRING) as String
    'undo isolatePuntuation$
    DIM b AS STRING, find AS STRING, i AS INTEGER, place AS INTEGER
    b = s
    FOR i = 1 TO LEN(punctuation)
        find = " " + MID(punctuation, i, 1) + " "
        place = INSTR(b, find)
        WHILE place > 0
            IF place = 1 THEN
                b = MID(punctuation, i, 1) + MID(b, place + 3)
            ELSE
                b = MID(b, 1, place - 1) + MID(punctuation, i, 1) + MID(b, place + 3)
            END IF
            place = INSTR(b, find)
        WEND
    NEXT
    joinPunctuation = b
END Function

' =============================== here is the heart of ELIZA / Player function
FUNCTION PERSON.GetReply (rply2 as string, switch as integer) as string
    DIM inpt AS STRING, tail AS STRING, answ AS STRING
    DIM kFlag AS INTEGER, k AS INTEGER, kFound AS INTEGER, l AS INTEGER, w AS INTEGER
	if switch = 0 then
		
	' USER INPUT SECTION
	rply2 = ""
    PRINT You + ": ";: LINE INPUT "", inpt
    IF LCASE(inpt) = "q" OR LCASE(inpt) = "x" OR LCASE(inpt) = "goodbye" OR LCASE(inpt) = "good night" OR LCASE(inpt) = "bye" THEN
        GetReply = "Goodbye!": EXIT FUNCTION
    END IF
	else
    inpt = rply2
	endif
    inpt = " " + inpt + " " '<< need this because keywords embedded in spaces to ID whole words only
    inpt = this.isolatePunctuation(inpt)
    FOR k = 1 TO kCnt 'loop through key words until we find a match
        kFound = INSTR(LCASE(inpt), LCASE(keywords(k)))
        IF kFound > 0 THEN '>>> need the following for * in some replies
            tail = " " + MID(inpt, kFound + LEN(keywords(k)))
            FOR l = 1 TO LEN(tail) 'DO NOT USE INSTR
                FOR w = 1 TO wCnt 'swap words in tail if used there
                    IF LCASE(MID(tail, l, LEN(wordIn(w)))) = LCASE(wordIn(w)) THEN 'swap words exit for
                        tail = MID(tail, 1, l - 1) + wordOut(w) + MID(tail, l + LEN(wordIn(w)))
                        EXIT FOR
                    END IF
                NEXT w
            NEXT l
            kFlag = -1
            EXIT FOR
        END IF
    NEXT
    IF kFlag = 0 THEN k = NoKeyFoundIndex
    answ = replies(INT((rEnds(k) - rStarts(k) + 1) * RND) + rStarts(k))
    'set pointer to next reply in rIndex array
    IF k = NoKeyFoundIndex THEN 'let's not get too predictable for most used set of replies
        rIndex(k) = INT((rEnds(k) - rStarts(k) + 1) * RND) + rStarts(k)
        'ELSE
        '    rIndex(k) = rIndex(k) + 1 'set next reply index then check it
        '    IF rIndex(k) > rEnds(k) THEN rIndex(k) = rStarts(k)
    END IF
    IF RIGHT(answ, 1) <> "*" THEN GetReply = answ: EXIT FUNCTION 'oh so the * signal an append to reply!
    If Trim(tail) = "" THEN
        GetReply = "Please elaborate on, " + keywords(k)
    ELSE
        tail = this.joinPunctuation(tail)
        GetReply = MID(answ, 1, LEN(answ) - 1) + tail
    END IF
END FUNCTION

sub slow (text as String )
   DIM as integer speed(0 to 4) => {50,100,20,300,250}
   for i as integer = 1 to len(text)
      print mid(text, i, 1);
      SLEEP speed(INT(RND*ubound(speed))) 
   next
end sub

SUB PERSON.speakTotext (lines as string) 'uses voice command line voice.exe
    PRINT Script & ": ";:slow  lines :print :print

END Sub

sub conversation(file as String)
   dim i as Integer
   'restart()
   dim kenzu as PERSON
'   sound("./icq-horn.wav")
   cls
   for i = 1 to 10
      locate 5, 5
      print "             ";
      sleep 250
      locate 5, 5
      print "connecting...";
      sleep 250
   next
   cls
   DIM rply AS STRING
   dim rply3 as string'              for main loop
   kenzu.LoadArrays file '   check file load, OK checks out
   PRINT kenzu.Greeting: PRINT '           start testing main Eliza code
   DO
      rply = kenzu.GetReply(rply3, 0)
      PRINT: kenzu.speakTotext rply
   LOOP UNTIL rply = "Goodbye!"
   cls
   locate 5, 5
   print "disconnected...  "
   sleep
end sub

SUB chatroom()
	dim i as Integer
   
   dim guide as PERSON
   DIM mike AS PERSON
   DIM ken AS PERSON

   cls
   for i = 1 to 10
      locate 5, 5
      print "             ";
      sleep 250
      locate 5, 5
      print "connecting...";
      sleep 250
   next
   cls
	DIM in AS STRING
   DIM rply AS STRING
   dim rply2 as string'              for main loop
   DIM rply3 AS STRING
   guide.LoadArrays "yuka_guide.txt" '   check file load, OK checks out
   mike.LoadArrays "mike.txt"
   ken.LoadArrays "ken.txt"
   PRINT guide.Greeting: PRINT '           start testing main Eliza code
   DO
      PRINT: PRINT guide.You + ": ";: LINE INPUT "", in
    IF LCASE(in) = "q" OR LCASE(in) = "x" OR LCASE(in) = "goodbye" OR LCASE(in) = "good night" OR LCASE(in) = "bye" THEN
	PRINT: PRINT: guide.speakTotext "Goodbye!"
	EXIT do
	ENDIF
	rply = guide.GetReply(in, 1)
	PRINT: guide.speakTotext rply
	rply2 = mike.GetReply(in, 1)
	PRINT:mike.speakTotext rply2
	rply3 = ken.GetReply(in, 1)
	PRINT: ken.speakTotext rply3
      
   LOOP UNTIL in = "bye"
   cls
   locate 5, 5
   print "disconnected...  "
   sleep



END SUB

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

SUB upper (f as string)
	cls
	redim lines(0) as string
	dim h as long = freefile()
	dim as integer r =35,c =30
	dim fline as string
	OPEN f FOR INPUT AS #h
	WHILE Not EOF(h)
        LINE INPUT #h, fline
		sAppend lines(), fline		
    wend
	close #h

	dim as integer hi = hiword(width()) 'num columns on display
   'print closing credits
   for i as integer = 0 to ubound(lines)
      locate hi, 10
      print lines(i)
      sleep 500
   next
   'clear screen
   for i as integer = 1 to hi
      print
      sleep 500
   next
   print "End"
	getkey()
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
print "PRESS 'q' OR ESC TO STOP"
do
print newsbla(int(rnd* (ubound(newsbla)+1)))
print
sleep 3000
k = inkey()
loop until k = chr(27) or k = "q"
end sub

sub txtfile(f as string)
cls
	dim as string buffer
	dim h as long = freefile()
	open f for binary as #h
	buffer = space(lof(h))
	get #h,,buffer
	close #h

	print buffer
	sleep
	fbs_Destroy_Wave(@hWave)
end sub



Sub sound(f As String, t as integer)
	Dim as boolean ok
	ok=fbs_Init()
'	Dim as integer hWave
	fbs_Load_WAVFile(f,@hWave)
	fbs_Play_Wave(hWave, t)
	if inkey<>"" then
      fbs_Destroy_Wave(@hWave)
   endif
end sub

sub email()
	dim k as string
	
	cls
	if counter = 0 then
		cp 5, "NO EMAIL..."
		counter += 1
'	sleep
	elseif counter = 1 then
		sound("email1.wav",1)
		txtfile("email1.txt")
		print
		print
		print "1. REPLY OR 2. IGNORE"
	k = getKeys("12")
			if k = "2" then
			exit sub
		elseif k = "1" then
			print
			print
			print "YOU REPLY TO YUKA-YUKA CENTRE"
			counter += 1
		endif
	endif
	sleep
end sub


sub guitar()

if songcounter = 0 then
sound("pray_song.wav",1)
txtfile("guitar1.txt")
songcounter += 1
elseif songcounter = 1 then
sound("love_song.wav",1)
txtfile("guitar2.txt")
songcounter += 1
elseif songcounter = 2 then
sound("blues_song.wav", 1)
txtfile("guitar3.txt")



endif

end sub


sub dreams()
'dim as integer numbers(0 to 2) => {0,1,2}
dim index as integer
index = (int(rnd*(6)))
cls
if index = 0 then
sound("dream1.wav", 3)
txtfile("dream.txt")
elseif index = 1 then
sound("dream2.wav",3)
txtfile("dream2.txt")
elseif index = 2 then
sound("walk1.wav", 3)
txtfile("dream3.txt")
elseif index = 3 then
txtfile("nightmare1.txt")
elseif index = 4 then
sound("eddie.wav", 3)
txtfile("eddie.txt")
ELSEIF index = 5 THEN
sound("dream1.wav",3)
txtfile("dream_ta.txt")
endif
'print index
sleep
end sub

sub music()
cls
sound(".\fbsound-1.1\data\fbsloop44.wav", 5)
txtfile("music.txt")
sleep
end sub

sub outside()
sound("walk1.wav", 2)
txtfile("walk1.txt")

end sub

sub warningscreen()
screen 19
dim k as string
txtfile("warning.txt")
k = getKeys("12")
if k = "2" then
end
endif
end sub


sub opening()

screenres 800, 600, 32
sound("sabrina.wav", 2)
Dim As Any Ptr bild
Dim As string datei
Dim As Integer breite, hoehe

datei = "hikpic.bmp"
breite = 800
hoehe = 600

bild = ImageCreate(breite, hoehe, 0)
BLoad datei, bild
Put (0, 0), bild, PSet

Sleep

ImageDestroy(bild)

var font = FontLoad(".\fonts\Montserrat-Bold.ttf")

dim as string word = "A GAME BY RON77"
dim s as string = "HIKIKOMORY"

TTPrint font, 300 , 150, s,rgb(0, 255, 0), 50

ttprint font , 250, 200, word, rgb(0,80,255), 50


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
cp 20, "8. PLAY SOMETHING WITH YOUR GUITAR..."
cp 22, "9. LISTEN TO THE NEWS..."
cp 24, "PRESS ESC TO EXIT GAME..."
k = getkeys("123456789" + CHR(27))
if k = "9" then
news()
elseif k = "3" then
music()
elseif k ="1" then
'timePass += 1
'goto exitif
dreams()
elseif k = "5" then
outside()
elseif k ="4" then
email()
elseif k = "2" then
'playvideo("pacman.mp4")
shell("start https://www.youtube.com/watch?v=-CbyAk3Sn9I&t=15s")
elseif k = "6" then
'conversation("chat1.txt")
chatroom()
elseif k = "8" then
guitar()
end if




timePass += 1
LOOP UNTIL k = chr(27)

end sub

warningscreen()
opening()
main()
upper("ending_titles.txt")

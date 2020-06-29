#Include ONCE ".\FBTrueType\FBTrueType.bi"
#INCLUDE ".\fbsound-1.1\inc\fbsound_dynamic.bi"
WINDOWTITLE("HIKIKOMORI GAME")
RANDOMIZE TIMER
DIM SHARED AS INTEGER timePass
REDIM SHARED newsbla(0) AS STRING
'DIM SHARED AS long hWave
DIM SHARED counter AS INTEGER  ', index AS INTEGER
DIM SHARED songcounter AS INTEGER

TYPE PERSON
    PUBLIC  :
    DIM intext AS STRING
    CONST punctuation = "?!,.:;<>(){}[]"
    DIM Greeting AS STRING
    DIM You AS STRING
    DIM Script AS STRING
    DIM kCnt AS INTEGER
    DIM rCnt AS INTEGER
    DIM wCnt AS INTEGER
    DIM NoKeyFoundIndex AS INTEGER
    REDIM keywords(0) AS STRING
    REDIM replies(0) AS STRING
    REDIM wordIn(0) AS STRING
    REDIM wordOut(0) AS STRING
    REDIM rStarts(0) AS INTEGER
    REDIM rEnds(0) AS INTEGER
    REDIM rIndex(0) AS INTEGER
	
	
	DECLARE SUB sAppend(arr() AS STRING , Item AS STRING)
	DECLARE SUB nAppend(arr() AS INTEGER , Item AS INTEGER)
	DECLARE SUB LoadArrays(scriptFile AS STRING)
	DECLARE FUNCTION isolatePunctuation(s AS STRING) AS STRING
	DECLARE FUNCTION joinPunctuation(s AS STRING) AS STRING
	DECLARE FUNCTION GetReply(rply2 AS STRING , switch AS INTEGER) AS STRING
	DECLARE SUB speakTotext(lines AS STRING)
END TYPE


'APPEND TO the STRING array the STRING item
SUB sAppend(arr() AS STRING , Item AS STRING)
	REDIM PRESERVE arr(LBOUND(arr) TO UBOUND(arr) + 1) AS STRING
	arr(UBOUND(arr)) = Item
END SUB
'APPEND TO the INTEGER array the INTEGER item
SUB nAppend(arr() AS INTEGER , Item AS INTEGER)
	REDIM PRESERVE arr(LBOUND(arr) TO UBOUND(arr) + 1) AS INTEGER
	arr(UBOUND(arr)) = Item
END SUB

'APPEND TO the STRING array the STRING item
SUB PERSON.sAppend(arr() AS STRING , Item AS STRING)
	REDIM PRESERVE arr(LBOUND(arr) TO UBOUND(arr) + 1) AS STRING
	arr(UBOUND(arr)) = Item
END SUB

'APPEND TO the INTEGER array the INTEGER item
SUB PERSON.nAppend(arr() AS INTEGER , Item AS INTEGER)
	REDIM PRESERVE arr(LBOUND(arr) TO UBOUND(arr) + 1) AS INTEGER
	arr(UBOUND(arr)) = Item
END SUB

'pull DATA OUT of some script file
SUB PERSON.LoadArrays(scriptFile AS STRING)
	DIM startR AS INTEGER , endR AS INTEGER , ReadingR AS INTEGER , temp AS INTEGER
	DIM fline AS STRING , kWord AS STRING
	OPEN scriptFile FOR INPUT AS #1
	WHILE NOT EOF(1)
        LINE INPUT #1 , fline
        SELECT CASE LEFT(fline , 2)
        CASE "g:"  :  Greeting = TRIM(MID(fline , 3))
        CASE "y:"  :  You = TRIM(MID(fline , 3))
        CASE "c:"  :  Script = TRIM(MID(fline , 3))
        CASE "s:"
        wCnt = wCnt + 1  :  temp = INSTR(fline , ">")
        IF temp THEN
            THIS.sAppend wordIn() , " " + TRIM(MID(fline , 3 , temp - 3)) + " "
            THIS.sAppend wordOut() , " " + TRIM(MID(fline , temp + 1)) + " "
        END IF
    CASE "r:"
	rCnt = rCnt + 1
	THIS.sAppend replies() , TRIM(MID(fline , 3))
	IF NOT ReadingR THEN
        ReadingR = - 1
        startR = rCnt
    END IF
CASE "k:"
IF ReadingR THEN
	endR = rCnt
	ReadingR = 0
END IF
IF rCnt THEN
	kCnt = kCnt + 1
	kWord = TRIM(MID(fline , 3))
	THIS.sAppend keywords() , " " + kWord + " "
	THIS.nAppend rStarts() , startR
	THIS.nAppend rIndex() , startR
	THIS.nAppend rEnds() , endR
	IF kWord = "nokeyfound" THEN NoKeyFoundIndex = kCnt
END IF
CASE "e:"  :  EXIT WHILE
END SELECT
WEND
CLOSE #1
IF ReadingR THEN  'handle last bits
	endR = rCnt
	kCnt = kCnt + 1
	THIS.sAppend keywords() , "nokeyfound"
	THIS.nAppend rStarts() , startR
	THIS.nAppend rIndex() , startR
	THIS.nAppend rEnds() , endR
	NoKeyFoundIndex = kCnt
END IF
END SUB


FUNCTION PERSON.isolatePunctuation(s AS STRING) AS STRING
	'isolate punctuation so when we look FOR key words they don't interfere
	DIM b AS STRING , i AS INTEGER
	b = ""
	FOR i = 1 TO LEN(s)
        IF INSTR(punctuation , MID(s , i , 1)) > 0 THEN b = b + " " + MID(s , i , 1) + " " ELSE b = b + MID(s , i , 1)
    NEXT
	isolatePunctuation = b
END FUNCTION

FUNCTION PERSON.joinPunctuation(s AS STRING) AS STRING
	'undo isolatePuntuation$
	DIM b AS STRING , find AS STRING , i AS INTEGER , place AS INTEGER
	b = s
	FOR i = 1 TO LEN(punctuation)
        find = " " + MID(punctuation , i , 1) + " "
        place = INSTR(b , find)
        WHILE place > 0
            IF place = 1 THEN
                b = MID(punctuation , i , 1) + MID(b , place + 3)
            ELSE
                b = MID(b , 1 , place - 1) + MID(punctuation , i , 1) + MID(b , place + 3)
            END IF
            place = INSTR(b , find)
        WEND
    NEXT
	joinPunctuation = b
END FUNCTION

'=============================== here IS the heart of ELIZA / Player FUNCTION
FUNCTION PERSON.GetReply(rply2 AS STRING , switch AS INTEGER) AS STRING
	DIM inpt AS STRING , tail AS STRING , answ AS STRING
	DIM kFlag AS INTEGER , k AS INTEGER , kFound AS INTEGER , l AS INTEGER , w AS INTEGER
	IF switch = 0 THEN
        
        'USER INPUT SECTION
        rply2 = ""
        PRINT You + ": ";  :  LINE INPUT "" , inpt
        IF LCASE(inpt) = "q" OR LCASE(inpt) = "x" OR LCASE(inpt) = "goodbye" OR LCASE(inpt) = "good night" OR LCASE(inpt) = "bye" THEN
            GetReply = "Goodbye!"  :  EXIT FUNCTION
        END IF
    ELSE
        inpt = rply2
    END IF
	
	inpt = " " + inpt + " "  '<< need THIS because keywords embedded in spaces TO ID whole words only
	inpt = THIS.isolatePunctuation(inpt)
	FOR k = 1 TO kCnt  'LOOP through key words UNTIL we find a match
        kFound = INSTR(LCASE(inpt) , LCASE(keywords(k)))
        IF kFound > 0 THEN  '>>> need the following FOR * in some replies
            tail = " " + MID(inpt , kFound + LEN(keywords(k)))
            FOR l = 1 TO LEN(tail) 'DO NOT USE INSTR
                FOR w = 1 TO wCnt  'SWAP words in tail IF used there
                    IF LCASE(MID(tail , l , LEN(wordIn(w)))) = LCASE(wordIn(w)) THEN  'SWAP words EXIT FOR
                        tail = MID(tail , 1 , l - 1) + wordOut(w) + MID(tail , l + LEN(wordIn(w)))
                        EXIT FOR
                    END IF
                NEXT w
            NEXT l
            kFlag = - 1
            EXIT FOR
        END IF
    NEXT
	IF kFlag = 0 THEN k = NoKeyFoundIndex
	answ = replies(INT((rEnds(k) - rStarts(k) + 1) * RND) + rStarts(k))
	'set POINTER TO NEXT reply in rIndex array
	IF k = NoKeyFoundIndex THEN  'LET's NOT GET too predictable FOR most used set of replies
        rIndex(k) = INT((rEnds(k) - rStarts(k) + 1) * RND) + rStarts(k)
        'ELSE
        'rIndex(k) = rIndex(k) + 1 'set NEXT reply index THEN check it
        'IF rIndex(k) > rEnds(k) THEN rIndex(k) = rStarts(k)
    END IF
	IF RIGHT(answ , 1) <> "*" THEN GetReply = answ  :  EXIT FUNCTION  'oh so the * signal an APPEND TO reply!
	IF TRIM(tail) = "" THEN
        GetReply = "Please elaborate ON, " + keywords(k)
    ELSE
        tail = THIS.joinPunctuation(tail)
        GetReply = MID(answ , 1 , LEN(answ) - 1) + tail
    END IF
END FUNCTION

SUB slow(Text AS STRING)
	DIM AS INTEGER speed(0 TO 4) = > {50 , 100 , 20 , 300 , 250}
	FOR i AS INTEGER = 1 TO LEN(Text)
        PRINT MID(Text , i , 1) ;
        SLEEP speed(INT(RND * UBOUND(speed)))
    NEXT
END SUB

SUB PERSON.speakTotext(lines AS STRING) 'uses voice COMMAND LINE voice.exe
	PRINT Script & ": ";  :  slow lines
	PRINT
	
END SUB

SUB conversation(file AS STRING)
	DIM i AS INTEGER
	'restart()
	DIM kenzu AS PERSON
	'sound("./icq-horn.wav")
	CLS
	FOR i = 1 TO 10
        LOCATE 5 , 5
        PRINT "             ";
        SLEEP 250
        LOCATE 5 , 5
        PRINT "connecting...";
        SLEEP 250
    NEXT
	CLS
	DIM rply AS STRING
	DIM rply3 AS STRING  'FOR main LOOP
	kenzu.LoadArrays file  'check file load, OK checks OUT
	PRINT kenzu.Greeting  :  PRINT  'start testing main Eliza code
	DO
        rply = kenzu.GetReply(rply3 , 0)
        PRINT  :  kenzu.speakTotext rply  :  PRINT
    LOOP UNTIL rply = "Goodbye!"
	CLS
	LOCATE 5 , 5
	PRINT "disconnected...  "
	SLEEP
END SUB

SUB chatroom()
	DIM i AS INTEGER
	
	DIM guide AS PERSON
	DIM mike AS PERSON
	DIM ken AS PERSON
	
	CLS
	FOR i = 1 TO 10
        LOCATE 5 , 5
        PRINT "             ";
        SLEEP 250
        LOCATE 5 , 5
        PRINT "connecting...";
        SLEEP 250
    NEXT
	CLS
	DIM in AS STRING
	DIM rply AS STRING
	DIM rply2 AS STRING  'FOR main LOOP
	DIM rply3 AS STRING
	guide.LoadArrays "yuka_guide.txt"  'check file load, OK checks OUT
	mike.LoadArrays "mike.txt"
	ken.LoadArrays "ken.txt"
	PRINT guide.Greeting  :  PRINT  'start testing main Eliza code
	DO
        PRINT  :  PRINT guide.You + ": ";  :  LINE INPUT "" , in
        IF LCASE(in) = "q" OR LCASE(in) = "x" OR LCASE(in) = "goodbye" OR LCASE(in) = "good night" OR LCASE(in) = "bye" THEN
            PRINT  :  PRINT  :  guide.speakTotext "Goodbye!"
            EXIT DO
        END IF
        rply = guide.GetReply(in , 1)
        PRINT  :  guide.speakTotext rply
        rply2 = mike.GetReply(in , 1)
        PRINT  :  mike.speakTotext rply2
        rply3 = ken.GetReply(in , 1)
        PRINT  :  ken.speakTotext rply3
        
    LOOP UNTIL in = "bye"
	CLS
	LOCATE 5 , 5
	PRINT "disconnected...  "
	SLEEP
	
	
END SUB

SUB cp(Row AS INTEGER , s AS STRING)
	
	LOCATE Row , (100 - LEN(s)) / 2  :  PRINT s
	
END SUB

FUNCTION _
	getKeys(_
	BYREF keysToCatch AS CONST STRING) _
	AS STRING
	
	DIM AS STRING _
	k
	
	DO
        k = > INKEY()
        
        SLEEP(1 , 1)
    LOOP UNTIL (INSTR(keysToCatch , k))
	
	'CLEAR keyboard buffer
	DO WHILE (LEN(INKEY()) > 0)
        SLEEP(1 , 1)
    LOOP
	
	RETURN(k)
END FUNCTION


FUNCTION dates1(months AS INTEGER) AS STRING
	DIM AS CONST STRING _
	monthNames(0 TO 11) = > { _
	"January" , _
	"February" , _
	"March" , _
	"April" , _
	"May" , _
	"June" , _
	"July" , _
	"August" , _
	"September" , _
	"October" , _
	"November" , _
	"December" }
	
	DIM AS INTEGER _
	m = > (months + 11) MOD 12
	
	RETURN(monthNames(m) & "," & STR(INT((months - 1) / 12) + 1997))
END FUNCTION

SUB upper(f AS STRING)
	CLS
	REDIM lines(0) AS STRING
	DIM h AS LONG = FREEFILE()
	DIM AS INTEGER r = 35 , c = 30
	DIM fline AS STRING
	OPEN f FOR INPUT AS #h
	WHILE NOT EOF(h)
        LINE INPUT #h , fline
        sAppend lines() , fline
    WEND
	CLOSE #h
	
	DIM AS INTEGER hi = HIWORD(WIDTH()) 'num columns ON display
	'PRINT closing credits
	FOR i AS INTEGER = 0 TO UBOUND(lines)
        LOCATE hi , 10
        PRINT lines(i)
        SLEEP 500
    NEXT
	'CLEAR SCREEN
	FOR i AS INTEGER = 1 TO hi
        PRINT
        SLEEP 500
    NEXT
	PRINT "END"
	GETKEY()
END SUB

SUB news()
	DIM l AS STRING , h AS INTEGER
	h = FREEFILE()
	DIM k AS STRING
	CLS
	OPEN "news.txt" FOR INPUT AS #h
	WHILE NOT EOF(h)
        LINE INPUT #h , l
        sAppend newsbla() , l
    WEND
	CLOSE #h
	PRINT "PRESS 'q' OR ESC TO STOP"
	'FOR i AS INTEGER = 1 TO 1000
	'PRINT (INT(RND* (UBOUND(newsbla))));
	'NEXT
	'SLEEP
	DO
        PRINT newsbla(INT(RND * (UBOUND(newsbla) + 1)))
        PRINT
        SLEEP 3000
        k = INKEY()
    LOOP UNTIL k = CHR(27) OR k = "q"
END SUB

SUB txtfile(f AS STRING)
	CLS
	DIM AS STRING buffer
	DIM h AS LONG = FREEFILE()
	OPEN f FOR BINARY AS #h
	buffer = SPACE(LOF(h))
	GET #h ,  , buffer
	CLOSE #h
	PRINT buffer
	'SLEEP
	'fbs_Destroy_Wave(@hWave)
END SUB


SUB SOUND(f AS STRING , t AS INTEGER)
	DIM AS BOOLEAN ok
	ok = fbs_Init()
	DIM AS Integer hWave
	fbs_Load_WAVFile(f , @hWave)
	fbs_Play_Wave(hWave , t)
	Sleep
	fbs_Destroy_Wave(@hWave)
END SUB

SUB email()
	DIM k AS STRING
	
	CLS
	IF counter = 0 THEN
        cp 5 , "NO EMAIL..."
        counter += 1
        'SLEEP
    ELSEIF counter = 1 THEN
        txtfile("email1.txt")
        SOUND("email1.wav" , 1)
        
        PRINT
        PRINT
        PRINT "1. REPLY OR 2. IGNORE"
        k = getKeys("12")
        IF k = "2" THEN
            EXIT SUB
        ELSEIF k = "1" THEN
            PRINT
            PRINT
            PRINT "YOU REPLY TO YUKA-YUKA CENTER"
            counter += 1
        END IF
    END IF
	SLEEP
END SUB


SUB guitar()
	
	IF songcounter = 0 THEN
        txtfile("guitar1.txt")
        SOUND("pray_song.wav" , 1)
        songcounter += 1
    ELSEIF songcounter = 1 THEN
        txtfile("guitar2.txt")
        SOUND("love_song.wav" , 1)
        songcounter += 1
    ELSEIF songcounter = 2 THEN
        txtfile("guitar3.txt")
        SOUND("blues_song.wav" , 1)
        songcounter += 1
    ELSEIF songcounter = 3 THEN
        txtfile("pray2.txt")
        SOUND("pray_song2.wav" , 1)
        songcounter += 1
    ELSEIF songcounter = 4 THEN
        txtfile("guitar4.txt")
        SOUND("guitar_song.wav" , 1)
        songcounter = 0
    END IF
	
END SUB


SUB dreams()
	
	DIM AS STRING dream(0 TO 6) = > {"dream.txt" , "dream2.txt" , "dream3.txt" , "nightmare1.txt" , "eddie.txt" , "dream_ta.txt" , "johnny.txt"}
	DIM AS STRING dreamMusic(0 TO 4) = > {"dream1.wav" , "dream2.wav" , "walk1.wav" , "eddie.wav" , "dream_eva.wav"}
	CLS
	txtfile(dream(INT(RND * (UBOUND(dream) + 1))))
	SOUND(dreamMusic(INT(RND * (UBOUND(dreammusic) + 1))) , 3)
		
	SLEEP
END SUB

SUB music()
	CLS
	txtfile("music.txt")
	SOUND(".\fbsound-1.1\DATA\fbsloop44.wav" , 5)
	
	SLEEP
END SUB

SUB outside()
	txtfile("walk1.txt")
	SOUND("walk1.wav" , 2)
	
	
END SUB

SUB warningscreen()
	SCREEN 19
	DIM k AS STRING
	txtfile("warning.txt")
	k = getKeys("12")
	IF k = "2" THEN
        END
    END IF
END SUB


SUB opening()
	
	SCREENRES 800 , 600 , 32
	'SOUND("sabrina.wav" , 2)
	DIM AS ANY PTR bild
	DIM AS STRING datei
	DIM AS INTEGER breite , hoehe
	
	datei = "hikpic.bmp"
	breite = 800
	hoehe = 600
	
	bild = IMAGECREATE(breite , hoehe , 0)
	BLOAD datei , bild
	PUT(0 , 0) , bild , PSET
	
	SLEEP
	'SOUND("sabrina.wav" , 2)
	IMAGEDESTROY(bild)
	
	VAR Font = FontLoad(".\fonts\Montserrat-Bold.ttf")
	
	DIM AS STRING WORD = "A GAME BY RON77"
	DIM s AS STRING = "HIKIKOMORY"
	
	TTPrint Font , 300 , 150 , s , RGB(0 , 255 , 0) , 50
	
	ttprint Font , 250 , 200 , WORD , RGB(0 , 80 , 255) , 50
	
	SOUND("sabrina.wav" , 2)
	SLEEP
	
END SUB

SUB main()
	SCREEN 19
	txtfile("startpoint.txt")
	SLEEP
	DIM k AS STRING
	DO
        exitif  :
        CLS
        cp 2 , "DATE: " & dates1(timePass)
        cp 4 , "WHAT DO YOU WANT TO DO?"
        cp 6 , "1. GO TO SLEEP..."
        cp 8 , "2. PLAY A COMPUTER GAME..."
        cp 10 , "3. LISTEN TO MUSIC..."
        cp 12 , "4. CHECK E-MAIL..."
        cp 14 , "5. GO OUTSIDE..."
        cp 16 , "6. JOIN A CHAT-ROOM ON THE NET..."
        cp 18 , "7. READ A BOOK..."
        cp 20 , "8. PLAY SOMETHING WITH YOUR GUITAR..."
        cp 22 , "9. LISTEN TO THE NEWS..."
        cp 24 , "PRESS ESC TO EXIT GAME..."
        k = getkeys("123456789" + CHR(27))
        IF k = "9" THEN
            news()
        ELSEIF k = "3" THEN
            music()
        ELSEIF k = "1" THEN
            'timePass += 1
            'GOTO exitif
            dreams()
        ELSEIF k = "5" THEN
            outside()
        ELSEIF k = "4" THEN
            email()
        ELSEIF k = "2" THEN
            'playvideo("pacman.mp4")
            SHELL("start https://www.youtube.com/watch?v=-CbyAk3Sn9I&t=15s")
        ELSEIF k = "6" THEN
            IF counter < 2 THEN
                conversation("chat1.txt")
            ELSEIF counter = 2 THEN
                chatroom()
            END IF
        ELSEIF k = "8" THEN
            guitar()
        END IF
        
        
        timePass += 1
    LOOP UNTIL k = CHR(27)
	
END SUB

warningscreen()
opening()
main()
upper("ending_titles.txt")



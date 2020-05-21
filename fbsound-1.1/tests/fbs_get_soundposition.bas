'  #############################
' # fbs_get_soundposition.bas #
'#############################
' short test for:
' fbs_Get_SoundPosition()

'#include "../inc/fbsound.bi"
#include "../inc/fbsound_dynamic.bi"

const data_path = "../data/"

' only if not same as exe path
' fbs_Set_PlugPath("./")

dim as integer hWave,hSound
dim as integer scr_w
screeninfo scr_w:scr_w*=0.8
screenres scr_w,100
windowtitle "[any key] = quit fbs_Get_SoundPosition()"

fbs_Init()
fbs_Load_MP3File(data_path & "fox.mp3",@.hWave)
fbs_Create_Sound(hWave,@hSound)
fbs_Play_Sound(hSound)
dim as ubyte c=1
dim as single position
while inkey()=""
  if fbs_Get_SoundPosition(hSound,@Position) then
    ' end of sound trigger new playback
    if Position>=1.0 then fbs_Play_Sound(hSound):c+=1
    dim as integer x=Position*scr_w
    line(x-3,0)-step(6,99),c,BF
  end if
  sleep(5)
wend

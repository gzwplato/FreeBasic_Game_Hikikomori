'  ########################################
' # fbs_load_and_reloading_mp3stream.bas #
'########################################

' simple speed test
' streaming decoding and reloading MP3.

'#include "../inc/fbsound.bi"
#include "../inc/fbsound_dynamic.bi"

const data_path = "../data/"

' only if not same as exe path
' fbs_Set_PlugPath("./")

dim as boolean ok
ok=fbs_Init()
if ok=false then
  ? "error: fbs_Init() !"
  beep:sleep:end 1
end if

? "any key = quit"
while inkey=""
  ? "create stream: ";
  fbs_Create_MP3Stream(data_path & "atem.mp3")
  ? "ok"
  ? "wait on start: ";
  fbs_Play_MP3Stream()
  while fbs_Get_PlayingStreams()=0:sleep 10:wend
  ? "ok"
  ? "wait on end  : ";
  while fbs_Get_PlayingStreams()>0:sleep 10:wend
  ? "ok":?
  sleep 100
wend 
end



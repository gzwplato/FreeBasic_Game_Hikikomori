#include once "../inc/fbsound_oop.bi"
' Sound.Play([nLoops])
' Samples.Length 
var Device  = SoundDevice()
var Samples = SampleBuffer("../data/jimi.mod")
var Sound   = SoundBuffer(Samples)
Sound.Play
print "play time: " & Samples.Length & " seconds"
print "press any key ..."
sleep

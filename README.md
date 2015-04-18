riottracker
===========

text-based music tracker

#### planned features
* internal sample generation (composing basic wave functions)
* (support for MIDI input)
* (.XM support)
* playing and editing simultaneously
* render tracks directly to .wav

#### Howto
As of now, this is just a simple test on how to sequence sound with Haskell.
Some sound is generated using a simple sample. Since the data output is RAW
WAV data, one must pipe it into an appropriate program. To get to listen to
the sound, one could use the following command:
`runhaskell Main.hs | play -traw -B -r 44100 -e signed -b 32 -`

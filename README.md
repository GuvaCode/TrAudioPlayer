# TrAudioPlayer - Audio Component for Lazarus

## Overview
Universal audio playback component built on **rAudio** (Raylib backend) with multi-format support and extensible interface architecture.

## Key Features

- 🎵 **Multi-format**: MP3, WAV, QOA via rAudio + ZxTune, VGMPlay, OpenMPT, WavPack, LibSndFile
- 🎛️ **band EQ**: Professional equalizer (all players EXCEPT rAudio)
- 🔧 **Extensible**: Add custom players via `IMusicPlayer` interface
- 🔄 **Auto-conversion**: 24-bit → 16-bit for compatibility
- 📱 **Cross-platform**: Windows, Linux

## Supported Formats

| Backend | Formats | Equalizer |
|---------|---------|-----------|
| rAudio (Default) | MP3, WAV, QOA, OGG, FLAC | ❌ Not supported |
| ZxTune | ZX Spectrum, Amiga, Atari | ✅ Supported |
| VGMPlay | VGM, SPC, NSF, GYM | ✅ Supported |
| OpenMPT | MOD, XM, IT, S3M | ✅ Supported |
| WavPack | WV (lossless) | ✅ Supported |
| LibSndFile | 40+ formats incl. AIFF, AU | ✅ Supported |

## Important Limitations

- 🚫 **Single instance**: Only one player per application
- 🎚️ **EQ restrictions**: Works with ALL players EXCEPT DefaultPlayer
- 💽 **Bit depth**: 24-bit files auto-converted to 16-bit (quality loss)

## Band Equalizer Frequencies

| Band | Frequency | Control Range |
|------|-----------|---------------|
| 0 | 32 Hz | -12dB to +12dB |
| 1 | 64 Hz | -12dB to +12dB |
| 2 | 125 Hz | -12dB to +12dB |
| 3 | 250 Hz | -12dB to +12dB |
| 4 | 500 Hz | -12dB to +12dB |
| 5 | 1 kHz | -12dB to +12dB |
| 6 | 2 kHz | -12dB to +12dB |
| 7 | 4 kHz | -12dB to +12dB |
| 8 | 8 kHz | -12dB to +12dB |
| 9 | 16 kHz | -12dB to +12dB |



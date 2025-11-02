{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rAudio;

{$warn 5023 off : no warning about unused units}
interface

uses
  rAudioPlayer, DefaultAudioPlayer, ZxTuneAudioPlayer, VgmAudioPlayer, 
  WavPackAudioPlayer, SndFileAudioPlayer, XmpAudioPlayer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('rAudioPlayer', @rAudioPlayer.Register);
end;

initialization
  RegisterPackage('rAudio', @Register);
end.

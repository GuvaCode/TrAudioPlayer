unit libraudio;

(*
  project : Free Pascal header for libraudio
  author  : TRon
  date    : nov 2024
  -------------------------------------------------------------------
  name    : raudio (raylib 5.5)
  author  : Ramon Santamaria
  version : 1.1.0
  rev     : 711c86eae17db9a94af575f7a5b496244b48b22d
  repo    : https://github.com/raysan5/raudio
*)

{$mode objfpc}{$h+}
{$packrecords c}

interface

uses
  ctypes;


const
  {$ifdef linux}
  library_name = 'libraudio.so';
  {$endif}
  {$ifdef windows}
  library_name = 'libraudio.dll';
  {$endif}
type
  TAudioCallback = procedure(buffferData: pointer; frames: cuint); cdecl;


  // Wave, audio wave data
  PWave = ^TWave;
  TWave = record
    frameCount : cuint;    // Total number of frames (considering channels)
    sampleRate : cuint;    // Frequency (samples per second)
    sampleSize : cuint;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    channels   : cuint;    // Number of channels (1-mono, 2-stereo, ...)
    data       : pointer;  // Buffer data pointer
  end;

  // Opaque structs declaration
  PrAudioBuffer = ^TrAudioBuffer;
  TrAudioBuffer = record end;

  PrAudioProcessor = ^TrAudioProcessor;
  TrAudioProcessor = record end;

  // AudioStream, custom audio stream
  TAudioStream = record
    buffer     : PrAudioBuffer;    // Pointer to internal data used by the audio system
    processor  : PrAudioProcessor; // Pointer to internal data processor, useful for audio effects
    sampleRate : cuint;            // Frequency (samples per second)
    sampleSize : cuint;            // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    channels   : cuint;            // Number of channels (1-mono, 2-stereo, ...)
  end;

  // Sound
  PSound = ^TSound;
  TSound = record
    stream     : TAudioStream;    // Audio stream
    frameCount : cuint;           // Total number of frames (considering channels)
  end;

  // Music, audio stream, anything longer than ~10 seconds should be streamed
  PMusic = ^TMusic;
  TMusic = record
    stream     : TAudioStream; // Audio stream
    frameCount : cuint;        // Total number of frames (considering channels)
    looping    : boolean;        // Music looping enable

    ctxType    : cint;         // Type of music context (audio filetype)
    ctxData    : pointer;      // Audio context data, depends on type
  end;

//----------------------------------------------------------------------------------
// Global Variables Definition
//----------------------------------------------------------------------------------
//...

//----------------------------------------------------------------------------------
// Module Functions Declaration
//----------------------------------------------------------------------------------

var
  // Audio device management functions
  InitAudioDevice                : procedure();                                                                    cdecl; // Initialize audio device and context
  CloseAudioDevice               : procedure();                                                                    cdecl; // Close the audio device and context
  IsAudioDeviceReady             : function (): cbool;                                                             cdecl; // Check if audio device has been initialized successfully
  SetMasterVolume                : procedure(volume: cfloat);                                                      cdecl; // Set master volume (listener)
  GetMasterVolume                : function (): cfloat;                                                            cdecl; // Get master volume (listener)

  // Wave/Sound loading/unloading functions
  LoadWave                       : function (const fileName: pchar): TWave;                                        cdecl; // Load wave data from file
  LoadWaveFromMemory             : function (const fileType: pchar; const fileData: pbyte; dataSize: cint): TWave; cdecl; // Load wave from memory buffer, fileType refers to extension: i.e. ".wav"
  IsWaveReady                    : function (wave: TWave): cbool;                                                  cdecl; // Checks if wave data is ready
  LoadSound                      : function (const fileName: pchar): TSound;                                       cdecl; // Load sound from file
  LoadSoundFromWave              : function (wave: TWave): TSound;                                                 cdecl; // Load sound from wave data
  LoadSoundAlias                 : function (source: TSound): TSound;                                              cdecl; // Create a new sound that shares the same sample data as the source sound, does not own the sound data
  IsSoundReady                   : function (sound: TSound): cbool;                                                cdecl; // Checks if a sound is ready
  UpdateSound                    : procedure(sound: TSound; const data: pointer; frameCount: cint);                cdecl; // Update sound buffer with new data
  UnloadWave                     : procedure(wave: TWave);                                                         cdecl; // Unload wave data
  UnloadSound                    : procedure(sound: TSound);                                                       cdecl; // Unload sound
  UnloadSoundAlias               : procedure(&alias: TSound);                                                      cdecl; // Unload a sound alias (does not deallocate sample data)
  ExportWave                     : function (wave: TWave; const fileName: pchar): cbool;                           cdecl; // Export wave data to file, returns true on success
  ExportWaveAsCode               : function (wave: TWave; const fileName: pchar): cbool;                           cdecl; // Export wave sample data to code (.h), returns true on success

  // Wave/Sound management functions
  PlaySound                      : procedure(sound: TSound);                                                       cdecl; // Play a sound
  StopSound                      : procedure(sound: TSound);                                                       cdecl; // Stop playing a sound
  PauseSound                     : procedure(sound: TSound);                                                       cdecl; // Pause a sound
  ResumeSound                    : procedure(sound: TSound);                                                       cdecl; // Resume a paused sound
  IsSoundPlaying                 : function (sound: TSound): cbool;                                                cdecl; // Check if a sound is currently playing
  SetSoundVolume                 : procedure(sound: TSound; volume: cfloat);                                       cdecl; // Set volume for a sound (1.0 is max level)
  SetSoundPitch                  : procedure(sound: TSound; pitch: cfloat);                                        cdecl; // Set pitch for a sound (1.0 is base level)
  SetSoundPan                    : procedure(sound: TSound; pan: cfloat);                                          cdecl; // Set pan for a sound (0.0 to 1.0, 0.5=center)
  WaveCopy                       : function (wave: TWave): TWave;                                                  cdecl; // Copy a wave to a new wave
  WaveCrop                       : procedure(wave: PWave; initFrame: cint; finalFrame: cint);                      cdecl; // Crop a wave to defined samples range
  WaveFormat                     : procedure(wave: PWave; sampleRate: cint; sampleSize: cint; channels: cint);     cdecl; // Convert wave data to desired format
  LoadWaveSamples                : function (wave: TWave): pcfloat;                                                cdecl; // Load samples data from wave as a floats array
  UnloadWaveSamples              : procedure(samples: pcfloat);                                                    cdecl; // Unload samples data loaded with LoadWaveSamples()

  // Music management functions
  LoadMusicStream                : function (const fileName: pchar): TMusic;                                       cdecl; // Load music stream from file
  LoadMusicStreamFromMemory      : function (const fileType: pchar; const data: pcuchar; dataSize: cint): TMusic;  cdecl; // Load music stream from data
  IsMusicReady                   : function (music: TMusic): cbool;                                                cdecl; // Checks if a music stream is ready
  UnloadMusicStream              : procedure(music: TMusic);                                                       cdecl; // Unload music stream
  PlayMusicStream                : procedure(music: TMusic);                                                       cdecl; // Start music playing
  IsMusicStreamPlaying           : function (music: TMusic): cbool;                                                cdecl; // Check if music is playing
  UpdateMusicStream              : procedure(music: TMusic);                                                       cdecl; // Updates buffers for music streaming
  StopMusicStream                : procedure(music: TMusic);                                                       cdecl; // Stop music playing
  PauseMusicStream               : procedure(music: TMusic);                                                       cdecl; // Pause music playing
  ResumeMusicStream              : procedure(music: TMusic);                                                       cdecl; // Resume playing paused music
  SeekMusicStream                : procedure(music: TMusic; position: cfloat);                                     cdecl; // Seek music to a position (in seconds)
  SetMusicVolume                 : procedure(music: TMusic; volume: cfloat);                                       cdecl; // Set volume for music (1.0 is max level)
  SetMusicPitch                  : procedure(music: TMusic; pitch: cfloat);                                        cdecl; // Set pitch for a music (1.0 is base level)
  SetMusicPan                    : procedure(music: TMusic; pan: cfloat);                                          cdecl; // Set pan for a music (0.0 to 1.0, 0.5=center)
  GetMusicTimeLength             : function (music: TMusic): cfloat;                                               cdecl; // Get music time length (in seconds)
  GetMusicTimePlayed             : function (music: TMusic): cfloat;                                               cdecl; // Get current music time played (in seconds)

  // AudioStream management functions
  LoadAudioStream                : function (sampleRate: cuint; sampleSize: cuint; channels: cuint): TAudioStream; cdecl; // Load audio stream (to stream raw audio pcm data)
  IsAudioStreamReady             : function (stream: TAudioStream): cbool;                                         cdecl; // Checks if an audio stream is ready
  UnloadAudioStream              : procedure(stream: TAudioStream);                                                cdecl; // Unload audio stream and free memory
  UpdateAudioStream              : procedure(stream: TAudioStream; const data: pointer; samplesCount: cint);       cdecl; // Update audio stream buffers with data
  IsAudioStreamProcessed         : function (stream: TAudioStream): cbool;                                         cdecl; // Check if any audio stream buffers requires refill
  PlayAudioStream                : procedure(stream: TAudioStream);                                                cdecl; // Play audio stream
  PauseAudioStream               : procedure(stream: TAudioStream);                                                cdecl; // Pause audio stream
  ResumeAudioStream              : procedure(stream: TAudioStream);                                                cdecl; // Resume audio stream
  IsAudioStreamPlaying           : function (stream: TAudioStream): cbool;                                         cdecl; // Check if audio stream is playing
  StopAudioStream                : procedure(stream: TAudioStream);                                                cdecl; // Stop audio stream
  SetAudioStreamVolume           : procedure(stream: TAudioStream; volume: cfloat);                                cdecl; // Set volume for audio stream (1.0 is max level)
  SetAudioStreamPitch            : procedure(stream: TAudioStream; pitch: cfloat);                                 cdecl; // Set pitch for audio stream (1.0 is base level)
  SetAudioStreamPan              : procedure(stream: TAudioStream; pan: cfloat);                                   cdecl; // Set pan for audio stream  (0.0 to 1.0, 0.5=center)
  SetAudioStreamBufferSizeDefault: procedure(size: cint);                                                          cdecl; // Default size for new audio streams
  SetAudioStreamCallback         : procedure(stream: TAudioStream; callback: TAudioCallback);                      cdecl; // Audio thread callback to request new data

  AttachAudioStreamProcessor     : procedure(stream: TAudioStream; processor: TAudioCallback);                     cdecl; // Attach audio stream processor to stream
  DetachAudioStreamProcessor     : procedure(stream: TAudioStream; processor: TAudioCallback);                     cdecl; // Detach audio stream processor from stream

  AttachAudioMixedProcessor      : procedure(processor: TAudioCallback);                                           cdecl; // Attach audio stream processor to the entire audio pipeline
  DetachAudioMixedProcessor      : procedure(processor: TAudioCallback);                                           cdecl; // Detach audio stream processor from the entire audio pipeline

  procedure LoadLib_rAudio(const aLibName: string);
  function FindLibName(PathName, aLibName: string): string;
  function rAudioLoaded: Boolean;

implementation

uses
  sysutils, dynlibs;

var
  library_handle: TLibHandle;



function FindLibName(PathName, aLibName: string): string;
begin
  if FileExists(PathName + '/' + aLibName) then
  begin
      FindLibName := PathName + '/' + aLibName;
      exit;
  end;
  FindLibName := aLibName;
end;

function rAudioLoaded: Boolean;
begin
  Result := library_handle <> NilHandle;
end;

procedure LoadLibFn(var fn_var; const fn_name: string);
begin
  pointer(fn_var) := GetProcedureAddress(library_handle, fn_name);
end;

procedure LoadLib_rAudio(const aLibName: string);
begin
  library_handle := LoadLibrary(aLibName);

  if library_handle = NilHandle then
  begin
    writeln(GetLoadErrorStr);

    runError(2);
  end;

  // Audio device management functions
  LoadLibFn(InitAudioDevice                , 'InitAudioDevice'                );
  LoadLibFn(CloseAudioDevice               , 'CloseAudioDevice'               );
  LoadLibFn(IsAudioDeviceReady             , 'IsAudioDeviceReady'             );
  LoadLibFn(SetMasterVolume                , 'SetMasterVolume'                );
  LoadLibFn(GetMasterVolume                , 'GetMasterVolume'                );

  // Wave/Sound loading/unloading functions
  LoadLibFn(LoadWave                       , 'LoadWave'                       );
  LoadLibFn(LoadWaveFromMemory             , 'LoadWaveFromMemory'             );
  LoadLibFn(IsWaveReady                    , 'IsWaveReady'                    );
  LoadLibFn(LoadSound                      , 'LoadSound'                      );
  LoadLibFn(LoadSoundFromWave              , 'LoadSoundFromWave'              );
  LoadLibFn(LoadSoundAlias                 , 'LoadSoundAlias'                 );
  LoadLibFn(IsSoundReady                   , 'IsSoundReady'                   );
  LoadLibFn(UpdateSound                    , 'UpdateSound'                    );
  LoadLibFn(UnloadWave                     , 'UnloadWave'                     );
  LoadLibFn(UnloadSound                    , 'UnloadSound'                    );
  LoadLibFn(UnloadSoundAlias               , 'UnloadSoundAlias'               );
  LoadLibFn(ExportWave                     , 'UnloadSoundAlias'               );
  LoadLibFn(ExportWaveAsCode               , 'UnloadSoundAlias'               );

  // Wave/Sound management functions
  LoadLibFn(PlaySound                      , 'PlaySound'                      );
  LoadLibFn(StopSound                      , 'StopSound'                      );
  LoadLibFn(PauseSound                     , 'PauseSound'                     );
  LoadLibFn(ResumeSound                    , 'ResumeSound'                    );
  LoadLibFn(IsSoundPlaying                 , 'IsSoundPlaying'                 );
  LoadLibFn(SetSoundVolume                 , 'SetSoundVolume'                 );
  LoadLibFn(SetSoundPitch                  , 'SetSoundPitch'                  );
  LoadLibFn(SetSoundPan                    , 'SetSoundPan'                    );
  LoadLibFn(WaveCopy                       , 'WaveCopy'                       );
  LoadLibFn(WaveCrop                       , 'WaveCrop'                       );
  LoadLibFn(WaveFormat                     , 'WaveFormat'                     );
  LoadLibFn(LoadWaveSamples                , 'LoadWaveSamples'                );
  LoadLibFn(UnloadWaveSamples              , 'UnloadWaveSamples'              );

  // Music management functions
  LoadLibFn(LoadMusicStream                , 'LoadMusicStream'                );
  LoadLibFn(LoadMusicStreamFromMemory      , 'LoadMusicStreamFromMemory'      );
  LoadLibFn(IsMusicReady                   , 'IsMusicReady'                   );
  LoadLibFn(UnloadMusicStream              , 'UnloadMusicStream'              );
  LoadLibFn(PlayMusicStream                , 'PlayMusicStream'                );
  LoadLibFn(IsMusicStreamPlaying           , 'IsMusicStreamPlaying'           );
  LoadLibFn(UpdateMusicStream              , 'UpdateMusicStream'              );
  LoadLibFn(StopMusicStream                , 'StopMusicStream'                );
  LoadLibFn(PauseMusicStream               , 'PauseMusicStream'               );
  LoadLibFn(ResumeMusicStream              , 'ResumeMusicStream'              );
  LoadLibFn(SeekMusicStream                , 'SeekMusicStream'                );
  LoadLibFn(SetMusicVolume                 , 'SetMusicVolume'                 );
  LoadLibFn(SetMusicPitch                  , 'SetMusicPitch'                  );
  LoadLibFn(SetMusicPan                    , 'SetMusicPan'                    );
  LoadLibFn(GetMusicTimeLength             , 'GetMusicTimeLength'             );
  LoadLibFn(GetMusicTimePlayed             , 'GetMusicTimePlayed'             );

  // AudioStream management functions
  LoadLibFn(LoadAudioStream                , 'LoadAudioStream'                );
  LoadLibFn(IsAudioStreamReady             , 'IsAudioStreamReady'             );
  LoadLibFn(UnloadAudioStream              , 'UnloadAudioStream'              );
  LoadLibFn(UpdateAudioStream              , 'UpdateAudioStream'              );
  LoadLibFn(IsAudioStreamProcessed         , 'IsAudioStreamProcessed'         );
  LoadLibFn(PlayAudioStream                , 'PlayAudioStream'                );
  LoadLibFn(PauseAudioStream               , 'PauseAudioStream'               );
  LoadLibFn(ResumeAudioStream              , 'ResumeAudioStream'              );
  LoadLibFn(IsAudioStreamPlaying           , 'IsAudioStreamPlaying'           );
  LoadLibFn(StopAudioStream                , 'StopAudioStream'                );
  LoadLibFn(SetAudioStreamVolume           , 'SetAudioStreamVolume'           );
  LoadLibFn(SetAudioStreamPitch            , 'SetAudioStreamPitch'            );
  LoadLibFn(SetAudioStreamPan              , 'SetAudioStreamPan'              );
  LoadLibFn(SetAudioStreamBufferSizeDefault, 'SetAudioStreamBufferSizeDefault');
  LoadLibFn(SetAudioStreamCallback         , 'SetAudioStreamCallback'         );

  LoadLibFn(AttachAudioStreamProcessor     , 'AttachAudioStreamProcessor'     );
  LoadLibFn(DetachAudioStreamProcessor     , 'DetachAudioStreamProcessor'     );

  LoadLibFn(AttachAudioMixedProcessor      , 'AttachAudioMixedProcessor'      );
  LoadLibFn(DetachAudioMixedProcessor      , 'DetachAudioMixedProcessor'      );
end;


initialization

finalization
 // if library_handle <> NilHandle then
  //  UnloadLibrary(library_handle);
end.

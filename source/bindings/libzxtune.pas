unit libzxtune;

{$mode objfpc}{$h+}
{$packrecords c}

interface

uses
  SysUtils, dynlibs;

const
  {$IFDEF MSWINDOWS}
  DEFAULT_LIB_NAME = 'libzxtune.dll';
  {$ELSE}
  {$IFDEF DARWIN}
  DEFAULT_LIB_NAME = 'libzxtune.dylib';
  {$ELSE}
  DEFAULT_LIB_NAME = 'libzxtune.so';
  {$ENDIF}
  {$ENDIF}

type
  ZXTuneHandle = Pointer;
  PZXTuneModuleInfo = ^ZXTuneModuleInfo;
  ZXTuneModuleInfo = record
    Positions: Integer;
    LoopPosition: Integer;
    Frames: Integer;
    LoopFrame: Integer;
    Channels: Integer;
    InitialTempo: Integer;
  end;

var
  // Common functions
  ZXTune_GetVersion: function: PAnsiChar; cdecl;
  
  // Data operating
  ZXTune_CreateData: function(const data: Pointer; size: NativeUInt): ZXTuneHandle; cdecl;
  ZXTune_CloseData: procedure(data: ZXTuneHandle); cdecl;
  
  // Modules operating
  ZXTune_OpenModule: function(data: ZXTuneHandle): ZXTuneHandle; cdecl;
  ZXTune_CloseModule: procedure(module: ZXTuneHandle); cdecl;
  ZXTune_GetModuleInfo: function(module: ZXTuneHandle; var info: ZXTuneModuleInfo): Boolean; cdecl;
  
  // Players operating
  ZXTune_CreatePlayer: function(module: ZXTuneHandle): ZXTuneHandle; cdecl;
  ZXTune_DestroyPlayer: procedure(player: ZXTuneHandle); cdecl;

  ZXTune_RenderSound: function(player: ZXTuneHandle; buffer: Pointer; samples: NativeUInt): Integer; cdecl;

  ZXTune_SeekSound: function(player: ZXTuneHandle; sample: NativeUInt): Integer; cdecl;
  ZXTune_ResetSound: function(player: ZXTuneHandle): Boolean; cdecl;

  ZXTune_GetPlayerParameterInt: function(player: ZXTuneHandle; paramName: PAnsiChar; var paramValue: Integer): Boolean; cdecl;
  ZXTune_SetPlayerParameterInt: function(player: ZXTuneHandle; paramName: PAnsiChar; paramValue: Integer): Boolean; cdecl;

  ZXTune_GetCurrentPosition: function(player: ZXTuneHandle): LongInt; cdecl;
  ZXTune_GetDuration: function(player: ZXTuneHandle): LongInt; cdecl;

  ZXTune_GetDurationMs: function(player: ZXTuneHandle; const info: PZXTuneModuleInfo): LongInt; cdecl;
  ZXTune_GetPositionMs: function(player: ZXTuneHandle; const moduleInfo: PZXTuneModuleInfo): LongInt cdecl;

  ZXTune_SetPlayerLoopTrack: function(player: ZXTuneHandle; paramValue: Integer): Boolean; cdecl;
  ZXTune_GetPlayerLoopTrack: function(player: ZXTuneHandle): LongInt; cdecl;

  ZXTune_GetSoundFrequency: function(player: ZXTuneHandle): LongInt; cdecl;

  ZXTune_SetDoneSamples: function(player: ZXTuneHandle; const moduleInfo: PZXTuneModuleInfo): Boolean; cdecl;

  ZXTune_GetModuleAttribute: function(player: ZXTuneHandle; attrName: PAnsiChar; buffer: PAnsiChar;  bufferSize: NativeUInt): Boolean; cdecl;




procedure LoadZXTuneLibrary(const LibraryName: string = DEFAULT_LIB_NAME);
function ZXTuneLoaded: Boolean;

implementation
var
  library_handle: TLibHandle;

procedure LoadProc(var fn_var; const fn_name: string);
begin
  pointer(fn_var) := GetProcedureAddress(library_handle, fn_name);
end;


procedure LoadZXTuneLibrary(const LibraryName: string);
begin
  if library_handle <> NilHandle then
    Exit; // Уже загружена

  library_handle := LoadLibrary(LibraryName);
  if library_handle = NilHandle then
    raise Exception.CreateFmt('Could not load library "%s"', [LibraryName]);

  try
    // Common functions
    LoadProc(ZXTune_GetVersion, 'ZXTune_GetVersion');

    // Data operating
    LoadProc(ZXTune_CreateData, 'ZXTune_CreateData');
    LoadProc(ZXTune_CloseData, 'ZXTune_CloseData');
    
    // Modules operating
    LoadProc(ZXTune_OpenModule, 'ZXTune_OpenModule');
    LoadProc(ZXTune_CloseModule, 'ZXTune_CloseModule');
    LoadProc(ZXTune_GetModuleInfo, 'ZXTune_GetModuleInfo');
    
    // Players operating
    LoadProc(ZXTune_CreatePlayer, 'ZXTune_CreatePlayer');
    LoadProc(ZXTune_DestroyPlayer, 'ZXTune_DestroyPlayer');

    LoadProc(ZXTune_RenderSound, 'ZXTune_RenderSound');

    LoadProc(ZXTune_SeekSound, 'ZXTune_SeekSound');
    LoadProc(ZXTune_ResetSound, 'ZXTune_ResetSound');

    LoadProc(ZXTune_GetPlayerParameterInt, 'ZXTune_GetPlayerParameterInt');
    LoadProc(ZXTune_SetPlayerParameterInt, 'ZXTune_SetPlayerParameterInt');

    LoadProc(ZXTune_GetDuration, 'ZXTune_GetDuration');
    LoadProc(ZXTune_GetCurrentPosition, 'ZXTune_GetCurrentPosition');
    LoadProc(ZXTune_GetPositionMs, 'ZXTune_GetPositionMs');

    LoadProc(ZXTune_GetDurationMs, 'ZXTune_GetDurationMs');

    LoadProc(ZXTune_GetPlayerLoopTrack, 'ZXTune_GetPlayerLoopTrack');
    LoadProc(ZXTune_SetPlayerLoopTrack, 'ZXTune_SetPlayerLoopTrack');

    LoadProc(ZXTune_SetDoneSamples, 'ZXTune_SetDoneSamples');

    LoadProc(ZXTune_GetSoundFrequency, 'ZXTune_GetSoundFrequency');

    LoadProc(ZXTune_GetModuleAttribute, 'ZXTune_GetModuleAttribute');


  except
    UnloadLibrary(library_handle);
    library_handle := NilHandle;
    raise;
  end;
end;

function ZXTuneLoaded: Boolean;
begin
  Result := library_handle <> NilHandle;
end;

initialization

finalization
//  if library_handle <> NilHandle then
//    UnloadLibrary(library_handle);
end.

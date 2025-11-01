unit libvgmplay;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  SysUtils, dynlibs;



const
  {$ifdef linux}
      VGMLIB_NAME = 'libvgmplay.so';
  {$endif}
    {$ifdef windows}
      VGMLIB_NAME = 'libvgmplay.dll';
  {$endif}

  FCC_VGM = $206D6756; // 'Vgm '
  FCC_GD3 = $20336447; // 'Gd3 '

type
  VGM_TAG = record
    fccGD3: UINT32;
    lngVersion: UINT32;
    lngTagLength: UINT32;
    strTrackNameE: PChar;
    strTrackNameJ: PWideChar;
    strGameNameE: PWideChar;
    strGameNameJ: PWideChar;
    strSystemNameE: PChar;
    strSystemNameJ: PWideChar;
    strAuthorNameE: PWideChar;
    strAuthorNameJ: PWideChar;
    strReleaseDate: PWideChar;
    strCreator: PWideChar;
    strNotes: PWideChar;
  end;
  PVGM_TAG = ^VGM_TAG;

  VGM_HEADER = record
    fccVGM: Cardinal;
    lngEOFOffset: Cardinal;
    lngVersion: Cardinal;
    lngHzPSG: Cardinal;
    lngHzYM2413: Cardinal;
    lngGD3Offset: Cardinal;
    lngTotalSamples: Cardinal;
    lngLoopOffset: Cardinal;
    lngLoopSamples: Cardinal;
    lngRate: Cardinal;
    shtPSG_Feedback: Word;
    bytPSG_SRWidth: Byte;
    bytPSG_Flags: Byte;
    lngHzYM2612: Cardinal;
    lngHzYM2151: Cardinal;
    lngDataOffset: Cardinal;
    lngHzSPCM: Cardinal;
    lngSPCMIntf: Cardinal;
    lngHzRF5C68: Cardinal;
    lngHzYM2203: Cardinal;
    lngHzYM2608: Cardinal;
    lngHzYM2610: Cardinal;
    lngHzYM3812: Cardinal;
    lngHzYM3526: Cardinal;
    lngHzY8950: Cardinal;
    lngHzYMF262: Cardinal;
    lngHzYMF278B: Cardinal;
    lngHzYMF271: Cardinal;
    lngHzYMZ280B: Cardinal;
    lngHzRF5C164: Cardinal;
    lngHzPWM: Cardinal;
    lngHzAY8910: Cardinal;
    bytAYType: Byte;
    bytAYFlag: Byte;
    bytAYFlagYM2203: Byte;
    bytAYFlagYM2608: Byte;
    bytVolumeModifier: Byte;
    bytReserved2: Byte;
    bytLoopBase: ShortInt;
    bytLoopModifier: Byte;
    lngHzGBDMG: Cardinal;
    lngHzNESAPU: Cardinal;
    lngHzMultiPCM: Cardinal;
    lngHzUPD7759: Cardinal;
    lngHzOKIM6258: Cardinal;
    bytOKI6258Flags: Byte;
    bytK054539Flags: Byte;
    bytC140Type: Byte;
    bytReservedFlags: Byte;
    lngHzOKIM6295: Cardinal;
    lngHzK051649: Cardinal;
    lngHzK054539: Cardinal;
    lngHzHuC6280: Cardinal;
    lngHzC140: Cardinal;
    lngHzK053260: Cardinal;
    lngHzPokey: Cardinal;
    lngHzQSound: Cardinal;
    lngHzSCSP: Cardinal;
    lngExtraOffset: Cardinal;
    lngHzWSwan: Cardinal;
    lngHzVSU: Cardinal;
    lngHzSAA1099: Cardinal;
    lngHzES5503: Cardinal;
    lngHzES5506: Cardinal;
    bytES5503Chns: Byte;
    bytES5506Chns: Byte;
    bytC352ClkDiv: Byte;
    bytESReserved: Byte;
    lngHzX1_010: Cardinal;
    lngHzC352: Cardinal;
    lngHzGA20: Cardinal;
  end;
  PVGM_HEADER = ^VGM_HEADER;

  VGM_HDR_EXTRA = record
    DataSize: UINT32;
    Chp2ClkOffset: UINT32;
    ChpVolOffset: UINT32;
  end;

  VGMX_CHIP_DATA32 = record
    Type_: UINT8;
    Data: UINT32;
  end;

  VGMX_CHIP_DATA16 = record
    Type_: UINT8;
    Flags: UINT8;
    Data: UINT16;
  end;

  VGMX_CHP_EXTRA32 = record
    ChipCnt: UINT8;
    CCData: ^VGMX_CHIP_DATA32;
  end;

  VGMX_CHP_EXTRA16 = record
    ChipCnt: UINT8;
    CCData: ^VGMX_CHIP_DATA16;
  end;

  VGM_EXTRA = record
    Clocks: VGMX_CHP_EXTRA32;
    Volumes: VGMX_CHP_EXTRA16;
  end;

  VGM_PCM_DATA = record
    DataSize: UINT32;
    Data: PByte;
    DataStart: UINT32;
  end;

  VGM_PCM_BANK = record
    BankCount: UINT32;
    Bank: ^VGM_PCM_DATA;
    DataSize: UINT32;
    Data: PByte;
    DataPos: UINT32;
    BnkPos: UINT32;
  end;

var
  // Основные функции воспроизведения
  VGMPlay_Init: procedure; cdecl;
  VGMPlay_Init2: procedure; cdecl;
  StopVGM: procedure; cdecl;
  RestartVGM: procedure; cdecl;
  PauseVGM: procedure(Pause: Boolean); cdecl;
  PlayVGM: procedure; cdecl;
  VGMPlay_Deinit: procedure; cdecl;

  // Работа с файлами
  OpenVGMFile: function(fname: PChar): Boolean; cdecl;
  CloseVGMFile: procedure; cdecl;
  //OpenVGMFileW: function(const FileName: PWideChar): Boolean; cdecl;

  // Буферизация звука
  FillBuffer: function(buff: Pointer; buffsize: Integer): Integer; cdecl;

  // Поиск файлов
  FindFile: function(const FileName: PChar): PChar; cdecl;
  FindFile_List: function(const FileNameList: PPChar): PChar; cdecl;

  // Навигация по треку
  SeekVGM: procedure(Relative: Boolean; PlayBkSamples: Int32); cdecl;

  // Настройки воспроизведения
  RefreshMuting: procedure; cdecl;
  RefreshPanning: procedure; cdecl;
  RefreshPlaybackOptions: procedure; cdecl;

  // Работа с метаданными
  GetGZFileLength: function(const FileName: PChar): UInt32; cdecl;
//  GetGZFileLengthW: function(const FileName: PWideChar): UInt32; cdecl;
  FreeGD3Tag: procedure(TagData: Pointer); cdecl;
  ReadGD3Tag: function(FileName: PChar; GD3Offset: UINT32; RetGD3Tag: VGM_TAG): Integer; cdecl;

  // Вспомогательные функции

  CalcSampleMSec: function(Value: UInt64; Mode: UInt8): UInt32; cdecl;
  CalcSampleMSecExt: function(Value: UInt64; Mode: UInt8; FileHead: Pointer): UInt32; cdecl;
  GetChipName: function(ChipID: UInt8): PChar; cdecl;
  GetAccurateChipName: function(ChipID: UInt8; SubType: UInt8): PChar; cdecl;
  GetChipClock: function(FileHead: Pointer; ChipID: UInt8; RetSubType: PUInt8): UInt32; cdecl;

  // Получение информации о файле
  GetVGMFileInfo: function(const FileName: PChar; RetVGMHead: PVGM_HEADER; RetGD3Tag: VGM_TAG): UInt32; cdecl;

  // Конвертация сэмплов
  SampleVGM2Playback: function(SampleVal: Int32): Int32; cdecl;
  SamplePlayback2VGM: function(SampleVal: Int32): Int32; cdecl;

  GetVGMSamplesPlayed: function: UInt32; cdecl;
  GetVGMTotalSamples: function: UInt32; cdecl;
  GetVGMSampleRate: function: UInt32; cdecl;

  IsVGMPlayEnded: function: boolean; cdecl;
  GetVGMPos: function: UInt32; cdecl;
  GetVGMDataLen: function: Uint32; cdecl;
  GetMasterVolume: function: Single; cdecl;
  IsFadePlay: function: Boolean; cdecl;




procedure LoadVGMLibrary(const LibraryName: string = VGMLIB_NAME);
function VGMLoaded: Boolean;

implementation

var
  library_handle: TLibHandle = NilHandle;

procedure LoadProc(var fn_var; const fn_name: string);
begin
  pointer(fn_var) := GetProcedureAddress(library_handle, fn_name);
  if pointer(fn_var) = nil then
    raise Exception.CreateFmt('Procedure "%s" not found in VGMPlay library', [fn_name]);
end;

procedure LoadVGMLibrary(const LibraryName: string);
begin
  if library_handle <> NilHandle then
    Exit; // Уже загружена

  library_handle := LoadLibrary(LibraryName);
  if library_handle = NilHandle then
    raise Exception.CreateFmt('Could not load library "%s"', [LibraryName]);

  try
    // Основные функции воспроизведения
    LoadProc(VGMPlay_Init, 'VGMPlay_Init');
    LoadProc(VGMPlay_Init2, 'VGMPlay_Init2');
    LoadProc(StopVGM, 'StopVGM');
    LoadProc(RestartVGM, 'RestartVGM');
    LoadProc(PauseVGM, 'PauseVGM');
    LoadProc(PlayVGM, 'PlayVGM');
    LoadProc(VGMPlay_Deinit, 'VGMPlay_Deinit');

    // Работа с файлами
    LoadProc(OpenVGMFile, 'OpenVGMFile');
    LoadProc(CloseVGMFile, 'CloseVGMFile');
   // LoadProc(OpenVGMFileW, 'OpenVGMFileW');

    // Буферизация звука
    LoadProc(FillBuffer, 'FillBuffer');

    // Поиск файлов
    LoadProc(FindFile, 'FindFile');
    LoadProc(FindFile_List, 'FindFile_List');

    // Навигация по треку
    LoadProc(SeekVGM, 'SeekVGM');

    // Настройки воспроизведения
    LoadProc(RefreshMuting, 'RefreshMuting');
    LoadProc(RefreshPanning, 'RefreshPanning');
    LoadProc(RefreshPlaybackOptions, 'RefreshPlaybackOptions');

    // Работа с метаданными
    LoadProc(GetGZFileLength, 'GetGZFileLength');
 //   LoadProc(GetGZFileLengthW, 'GetGZFileLengthW');
    LoadProc(FreeGD3Tag, 'FreeGD3Tag');
    LoadProc(ReadGD3Tag, 'ReadGD3Tag');

    // Вспомогательные функции


    LoadProc(CalcSampleMSec, 'CalcSampleMSec');
    LoadProc(CalcSampleMSecExt, 'CalcSampleMSecExt');
    LoadProc(GetChipName, 'GetChipName');
    LoadProc(GetAccurateChipName, 'GetAccurateChipName');
    LoadProc(GetChipClock, 'GetChipClock');

    // Получение информации о файле
    LoadProc(GetVGMFileInfo, 'GetVGMFileInfo');
  //  LoadProc(GetVGMFileInfoW, 'GetVGMFileInfoW');
 //   LoadProc(ReadVGMHeader, 'ReadVGMHeader');

    // Конвертация сэмплов
    LoadProc(SampleVGM2Playback, 'SampleVGM2Playback');
    LoadProc(SamplePlayback2VGM, 'SamplePlayback2VGM');

  // LoadProc(GetCurrentPosition, 'GetCurrentPosition');


    LoadProc(GetVGMSamplesPlayed, 'GetVGMSamplesPlayed');
    LoadProc(GetVGMTotalSamples, 'GetVGMTotalSamples');
    LoadProc(GetVGMSampleRate, 'GetVGMSampleRate');

    LoadProc(IsVGMPlayEnded, 'IsVGMPlayEnded');
    LoadProc(GetVGMPos, 'GetVGMPos');
    LoadProc(GetVGMDataLen, 'GetVGMDataLen');
    LoadProc(GetMasterVolume, 'GetMasterVolume');
    LoadProc(IsFadePlay, 'IsFadePlay');
  except
    UnloadLibrary(library_handle);
    library_handle := NilHandle;
    raise;
  end;
end;

function VGMLoaded: Boolean;
begin
  Result := library_handle <> NilHandle;
end;

finalization
 // if library_handle <> NilHandle then
//    UnloadLibrary(library_handle);

end.

unit libwavpack;

{$mode objfpc}{$h+}
{$packrecords c}

interface

uses
  SysUtils, dynlibs;

const
  {$IFDEF MSWINDOWS}
  DEFAULT_LIB_NAME = 'wavpackdll.dll';
  {$ELSE}
  {$IFDEF DARWIN}
  DEFAULT_LIB_NAME = 'libwavpack.dylib';
  {$ELSE}
  DEFAULT_LIB_NAME = 'libwavpack.so';
  {$ENDIF}
  {$ENDIF}

type
  // Basic types
  uint8_t = Byte;
  int8_t = ShortInt;
  uint16_t = Word;
  int16_t = SmallInt;
  uint32_t = LongWord;
  Puint32_t = ^uint32_t;
  int32_t = LongInt;
  Pint32_t = ^int32_t;
  uint64_t = UInt64;
  int64_t = Int64;

  // WavPack context handle
  WavpackContext = Pointer;

  // WavPack header structure
  Pwavpackheader = ^wavpackheader;
  wavpackheader = record
    ckID: array[0..3] of AnsiChar;
    ckSize: uint32_t;
    version: int16_t;
    block_index_u8: uint8_t;
    total_samples_u8: uint8_t;
    total_samples: uint32_t;
    block_index: uint32_t;
    block_samples: uint32_t;
    flags: uint32_t;
    crc: uint32_t;
  end;

  // WavPack Configuration
  PWavpackConfig = ^WavpackConfig;
  WavpackConfig = record
    bitrate: Single;
    shaping_weight: Single;
    bits_per_sample: Integer;
    bytes_per_sample: Integer;
    qmode: Integer;
    flags: Integer;
    xmode: Integer;
    num_channels: Integer;
    float_norm_exp: Integer;
    block_samples: int32_t;
    worker_threads: int32_t;
    sample_rate: int32_t;
    channel_mask: int32_t;
    md5_checksum: array[0..15] of uint8_t;
    md5_read: Integer;
    num_tag_strings: Integer;
    tag_strings: PPAnsiChar;
  end;

  // Callback types
  TReadBytesFunc = function(id: Pointer; data: Pointer; bcount: int32_t): int32_t; cdecl;
  TGetPosFunc = function(id: Pointer): uint32_t; cdecl;
  TSetPosAbsFunc = function(id: Pointer; pos: uint32_t): Integer; cdecl;
  TSetPosRelFunc = function(id: Pointer; delta: int32_t; mode: Integer): Integer; cdecl;
  TPushBackByteFunc = function(id: Pointer; c: Integer): Integer; cdecl;
  TGetLengthFunc = function(id: Pointer): uint32_t; cdecl;
  TCanSeekFunc = function(id: Pointer): Integer; cdecl;
  TWriteBytesFunc = function(id: Pointer; data: Pointer; bcount: int32_t): int32_t; cdecl;

  // Standard Stream Reader
  PWavpackStreamReader = ^WavpackStreamReader;
  WavpackStreamReader = record
    read_bytes: TReadBytesFunc;
    get_pos: TGetPosFunc;
    set_pos_abs: TSetPosAbsFunc;
    set_pos_rel: TSetPosRelFunc;
    push_back_byte: TPushBackByteFunc;
    get_length: TGetLengthFunc;
    can_seek: TCanSeekFunc;
    write_bytes: TWriteBytesFunc;
  end;

  // Extended callback types for 64-bit
  TGetPos64Func = function(id: Pointer): int64_t; cdecl;
  TSetPosAbs64Func = function(id: Pointer; pos: int64_t): Integer; cdecl;
  TSetPosRel64Func = function(id: Pointer; delta: int64_t; mode: Integer): Integer; cdecl;
  TGetLength64Func = function(id: Pointer): int64_t; cdecl;
  TTruncateHereFunc = function(id: Pointer): Integer; cdecl;
  TCloseFunc = function(id: Pointer): Integer; cdecl;

  // Extended Stream Reader (64-bit)
  PWavpackStreamReader64 = ^WavpackStreamReader64;
  WavpackStreamReader64 = record
    read_bytes: TReadBytesFunc;
    write_bytes: TWriteBytesFunc;
    get_pos: TGetPos64Func;
    set_pos_abs: TSetPosAbs64Func;
    set_pos_rel: TSetPosRel64Func;
    push_back_byte: TPushBackByteFunc;
    get_length: TGetLength64Func;
    can_seek: TCanSeekFunc;
    truncate_here: TTruncateHereFunc;
    close: TCloseFunc;
  end;

  // Block output callback
  TWavpackBlockOutput = function(id: Pointer; data: Pointer; bcount: int32_t): Integer; cdecl;

// Constants
const
  // Flags for WavpackHeader
  BYTES_STORED     = 3;
  MONO_FLAG        = 4;
  HYBRID_FLAG      = 8;
  JOINT_STEREO     = $10;
  CROSS_DECORR     = $20;
  HYBRID_SHAPE     = $40;
  FLOAT_DATA       = $80;
  INT32_DATA       = $100;
  HYBRID_BITRATE   = $200;
  HYBRID_BALANCE   = $400;
  INITIAL_BLOCK    = $800;
  FINAL_BLOCK      = $1000;
  FALSE_STEREO     = $40000000;
  NEW_SHAPING      = $20000000;
  HAS_CHECKSUM     = $10000000;
  DSD_FLAG         = $80000000;

  // Open flags
  OPEN_WVC        = $1;
  OPEN_TAGS       = $2;
  OPEN_WRAPPER    = $4;
  OPEN_2CH_MAX    = $8;
  OPEN_NORMALIZE  = $10;
  OPEN_STREAMING  = $20;
  OPEN_EDIT_TAGS  = $40;
  OPEN_FILE_UTF8  = $80;
  OPEN_DSD_NATIVE = $100;
  OPEN_DSD_AS_PCM = $200;
  OPEN_ALT_TYPES  = $400;
  OPEN_NO_CHECKSUM = $800;

  // Mode flags
  MODE_WVC        = $1;
  MODE_LOSSLESS   = $2;
  MODE_HYBRID     = $4;
  MODE_FLOAT      = $8;
  MODE_VALID_TAG  = $10;
  MODE_HIGH       = $20;
  MODE_FAST       = $40;
  MODE_EXTRA      = $80;
  MODE_APETAG     = $100;
  MODE_SFX        = $200;
  MODE_VERY_HIGH  = $400;
  MODE_MD5        = $800;
  MODE_DNS        = $8000;

  // Configuration flags
  CONFIG_HYBRID_FLAG      = 8;
  CONFIG_JOINT_STEREO     = $10;
  CONFIG_CROSS_DECORR     = $20;
  CONFIG_HYBRID_SHAPE     = $40;
  CONFIG_FAST_FLAG        = $200;
  CONFIG_HIGH_FLAG        = $800;
  CONFIG_VERY_HIGH_FLAG   = $1000;
  CONFIG_BITRATE_KBPS     = $2000;
  CONFIG_SHAPE_OVERRIDE   = $8000;
  CONFIG_JOINT_OVERRIDE   = $10000;
  CONFIG_DYNAMIC_SHAPING  = $20000;
  CONFIG_CREATE_EXE       = $40000;
  CONFIG_CREATE_WVC       = $80000;
  CONFIG_OPTIMIZE_WVC     = $100000;
  CONFIG_COMPATIBLE_WRITE = $400000;
  CONFIG_CALC_NOISE       = $800000;
  CONFIG_EXTRA_MODE       = $2000000;
  CONFIG_SKIP_WVX         = $4000000;
  CONFIG_MD5_CHECKSUM     = $8000000;

  // QMode flags
  QMODE_BIG_ENDIAN        = $1;
  QMODE_SIGNED_BYTES      = $2;
  QMODE_UNSIGNED_WORDS    = $4;
  QMODE_REORDERED_CHANS   = $8;
  QMODE_DSD_LSB_FIRST     = $10;
  QMODE_DSD_MSB_FIRST     = $20;
  QMODE_DSD_IN_BLOCKS     = $40;

  // File formats
  WP_FORMAT_WAV = 0;
  WP_FORMAT_W64 = 1;
  WP_FORMAT_CAF = 2;
  WP_FORMAT_DFF = 3;
  WP_FORMAT_DSF = 4;
  WP_FORMAT_AIF = 5;

  WAVPACK_MAX_CHANS = 4096;

var
  // Raw decoder functions
  WavpackOpenRawDecoder: function(main_data: Pointer; main_size: int32_t;
    corr_data: Pointer; corr_size: int32_t; version: int16_t; error: PAnsiChar;
    flags: Integer; norm_offset: Integer): WavpackContext; cdecl;

  // File input functions
  WavpackOpenFileInputEx64: function(reader: PWavpackStreamReader64; wv_id: Pointer;
    wvc_id: Pointer; error: PAnsiChar; flags: Integer; norm_offset: Integer): WavpackContext; cdecl;

  WavpackOpenFileInputEx: function(reader: PWavpackStreamReader; wv_id: Pointer;
    wvc_id: Pointer; error: PAnsiChar; flags: Integer; norm_offset: Integer): WavpackContext; cdecl;

  WavpackOpenFileInput: function(infilename: PAnsiChar; error: PAnsiChar;
    flags: Integer; norm_offset: Integer): WavpackContext; cdecl;

  // Information functions
  WavpackGetMode: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetQualifyMode: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetErrorMessage: function(wpc: WavpackContext): PAnsiChar; cdecl;
  WavpackGetVersion: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetFileExtension: function(wpc: WavpackContext): PAnsiChar; cdecl;
  WavpackGetFileFormat: function(wpc: WavpackContext): uint8_t; cdecl;

  // Sample functions
  WavpackUnpackSamples: function(wpc: WavpackContext; buffer: Pint32_t;
    samples: uint32_t): uint32_t; cdecl;

  WavpackGetNumSamples: function(wpc: WavpackContext): uint32_t; cdecl;
  WavpackGetNumSamples64: function(wpc: WavpackContext): int64_t; cdecl;
  WavpackGetNumSamplesInFrame: function(wpc: WavpackContext): uint32_t; cdecl;
  WavpackGetSampleIndex: function(wpc: WavpackContext): uint32_t; cdecl;
  WavpackGetSampleIndex64: function(wpc: WavpackContext): int64_t; cdecl;

  // Error and status functions
  WavpackGetNumErrors: function(wpc: WavpackContext): Integer; cdecl;
  WavpackLossyBlocks: function(wpc: WavpackContext): Integer; cdecl;

  // Seek functions
  WavpackSeekSample: function(wpc: WavpackContext; sample: uint32_t): Integer; cdecl;
  WavpackSeekSample64: function(wpc: WavpackContext; sample: int64_t): Integer; cdecl;

  // Audio properties
  WavpackGetSampleRate: function(wpc: WavpackContext): uint32_t; cdecl;
  WavpackGetNativeSampleRate: function(wpc: WavpackContext): uint32_t; cdecl;
  WavpackGetBitsPerSample: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetBytesPerSample: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetNumChannels: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetChannelMask: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetReducedChannels: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetFloatNormExp: function(wpc: WavpackContext): Integer; cdecl;

  // MD5 functions
  WavpackGetMD5Sum: function(wpc: WavpackContext; data: PByte): Integer; cdecl;
  WavpackGetChannelIdentities: procedure(wpc: WavpackContext; identities: PByte); cdecl;

  // Wrapper functions
  WavpackGetWrapperBytes: function(wpc: WavpackContext): uint32_t; cdecl;
  WavpackGetWrapperData: function(wpc: WavpackContext): PByte; cdecl;
  WavpackFreeWrapper: procedure(wpc: WavpackContext); cdecl;
  WavpackSeekTrailingWrapper: procedure(wpc: WavpackContext); cdecl;

  // File information
  WavpackGetFileSize: function(wpc: WavpackContext): uint32_t; cdecl;
  WavpackGetFileSize64: function(wpc: WavpackContext): int64_t; cdecl;
  WavpackGetRatio: function(wpc: WavpackContext): Double; cdecl;
  WavpackGetAverageBitrate: function(wpc: WavpackContext; count_wvc: Integer): Double; cdecl;
  WavpackGetInstantBitrate: function(wpc: WavpackContext): Double; cdecl;
  WavpackGetProgress: function(wpc: WavpackContext): Double; cdecl;

  // Tag functions
  WavpackGetNumTagItems: function(wpc: WavpackContext): Integer; cdecl;
  WavpackGetTagItem: function(wpc: WavpackContext; item: PAnsiChar; value: PAnsiChar; size: Integer): Integer; cdecl;
  WavpackGetTagItemIndexed: function(wpc: WavpackContext; index: Integer; item: PAnsiChar; size: Integer): Integer; cdecl;
  WavpackAppendTagItem: function(wpc: WavpackContext; item: PAnsiChar; value: PAnsiChar; vsize: Integer): Integer; cdecl;
  WavpackDeleteTagItem: function(wpc: WavpackContext; item: PAnsiChar): Integer; cdecl;
  WavpackWriteTag: function(wpc: WavpackContext): Integer; cdecl;

  // Encoding functions
  WavpackOpenFileOutput: function(blockout: TWavpackBlockOutput; wv_id: Pointer;
    wvc_id: Pointer): WavpackContext; cdecl;

  WavpackSetFileInformation: procedure(wpc: WavpackContext; file_extension: PAnsiChar;
    file_format: uint8_t); cdecl;

  WavpackSetConfiguration: function(wpc: WavpackContext; config: PWavpackConfig;
    total_samples: uint32_t): Integer; cdecl;

  WavpackSetConfiguration64: function(wpc: WavpackContext; config: PWavpackConfig;
    total_samples: int64_t; chan_ids: PByte): Integer; cdecl;

  WavpackSetChannelLayout: function(wpc: WavpackContext; layout_tag: uint32_t;
    reorder: PByte): Integer; cdecl;

  WavpackAddWrapper: function(wpc: WavpackContext; data: Pointer; bcount: uint32_t): Integer; cdecl;
  WavpackStoreMD5Sum: function(wpc: WavpackContext; data: PByte): Integer; cdecl;
  WavpackPackInit: function(wpc: WavpackContext): Integer; cdecl;
  WavpackPackSamples: function(wpc: WavpackContext; sample_buffer: Pint32_t;
    sample_count: uint32_t): Integer; cdecl;
  WavpackFlushSamples: function(wpc: WavpackContext): Integer; cdecl;

  // Utility functions
  WavpackUpdateNumSamples: procedure(wpc: WavpackContext; first_block: Pointer); cdecl;
  WavpackGetWrapperLocation: function(first_block: Pointer; size: Puint32_t): Pointer; cdecl;
  WavpackGetEncodedNoise: function(wpc: WavpackContext; peak: PDouble): Double; cdecl;
  WavpackFloatNormalize: procedure(values: Pint32_t; num_values: int32_t; delta_exp: int32_t); cdecl;

  // Endian conversion functions
  WavpackLittleEndianToNative: procedure(data: Pointer; format: PAnsiChar); cdecl;
  WavpackNativeToLittleEndian: procedure(data: Pointer; format: PAnsiChar); cdecl;
  WavpackBigEndianToNative: procedure(data: Pointer; format: PAnsiChar); cdecl;
  WavpackNativeToBigEndian: procedure(data: Pointer; format: PAnsiChar); cdecl;

  // Library version
  WavpackGetLibraryVersion: function: uint32_t; cdecl;
  WavpackGetLibraryVersionString: function: PAnsiChar; cdecl;

  // Close function
  WavpackCloseFile: function(wpc: WavpackContext): WavpackContext; cdecl;

procedure LoadWavPackLibrary(const LibraryName: string = DEFAULT_LIB_NAME);
function WavPackLoaded: Boolean;

// Helper functions
function GET_BLOCK_INDEX(var hdr: wavpackheader): int64_t;
function GET_TOTAL_SAMPLES(var hdr: wavpackheader): int64_t;

implementation

var
  library_handle: TLibHandle = NilHandle;

procedure LoadProc(var fn_var; const fn_name: string);
begin
  pointer(fn_var) := GetProcedureAddress(library_handle, fn_name);
end;

procedure LoadWavPackLibrary(const LibraryName: string);
begin
  if library_handle <> NilHandle then
    Exit; // Уже загружена

  library_handle := LoadLibrary(LibraryName);
  if library_handle = NilHandle then
    raise Exception.CreateFmt('Could not load library "%s"', [LibraryName]);

  try
    // Raw decoder functions
    LoadProc(WavpackOpenRawDecoder, 'WavpackOpenRawDecoder');

    // File input functions
    LoadProc(WavpackOpenFileInputEx64, 'WavpackOpenFileInputEx64');
    LoadProc(WavpackOpenFileInputEx, 'WavpackOpenFileInputEx');
    LoadProc(WavpackOpenFileInput, 'WavpackOpenFileInput');

    // Information functions
    LoadProc(WavpackGetMode, 'WavpackGetMode');
    LoadProc(WavpackGetQualifyMode, 'WavpackGetQualifyMode');
    LoadProc(WavpackGetErrorMessage, 'WavpackGetErrorMessage');
    LoadProc(WavpackGetVersion, 'WavpackGetVersion');
    LoadProc(WavpackGetFileExtension, 'WavpackGetFileExtension');
    LoadProc(WavpackGetFileFormat, 'WavpackGetFileFormat');

    // Sample functions
    LoadProc(WavpackUnpackSamples, 'WavpackUnpackSamples');
    LoadProc(WavpackGetNumSamples, 'WavpackGetNumSamples');
    LoadProc(WavpackGetNumSamples64, 'WavpackGetNumSamples64');
    LoadProc(WavpackGetNumSamplesInFrame, 'WavpackGetNumSamplesInFrame');
    LoadProc(WavpackGetSampleIndex, 'WavpackGetSampleIndex');
    LoadProc(WavpackGetSampleIndex64, 'WavpackGetSampleIndex64');

    // Error and status functions
    LoadProc(WavpackGetNumErrors, 'WavpackGetNumErrors');
    LoadProc(WavpackLossyBlocks, 'WavpackLossyBlocks');

    // Seek functions
    LoadProc(WavpackSeekSample, 'WavpackSeekSample');
    LoadProc(WavpackSeekSample64, 'WavpackSeekSample64');

    // Audio properties
    LoadProc(WavpackGetSampleRate, 'WavpackGetSampleRate');
    LoadProc(WavpackGetNativeSampleRate, 'WavpackGetNativeSampleRate');
    LoadProc(WavpackGetBitsPerSample, 'WavpackGetBitsPerSample');
    LoadProc(WavpackGetBytesPerSample, 'WavpackGetBytesPerSample');
    LoadProc(WavpackGetNumChannels, 'WavpackGetNumChannels');
    LoadProc(WavpackGetChannelMask, 'WavpackGetChannelMask');
    LoadProc(WavpackGetReducedChannels, 'WavpackGetReducedChannels');
    LoadProc(WavpackGetFloatNormExp, 'WavpackGetFloatNormExp');

    // MD5 functions
    LoadProc(WavpackGetMD5Sum, 'WavpackGetMD5Sum');
    LoadProc(WavpackGetChannelIdentities, 'WavpackGetChannelIdentities');

    // Wrapper functions
    LoadProc(WavpackGetWrapperBytes, 'WavpackGetWrapperBytes');
    LoadProc(WavpackGetWrapperData, 'WavpackGetWrapperData');
    LoadProc(WavpackFreeWrapper, 'WavpackFreeWrapper');
    LoadProc(WavpackSeekTrailingWrapper, 'WavpackSeekTrailingWrapper');

    // File information
    LoadProc(WavpackGetFileSize, 'WavpackGetFileSize');
    LoadProc(WavpackGetFileSize64, 'WavpackGetFileSize64');
    LoadProc(WavpackGetRatio, 'WavpackGetRatio');
    LoadProc(WavpackGetAverageBitrate, 'WavpackGetAverageBitrate');
    LoadProc(WavpackGetInstantBitrate, 'WavpackGetInstantBitrate');
    LoadProc(WavpackGetProgress, 'WavpackGetProgress');

    // Tag functions
    LoadProc(WavpackGetNumTagItems, 'WavpackGetNumTagItems');
    LoadProc(WavpackGetTagItem, 'WavpackGetTagItem');
    LoadProc(WavpackGetTagItemIndexed, 'WavpackGetTagItemIndexed');
    LoadProc(WavpackAppendTagItem, 'WavpackAppendTagItem');
    LoadProc(WavpackDeleteTagItem, 'WavpackDeleteTagItem');
    LoadProc(WavpackWriteTag, 'WavpackWriteTag');

    // Encoding functions
    LoadProc(WavpackOpenFileOutput, 'WavpackOpenFileOutput');
    LoadProc(WavpackSetFileInformation, 'WavpackSetFileInformation');
    LoadProc(WavpackSetConfiguration, 'WavpackSetConfiguration');
    LoadProc(WavpackSetConfiguration64, 'WavpackSetConfiguration64');
    LoadProc(WavpackSetChannelLayout, 'WavpackSetChannelLayout');
    LoadProc(WavpackAddWrapper, 'WavpackAddWrapper');
    LoadProc(WavpackStoreMD5Sum, 'WavpackStoreMD5Sum');
    LoadProc(WavpackPackInit, 'WavpackPackInit');
    LoadProc(WavpackPackSamples, 'WavpackPackSamples');
    LoadProc(WavpackFlushSamples, 'WavpackFlushSamples');

    // Utility functions
    LoadProc(WavpackUpdateNumSamples, 'WavpackUpdateNumSamples');
    LoadProc(WavpackGetWrapperLocation, 'WavpackGetWrapperLocation');
    LoadProc(WavpackGetEncodedNoise, 'WavpackGetEncodedNoise');
    LoadProc(WavpackFloatNormalize, 'WavpackFloatNormalize');

    // Endian conversion functions
    LoadProc(WavpackLittleEndianToNative, 'WavpackLittleEndianToNative');
    LoadProc(WavpackNativeToLittleEndian, 'WavpackNativeToLittleEndian');
    LoadProc(WavpackBigEndianToNative, 'WavpackBigEndianToNative');
    LoadProc(WavpackNativeToBigEndian, 'WavpackNativeToBigEndian');

    // Library version
    LoadProc(WavpackGetLibraryVersion, 'WavpackGetLibraryVersion');
    LoadProc(WavpackGetLibraryVersionString, 'WavpackGetLibraryVersionString');

    // Close function
    LoadProc(WavpackCloseFile, 'WavpackCloseFile');

  except
    UnloadLibrary(library_handle);
    library_handle := NilHandle;
    raise;
  end;
end;

function WavPackLoaded: Boolean;
begin
  Result := library_handle <> NilHandle;
end;

function GET_BLOCK_INDEX(var hdr: wavpackheader): int64_t;
begin
  Result := int64_t(hdr.block_index) + (int64_t(hdr.block_index_u8) shl 32);
end;

function GET_TOTAL_SAMPLES(var hdr: wavpackheader): int64_t;
begin
  if hdr.total_samples = uint32_t(-1) then
    Result := -1
  else
    Result := int64_t(hdr.total_samples) + (int64_t(hdr.total_samples_u8) shl 32) - hdr.total_samples_u8;
end;

initialization

finalization
  if library_handle <> NilHandle then
    UnloadLibrary(library_handle);
end.

unit libxmp;

{$mode objfpc}{$H+}
{$PACKRECORDS C}

interface

uses
  {%H-}CTypes;

const
  XMP_VERSION     = '4.6.3';
  XMP_VERCODE     = $040603;

  XMP_VER_MAJOR   = 4;
  XMP_VER_MINOR   = 6;
  XMP_VER_RELEASE = 3;

const
 {$IFDEF windows}
  XMP_LIB_NAME = 'libxmp.dll';
 {$ELSE}
  XMP_LIB_NAME    = 'libxmp.so';
 {$ENDIF}

const
  XMP_NAME_SIZE     = 64; // Taille du nom du module et type

  // Note event constants
  XMP_KEY_OFF       = $81;
  XMP_KEY_CUT       = $82;
  XMP_KEY_FADE      = $83;

  // Sample format flags
  XMP_FORMAT_8BIT   = 1 shl 0;
  XMP_FORMAT_UNSIGNED = 1 shl 1;
  XMP_FORMAT_MONO   = 1 shl 2;

  // Player parameters
  XMP_PLAYER_AMP    = 0;
  XMP_PLAYER_MIX    = 1;
  XMP_PLAYER_INTERP = 2;
  XMP_PLAYER_DSP    = 3;
  XMP_PLAYER_FLAGS  = 4;
  XMP_PLAYER_CFLAGS = 5;
  XMP_PLAYER_SMPCTL = 6;
  XMP_PLAYER_VOLUME = 7;
  XMP_PLAYER_STATE  = 8;
  XMP_PLAYER_SMIX_VOLUME = 9;
  XMP_PLAYER_DEFPAN = 10;
  XMP_PLAYER_MODE   = 11;
  XMP_PLAYER_MIXER_TYPE = 12;
  XMP_PLAYER_VOICES = 13;

  // Interpolation types
  XMP_INTERP_NEAREST = 0;
  XMP_INTERP_LINEAR = 1;
  XMP_INTERP_SPLINE = 2;

  // DSP effect types
  XMP_DSP_LOWPASS   = 1 shl 0;
  XMP_DSP_ALL       = XMP_DSP_LOWPASS;

  // Player state
  XMP_STATE_UNLOADED = 0;
  XMP_STATE_LOADED  = 1;
  XMP_STATE_PLAYING = 2;

  // Player flags
  XMP_FLAGS_VBLANK  = 1 shl 0;
  XMP_FLAGS_FX9BUG  = 1 shl 1;
  XMP_FLAGS_FIXLOOP = 1 shl 2;
  XMP_FLAGS_A500    = 1 shl 3;

  // Player modes
  XMP_MODE_AUTO     = 0;
  XMP_MODE_MOD      = 1;
  XMP_MODE_NOISETRACKER = 2;
  XMP_MODE_PROTRACKER = 3;
  XMP_MODE_S3M      = 4;
  XMP_MODE_ST3      = 5;
  XMP_MODE_ST3GUS   = 6;
  XMP_MODE_XM       = 7;
  XMP_MODE_FT2      = 8;
  XMP_MODE_IT       = 9;
  XMP_MODE_ITSMP    = 10;

  // Mixer types
  XMP_MIXER_STANDARD = 0;
  XMP_MIXER_A500    = 1;
  XMP_MIXER_A500F   = 2;

  // Sample flags
  XMP_SMPCTL_SKIP   = 1 shl 0;

  // Channel flags
  XMP_CHANNEL_SYNTH     = 1 shl 0;
  XMP_CHANNEL_MUTE_      = 1 shl 1;
  XMP_CHANNEL_SPLIT     = 1 shl 2;
  XMP_CHANNEL_SURROUND  = 1 shl 4;

  // Envelope flags
  XMP_ENVELOPE_ON       = 1 shl 0;
  XMP_ENVELOPE_SUS      = 1 shl 1;
  XMP_ENVELOPE_LOOP     = 1 shl 2;
  XMP_ENVELOPE_FLT      = 1 shl 3;
  XMP_ENVELOPE_SLOOP    = 1 shl 4;
  XMP_ENVELOPE_CARRY    = 1 shl 5;

  // Sample flags
  XMP_SAMPLE_16BIT      = 1 shl 0;
  XMP_SAMPLE_LOOP       = 1 shl 1;
  XMP_SAMPLE_LOOP_BIDIR = 1 shl 2;
  XMP_SAMPLE_LOOP_REVERSE = 1 shl 3;
  XMP_SAMPLE_LOOP_FULL  = 1 shl 4;
  XMP_SAMPLE_SLOOP      = 1 shl 5;
  XMP_SAMPLE_SLOOP_BIDIR = 1 shl 6;
  XMP_SAMPLE_STEREO     = 1 shl 7;
  XMP_SAMPLE_SYNTH      = 1 shl 15;

  // Instrument constants
  XMP_INST_NNA_CUT      = $00;
  XMP_INST_NNA_CONT     = $01;
  XMP_INST_NNA_OFF      = $02;
  XMP_INST_NNA_FADE     = $03;
  XMP_INST_DCT_OFF      = $00;
  XMP_INST_DCT_NOTE     = $01;
  XMP_INST_DCT_SMP      = $02;
  XMP_INST_DCT_INST     = $03;
  XMP_INST_DCA_CUT      = XMP_INST_NNA_CUT;
  XMP_INST_DCA_OFF      = XMP_INST_NNA_OFF;
  XMP_INST_DCA_FADE     = XMP_INST_NNA_FADE;

  // Limits
  XMP_MAX_KEYS      = 121;
  XMP_MAX_ENV_POINTS = 32;
  XMP_MAX_MOD_LENGTH = 256;
  XMP_MAX_CHANNELS  = 64;
  XMP_MAX_SRATE     = 49170;
  XMP_MIN_SRATE     = 4000;
  XMP_MIN_BPM       = 20;
  XMP_MAX_FRAMESIZE = 5 * XMP_MAX_SRATE * 2 div XMP_MIN_BPM;

  // Error codes
  XMP_END           = 1;
  XMP_ERROR_INTERNAL = 2;
  XMP_ERROR_FORMAT  = 3;
  XMP_ERROR_LOAD    = 4;
  XMP_ERROR_DEPACK  = 5;
  XMP_ERROR_SYSTEM  = 6;
  XMP_ERROR_INVALID = 7;
  XMP_ERROR_STATE   = 8;

type
  xmp_context = PChar;

type
  xmp_channel = record
    pan: integer;   // Pan de canal (0x80 pour centre)
    vol: integer;   // Volume du canal
    flg: integer;   // Flags du canal
  end;

  xmp_pattern = record
    rows: integer;
    index: array[1..1] of integer;
  end;

  xmp_event = record
    note: byte;
    ins: byte;
    vol: byte;
    fxt: byte;
    fxp: byte;
    f2t: byte;
    f2p: byte;
    _flag: byte;
  end;

  xmp_track = record
    rows: integer;
    event: array[1..1] of xmp_event;
  end;

  xmp_envelope = record
    flg: integer;
    npt: integer;
    scl: integer;
    sus: integer;
    sue: integer;
    lps: integer;
    lpe: integer;
    Data: array[1..XMP_MAX_ENV_POINTS * 2] of smallint;
  end;

  xmp_subinstrument = record
    vol: integer;
    gvl: integer;
    pan: integer;
    xpo: integer;
    fin: integer;
    vwf: integer;
    vde: integer;
    vra: integer;
    vsw: integer;
    rvv: integer;
    sid: integer;
    nna: integer;
    dct: integer;
    dca: integer;
    ifc: integer;
    ifr: integer;
  end;

  xmp_instrument = record
    Name: array[0..31] of char;
    vol: integer;
    nsm: integer;
    rls: integer;
    aei: xmp_envelope;
    pei: xmp_envelope;
    fei: xmp_envelope;
    map: array[0..XMP_MAX_KEYS] of record
      ins: byte;
      xpo: shortint;
    end;
    sub: ^xmp_subinstrument;
    extra: Pointer;
  end;

  xmp_sample = record
    Name: array[0..31] of char;
    len: integer;
    lps: integer;
    lpe: integer;
    flg: integer;
    Data: PByte;
  end;

  xmp_sequence = record
    entry_point: integer;
    duration: integer;
  end;

  xmp_module = record
    Name: array[0..XMP_NAME_SIZE - 1] of char;
    typ: array[0..XMP_NAME_SIZE - 1] of char;
    pat: integer;
    trk: integer;
    chn: integer;
    ins: integer;
    smp: integer;
    spd: integer;
    bpm: integer;
    len: integer;
    rst: integer;
    gvl: integer;
    xxp: ^xmp_pattern;
    xxt: ^xmp_track;
    xxi: ^xmp_instrument;
    xxs: ^xmp_sample;
    xxc: array[0..XMP_MAX_CHANNELS - 1] of xmp_channel;
    xxo: array[0..XMP_MAX_MOD_LENGTH] of byte;
  end;

  xmp_test_info = record
    Name: array[0..XMP_NAME_SIZE - 1] of char;
    type_: array[0..XMP_NAME_SIZE - 1] of char;
  end;

  xmp_module_info = record
    md5: array[0..15] of byte;
    vol_base: integer;
    module: ^xmp_module;
    comment: PChar;
    num_sequences: integer;
    seq_data: ^xmp_sequence;
  end;

  xmp_channel_info = record
    period: longword;
    position: longword;
    pitchbend: smallint;
    note: byte;
    instrument: byte;
    sample: byte;
    volume: byte;
    pan: byte;
    reserved: byte;
    event: xmp_event;
  end;

  xmp_frame_info = record
    pos: integer;
    pattern: integer;
    row: integer;
    num_rows: integer;
    frame: integer;
    speed: integer;
    bpm: integer;
    time: integer;
    total_time: integer;
    frame_time: integer;
    buffer: Pointer;
    buffer_size: integer;
    total_size: integer;
    volume: integer;
    loop_count: integer;
    virt_channels: integer;
    virt_used: integer;
    sequence: integer;
    channel_info: array[0..XMP_MAX_CHANNELS - 1] of xmp_channel_info;
  end;

  xmp_callbacks = record
    read_func: function(dest: Pointer; len, nmemb: longword; priv: Pointer): longword; cdecl;
    seek_func: function(priv: Pointer; offset: longint; whence: integer): integer; cdecl;
    tell_func: function(priv: Pointer): longint; cdecl;
    close_func: function(priv: Pointer): integer; cdecl;
  end;

  { Dynamic load : Vars that will hold our dynamically loaded functions...

   *************************** functions ******************************* }

var
xmp_syserrno: function: integer; cdecl;
xmp_create_context: function: xmp_context; cdecl;
xmp_free_context: procedure(ctx: xmp_context); cdecl;
xmp_load_module: function(ctx: xmp_context; const filename: PChar): Integer; cdecl;
xmp_load_module_from_memory: function(ctx: xmp_context; const Data: Pointer; size: longint): integer; cdecl;
xmp_load_module_from_file: function(ctx: xmp_context; file_: Pointer; size: longint): integer; cdecl;
xmp_load_module_from_callbacks: function(ctx: xmp_context; file_: Pointer; callbacks: xmp_callbacks): integer; cdecl;
xmp_test_module: function(const filename: PChar; var info: xmp_test_info): Integer; cdecl;
xmp_test_module_from_memory: function (const data: Pointer; size: LongInt; var info: xmp_test_info): Integer; cdecl;
xmp_test_module_from_file: function(file_: Pointer; var info: xmp_test_info): Integer; cdecl;
xmp_test_module_from_callbacks: function(file_: Pointer; callbacks: xmp_callbacks; var info: xmp_test_info): Integer; cdecl;
xmp_scan_module: procedure(ctx: xmp_context); cdecl;
xmp_release_module: procedure(ctx: xmp_context); cdecl;
xmp_start_player: function(ctx: xmp_context; rate: Integer; flags: Integer): Integer; cdecl;
xmp_play_frame: function(ctx: xmp_context): Integer; cdecl;
xmp_play_buffer: function(ctx: xmp_context; buffer: Pointer; size: Integer; loop: Integer): Integer; cdecl;
xmp_get_frame_info: procedure(ctx: xmp_context; var info: xmp_frame_info); cdecl;
xmp_end_player: procedure(ctx: xmp_context); cdecl;
xmp_inject_event: procedure(ctx: xmp_context; channel: Integer; var event: xmp_event); cdecl;
xmp_get_module_info: procedure(ctx: xmp_context; var info: xmp_module_info); cdecl;
xmp_get_format_list: function(): PPAnsiChar; cdecl;
xmp_next_position: function(ctx: xmp_context): Integer; cdecl;
xmp_prev_position: function(ctx: xmp_context): Integer; cdecl;
xmp_set_position: function(ctx: xmp_context; pos: Integer): Integer; cdecl;
xmp_set_row: function(ctx: xmp_context; row: Integer): Integer; cdecl;
xmp_set_tempo_factor: function(ctx: xmp_context; factor: Double): Integer; cdecl;
xmp_stop_module: procedure(ctx: xmp_context); cdecl;
xmp_restart_module: procedure(ctx: xmp_context); cdecl;
xmp_seek_time: function(ctx: xmp_context; time: Integer): Integer; cdecl;
xmp_channel_mute: function(ctx: xmp_context; channel: Integer; mute: Integer): Integer; cdecl;
xmp_channel_vol: function(ctx: xmp_context; channel: Integer; volume: Integer): Integer; cdecl;
xmp_set_player: function(ctx: xmp_context; param: Integer; value: Integer): Integer; cdecl;
xmp_get_player: function(ctx: xmp_context; param: Integer): Integer; cdecl;
xmp_set_instrument_path: function(ctx: xmp_context; const path: PChar): Integer; cdecl;

// External sample mixer API
xmp_start_smix: function(ctx: xmp_context; rate: Integer; format: Integer): Integer; cdecl;
xmp_end_smix: procedure(ctx: xmp_context); cdecl;
xmp_smix_play_instrument: function(ctx: xmp_context; chn: Integer; ins: Integer; note: Integer; vol: Integer): Integer; cdecl;
xmp_smix_play_sample: function(ctx: xmp_context; chn: Integer; smp: Integer; note: Integer; vol: Integer): Integer; cdecl;
xmp_smix_channel_pan: function(ctx: xmp_context; chn: Integer; pan: Integer): Integer; cdecl;
xmp_smix_load_sample: function(ctx: xmp_context; num: Integer; const filename: PChar): Integer; cdecl;
xmp_smix_release_sample: function(ctx: xmp_context; num: Integer): Integer; cdecl;

{Special function for dynamic loading of lib ...}

procedure LoadXmpLib(const aLibName: string);
function XmpLoaded: Boolean;
implementation

uses
  sysutils, dynlibs;

var
  library_handle: TLibHandle;



procedure LoadLibFn(var fn_var; const fn_name: string);
begin
  pointer(fn_var) := GetProcedureAddress(library_handle, fn_name);
end;

procedure LoadXmpLib(const aLibName: string);
begin
  library_handle := LoadLibrary(aLibName);

  if library_handle = NilHandle then
  begin
    writeln(GetLoadErrorStr);
    runError(2);
  end;

{$push}
{$warn 5043 off} // suppress deprecated warnings
LoadLibFn(xmp_syserrno, 'xmp_syserrno');
LoadLibFn(xmp_create_context, 'xmp_create_context');
LoadLibFn(xmp_free_context, 'xmp_free_context');
LoadLibFn(xmp_load_module, 'xmp_load_module');
LoadLibFn(xmp_load_module_from_memory, 'xmp_load_module_from_memory');
LoadLibFn(xmp_load_module_from_file, 'xmp_load_module_from_file');
LoadLibFn(xmp_load_module_from_callbacks, 'xmp_load_module_from_callbacks');
LoadLibFn(xmp_test_module, 'xmp_test_module');
LoadLibFn(xmp_test_module_from_memory, 'xmp_test_module_from_memory');
LoadLibFn(xmp_test_module_from_file, 'xmp_test_module_from_file');
LoadLibFn(xmp_test_module_from_callbacks, 'xmp_test_module_from_callbacks');
LoadLibFn(xmp_scan_module, 'xmp_scan_module');
LoadLibFn(xmp_release_module, 'xmp_release_module');
LoadLibFn(xmp_start_player, 'xmp_start_player');
LoadLibFn(xmp_play_frame, 'xmp_play_frame');
LoadLibFn(xmp_play_buffer, 'xmp_play_buffer');
LoadLibFn(xmp_get_frame_info, 'xmp_get_frame_info');
LoadLibFn(xmp_end_player, 'xmp_end_player');
LoadLibFn(xmp_inject_event, 'xmp_inject_event');
LoadLibFn(xmp_get_module_info, 'xmp_get_module_info');
LoadLibFn(xmp_get_format_list, 'xmp_get_format_list');
LoadLibFn(xmp_next_position, 'xmp_next_position');
LoadLibFn(xmp_prev_position, 'xmp_prev_position');
LoadLibFn(xmp_set_position, 'xmp_set_position');
LoadLibFn(xmp_set_row, 'xmp_set_row');
LoadLibFn(xmp_set_tempo_factor, 'xmp_set_tempo_factor');
LoadLibFn(xmp_stop_module, 'xmp_stop_module');
LoadLibFn(xmp_restart_module, 'xmp_restart_module');
LoadLibFn(xmp_seek_time, 'xmp_seek_time');
LoadLibFn(xmp_channel_mute, 'xmp_channel_mute');
LoadLibFn(xmp_channel_vol, 'xmp_channel_vol');
LoadLibFn(xmp_set_player, 'xmp_set_player');
LoadLibFn(xmp_get_player, 'xmp_get_player');
LoadLibFn(xmp_set_instrument_path, 'xmp_set_instrument_path');
LoadLibFn(xmp_start_smix, 'xmp_start_smix');
LoadLibFn(xmp_end_smix, 'xmp_end_smix');
LoadLibFn(xmp_smix_play_instrument, 'xmp_smix_play_instrument');
LoadLibFn(xmp_smix_play_sample, 'xmp_smix_play_sample');
LoadLibFn(xmp_smix_channel_pan, 'xmp_smix_channel_pan');
LoadLibFn(xmp_smix_load_sample, 'xmp_smix_load_sample');
LoadLibFn(xmp_smix_release_sample, 'xmp_smix_release_sample');
{$pop}
end;

function XmpLoaded: Boolean;
begin
  Result := library_handle <> NilHandle;
end;

initialization
 // LoadLib(FindLibName(XMP_LIB_NAME));

finalization
  //if library_handle <> NilHandle then
  //  FreeLibrary(library_handle);

end.

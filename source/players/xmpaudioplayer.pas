unit XmpAudioPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, libxmp, libraudio,
  rAudioIntf, contnrs, syncobjs, math, ctypes;

type
  { TXmpAudioPlayer }
  TXmpAudioPlayer = class(TInterfacedObject, IMusicPlayer)
  private
    FStream: TAudioStream;
    FFilename: string;
    FXmpContext: xmp_context;
    FIsPaused: Boolean;
    FLoopMode: Boolean;
    FCurrentTrack: Integer;
    FPositionLock: TCriticalSection;
    FTrackEndTriggered: Boolean;
    FFrameInfo: xmp_frame_info;
    FModuleInfo: xmp_module_info;

    // Event handlers
    FOnPlay: TPlayEvent;
    FOnPause: TPauseEvent;
    FOnStop: TStopEvent;
    FOnEnd: TEndEvent;
    FOnError: TErrorEvent;
    FOnLoad: TLoadEvent;
    // Эквалайзер
    FBalance: Single;
    FBands: array[0..BANDS_COUNT-1] of TEqualizerBand;
    FFilterCoeffs: array[0..BANDS_COUNT-1] of TFilterCoeffs;
    FFilterHistory: array[0..BANDS_COUNT-1, 0..1, 0..1] of Single; // [band][channel][history]

    class var FPlayers: TFPHashList;
    class var FCurrentPlayer: TXmpAudioPlayer;

    class constructor ClassCreate;
    class destructor ClassDestroy;

    procedure InitializeAudioStream;
    procedure ResetPlayback;
    class procedure AudioCallback(bufferData: pointer; frames: LongWord); static; cdecl;
    class procedure AudioProcessEqualizer(buffer: pointer; frames: LongWord); static; cdecl;
    procedure InternalStop(ClearModule: Boolean = True);
    procedure CheckError(Condition: Boolean; const Msg: string);
    procedure LoadModuleFile(const MusicFile: string);
    procedure FreeModuleData;

    // IMusicPlayer property getters
    function GetOnEnd: TEndEvent;
    function GetOnError: TErrorEvent;
    function GetOnLoad: TLoadEvent;
    function GetOnPause: TPauseEvent;
    function GetOnPlay: TPlayEvent;
    function GetOnStop: TStopEvent;
    function GetPlayerState: TPlayerState;
    function GetTrackNumber: Integer;
    procedure SetTrackNumber(AValue: Integer);
    // Методы эквалайзера
    procedure InitializeEqualizer;
    procedure CalculateFilterCoefficients(bandIndex: Integer);
    function ApplyFilter(input: Single; bandIndex, channel: Integer): Single;
    function GetBalance: Single;
    procedure SetBalance(AValue: Single);

    const
      DEFAULT_FREQ = 44100;
      DEFAULT_BITS = 16;
      DEFAULT_CHANNELS = 2;
      BUFFER_SIZE = 8192;

  public
    constructor Create;
    destructor Destroy; override;

    // IMusicPlayer implementation
    procedure OpenMusicFile(const MusicFile: String);
    procedure Play;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    procedure SetPosition(PositionMs: Integer);
    function GetPosition: Integer;
    function GetDuration: Integer;
    procedure SetLoopMode(Mode: Boolean);
    function GetLoopMode: Boolean;
    function IsPlaying: Boolean;
    function IsPaused: Boolean;
    function IsStopped: Boolean;
    function GetState: TPlayerState;
    function GetCurrentTrack: Integer;
    function GetCurrentFile: String;
    function GetTrackCount: Integer;
    // Эквалайзер
    procedure SetEqualizerBand(BandIndex: Integer; Gain: Single);
    function GetEqualizerBand(BandIndex: Integer): Single;
    procedure ResetEqualizer;

    // Event properties setters
    procedure SetOnEnd(AEvent: TEndEvent);
    procedure SetOnError(AEvent: TErrorEvent);
    procedure SetOnLoad(AEvent: TLoadEvent);
    procedure SetOnPause(AEvent: TPauseEvent);
    procedure SetOnPlay(AEvent: TPlayEvent);
    procedure SetOnStop(AEvent: TStopEvent);

    // IMusicPlayer properties
    property TrackNumber: Integer read GetTrackNumber write SetTrackNumber;
    property OnPlay: TPlayEvent read GetOnPlay write SetOnPlay;
    property OnPause: TPauseEvent read GetOnPause write SetOnPause;
    property OnStop: TStopEvent read GetOnStop write SetOnStop;
    property OnEnd: TEndEvent read GetOnEnd write SetOnEnd;
    property OnError: TErrorEvent read GetOnError write SetOnError;
    property OnLoad: TLoadEvent read GetOnLoad write SetOnLoad;
    property State: TPlayerState read GetPlayerState;
  end;

implementation

{ TXmpAudioPlayer }

class constructor TXmpAudioPlayer.ClassCreate;
begin
  FPlayers := TFPHashList.Create;
  FCurrentPlayer := nil;
end;

class destructor TXmpAudioPlayer.ClassDestroy;
begin
  FPlayers.Free;
end;

constructor TXmpAudioPlayer.Create;
begin
  inherited Create;
  FTrackEndTriggered := False;
  FIsPaused := False;
  FLoopMode := False;
  FCurrentTrack := 0;
  FPositionLock := TCriticalSection.Create;
  FXmpContext := nil;
  FBalance := 0.0;

  // Инициализация контекста XMP
  FXmpContext := xmp_create_context();

  if FXmpContext = nil then
    raise Exception.Create('Failed to create XMP context');

  InitializeAudioStream;
  InitializeEqualizer;
end;

destructor TXmpAudioPlayer.Destroy;
begin
   StopAudioStream(FStream);
//  SetAudioStreamCallback(FStream, @AudioCallback);
  DetachAudioMixedProcessor(@AudioProcessEqualizer);
  InternalStop;
  FreeModuleData;
  FPositionLock.Free;

  if FXmpContext <> nil then
  begin
    xmp_free_context(FXmpContext);
    FXmpContext := nil;
  end;

  inherited Destroy;
end;

procedure TXmpAudioPlayer.InitializeAudioStream;
begin
  SetAudioStreamBufferSizeDefault(BUFFER_SIZE);
  FStream := LoadAudioStream(DEFAULT_FREQ, DEFAULT_BITS, DEFAULT_CHANNELS);
  if not IsAudioStreamReady(FStream) then
    raise Exception.Create('Failed to initialize audio stream');

  FPlayers.Add(IntToStr(PtrInt(Self)), Self);
  SetAudioStreamVolume(FStream, 1.0);
  SetAudioStreamCallback(FStream, @AudioCallback);
  AttachAudioStreamProcessor(FStream, @AudioProcessEqualizer);
end;

procedure TXmpAudioPlayer.LoadModuleFile(const MusicFile: string);
var
  Error: Integer;
begin
  FreeModuleData;

  try
    // Загружаем модуль из файла
    Error := xmp_load_module(FXmpContext, PChar(MusicFile));
    if Error <> 0 then
      raise Exception.Create('Failed to load XMP module: Error ' + IntToStr(Error));

    // Запускаем плеер
    Error := xmp_start_player(FXmpContext, DEFAULT_FREQ, 0);
    if Error <> 0 then
      raise Exception.Create('Failed to start XMP player: Error ' + IntToStr(Error));

    // Получаем информацию о модуле
   // xmp_get_module_info(FXmpContext, FModuleInfo);
//    xmp_get_frame_info(FXmpContext, FFrameInfo);

    FFilename := MusicFile;

    if Assigned(FOnLoad) then
      FOnLoad(Self, MusicFile, 0); // XMP обычно однодорожечные модули

  except
    FreeModuleData;
    raise;
  end;
end;

procedure TXmpAudioPlayer.FreeModuleData;
begin
  if FXmpContext <> nil then
  begin
    xmp_stop_module(FXmpContext);
    xmp_end_player(FXmpContext);
    xmp_release_module(FXmpContext);
  end;
end;

procedure TXmpAudioPlayer.ResetPlayback;
begin
  if FXmpContext <> nil then
  begin
    xmp_restart_module(FXmpContext);
  end;
end;
{
class procedure TXmpAudioPlayer.AudioCallback(bufferData: pointer; frames: LongWord); cdecl;
var
  SamplesRendered: Integer;
  CurrentPosition: Integer;
begin
  if FCurrentPlayer = nil then Exit;

  with FCurrentPlayer do
  begin
    FPositionLock.Enter;
    try
      if (FXmpContext = nil) or FIsPaused then
      begin
        FillChar(bufferData^, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8), 0);
        Exit;
      end;

      // Рендерим звук в буфер
      SamplesRendered := xmp_play_buffer(
        FXmpContext,
        bufferData,
        frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8),
        Integer(FLoopMode)
      );

      // Проверяем окончание трека
      xmp_get_frame_info(FXmpContext, FFrameInfo);
      CurrentPosition := FFrameInfo.time;

      if (SamplesRendered = 0) or (CurrentPosition >= FFrameInfo.total_time) then
      begin
        if Assigned(FOnEnd) and (not FLoopMode) then
        begin
          FOnEnd(FCurrentPlayer, FCurrentTrack, True);
          FTrackEndTriggered := True;
        end;

        if FCurrentPlayer.GetLoopMode then
        begin
          ResetPlayback;
          FTrackEndTriggered := False;
        end;
      end;

    finally
      FPositionLock.Leave;
      if FTrackEndTriggered then InternalStop(True);
    end;
  end;
end;
}

class procedure TXmpAudioPlayer.AudioCallback(bufferData: pointer; frames: LongWord); cdecl;
var
  {%H-}SamplesRendered: Integer;
begin
  if FCurrentPlayer = nil then Exit;

  with FCurrentPlayer do
  begin
    FPositionLock.Enter;
    try
      if (FXmpContext = nil) or FIsPaused then
      begin
        FillChar(bufferData^, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8), 0);
        Exit;
      end;

      // Рендерим звук в буфер
      SamplesRendered := xmp_play_buffer(
        FXmpContext,
        bufferData,

       frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8),
        Integer(FLoopMode)
      );

      // TTF анализ
     // AnalyzeAudioBuffer(bufferData, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8));

      // Проверяем окончание трека
      xmp_get_frame_info(FXmpContext, FFrameInfo);


      if FFrameInfo.time >= FFrameInfo.total_time then
      begin
         if Assigned(FOnEnd) and (not FLoopMode) then
          begin
            FOnEnd(FCurrentPlayer, FCurrentTrack, True);
            FTrackEndTriggered := True;
          end;

          if FLoopMode then
          begin
            FTrackEndTriggered := False;
          end;


        ResetPlayback;
    end;

    finally
      FPositionLock.Leave;
      if FTrackEndTriggered then InternalStop(True);
    end;
  end;
end;

class procedure TXmpAudioPlayer.AudioProcessEqualizer(buffer: pointer;
  frames: LongWord); cdecl;
var
  BufferData: PSingle;
  frame: LongWord;
  inputLeft, inputRight, outputLeft, outputRight: Single;
  band: Integer;
  leftGain, rightGain: Single;
begin
  if (FCurrentPlayer = nil) then Exit;
  BufferData := PSingle(buffer);

  // Расчет коэффициентов усиления для баланса
  if FCurrentPlayer.FBalance < 0 then
  begin
    // Сдвиг влево
    leftGain := 1.0;
    rightGain := 1.0 + FCurrentPlayer.FBalance;
  end
  else if FCurrentPlayer.FBalance > 0 then
  begin
    // Сдвиг вправо
    leftGain := 1.0 - FCurrentPlayer.FBalance;
    rightGain := 1.0;
  end
  else
  begin
    // Центр
    leftGain := 1.0;
    rightGain := 1.0;
  end;

  // Обновляем коэффициенты фильтров (на случай изменения настроек)
  for band := 0 to BANDS_COUNT - 1 do
    FCurrentPlayer.CalculateFilterCoefficients(band);

  // Обработка каждого семпла
  for frame := 0 to frames - 1 do
  begin
    inputLeft := BufferData[frame * 2];
    inputRight := BufferData[frame * 2 + 1];

    outputLeft := inputLeft;
    outputRight := inputRight;

    // Применяем все полосы эквалайзера
    for band := 0 to BANDS_COUNT - 1 do
    begin
      outputLeft := FCurrentPlayer.ApplyFilter(outputLeft, band, 0);
      outputRight := FCurrentPlayer.ApplyFilter(outputRight, band, 1);
    end;

    // Применяем баланс
    outputLeft := outputLeft * leftGain;
    outputRight := outputRight * rightGain;

    // Записываем обратно в буфер
    BufferData[frame * 2] := outputLeft;
    BufferData[frame * 2 + 1] := outputRight;
  end;
end;

procedure TXmpAudioPlayer.CheckError(Condition: Boolean; const Msg: string);
begin
  if Condition and Assigned(FOnError) then
    FOnError(Self, Msg);
end;

procedure TXmpAudioPlayer.InternalStop(ClearModule: Boolean);
begin
  FPositionLock.Enter;
  try
    // Проверяем, является ли этот плеер текущим и активным
    if (FCurrentPlayer = Self) and IsAudioStreamPlaying(FStream) then
    begin
      StopAudioStream(FStream);
      FCurrentPlayer := nil;

      if ClearModule then
        FreeModuleData;

      FIsPaused := False;
      FTrackEndTriggered := False;

      if Assigned(FOnStop) then
        FOnStop(Self, FCurrentTrack);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

// IMusicPlayer implementation
procedure TXmpAudioPlayer.OpenMusicFile(const MusicFile: String);
begin
  if not FileExists(MusicFile) then
  begin
    CheckError(True, 'File not found: ' + MusicFile);
    Exit;
  end;

  FPositionLock.Enter;
  try
    // Останавливаем текущее воспроизведение только если оно активно
    if (FCurrentPlayer = Self) and IsAudioStreamPlaying(FStream) then
      InternalStop;

    // Загружаем новый модуль
    try
      LoadModuleFile(MusicFile);
      FCurrentTrack := 0;
    except
      on E: Exception do
      begin
        CheckError(True, 'Error loading module: ' + E.Message);
        InternalStop;
      end;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TXmpAudioPlayer.Play;
begin
  FPositionLock.Enter;
  try
    if (FXmpContext <> nil) and not IsAudioStreamPlaying(FStream) then
    begin
      // Начинаем воспроизведение
      FCurrentPlayer := Self;
      PlayAudioStream(FStream);
      FIsPaused := False;
      FTrackEndTriggered := False;

      if Assigned(FOnPlay) then
        FOnPlay(Self, FCurrentTrack);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TXmpAudioPlayer.Pause;
begin
  FPositionLock.Enter;
  try
    if (FCurrentPlayer = Self) and not FIsPaused then
    begin
      PauseAudioStream(FStream);
      FIsPaused := True;

      if Assigned(FOnPause) then
        FOnPause(Self, FCurrentTrack);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TXmpAudioPlayer.Resume;
begin
  FPositionLock.Enter;
  try
    if (FCurrentPlayer = Self) and FIsPaused then
    begin
      ResumeAudioStream(FStream);
      FIsPaused := False;

      if Assigned(FOnPlay) then
        FOnPlay(Self, FCurrentTrack);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TXmpAudioPlayer.Stop;
begin
  InternalStop(False);
  SetPosition(0);
end;

procedure TXmpAudioPlayer.SetPosition(PositionMs: Integer);
begin
  FPositionLock.Enter;
  try
    if FXmpContext <> nil then
    begin
      xmp_seek_time(FXmpContext, PositionMs);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TXmpAudioPlayer.GetPosition: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if FXmpContext <> nil then
    begin
      xmp_get_frame_info(FXmpContext, FFrameInfo);
      Result := FFrameInfo.time;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TXmpAudioPlayer.GetDuration: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if FXmpContext <> nil then
    begin
      xmp_get_frame_info(FXmpContext, FFrameInfo);
      Result := FFrameInfo.total_time;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TXmpAudioPlayer.SetLoopMode(Mode: Boolean);
begin
  FLoopMode := Mode;
  // XMP автоматически обрабатывает loop через параметр в xmp_play_buffer
end;

function TXmpAudioPlayer.GetLoopMode: Boolean;
begin
  Result := FLoopMode;
end;

function TXmpAudioPlayer.IsPlaying: Boolean;
begin
  Result := (FCurrentPlayer = Self) and not FIsPaused and (FXmpContext <> nil);
end;

function TXmpAudioPlayer.IsPaused: Boolean;
begin
  Result := FIsPaused;
end;

function TXmpAudioPlayer.IsStopped: Boolean;
begin
  Result := (FXmpContext = nil) or (FCurrentPlayer <> Self) or
            (not IsAudioStreamPlaying(FStream) and not FIsPaused);
end;

function TXmpAudioPlayer.GetState: TPlayerState;
begin
  if IsPlaying then
    Result := psPlaying
  else if IsPaused then
    Result := psPaused
  else
    Result := psStopped;
end;

function TXmpAudioPlayer.GetCurrentTrack: Integer;
begin
  Result := FCurrentTrack;
end;

function TXmpAudioPlayer.GetCurrentFile: String;
begin
  Result := FFilename;
end;

function TXmpAudioPlayer.GetTrackCount: Integer;
begin
  Result := 1; // XMP обычно обрабатывает однодорожечные модули
end;

procedure TXmpAudioPlayer.SetEqualizerBand(BandIndex: Integer; Gain: Single);
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
  begin
    // Всегда сохраняем настройки
    FBands[BandIndex].Gain := Max(-12.0, Min(12.0, Gain));
    CalculateFilterCoefficients(BandIndex);
  end;
end;

function TXmpAudioPlayer.GetEqualizerBand(BandIndex: Integer): Single;
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
    Result := FBands[BandIndex].Gain
  else
    Result := 0.0;
end;

procedure TXmpAudioPlayer.ResetEqualizer;
var
  i: Integer;
  freq: Single;
begin
  // Настройка частотных полос (октавные полосы)
  freq := 32.0;
  for i := 0 to BANDS_COUNT - 1 do
  begin
    FBands[i].Frequency := freq;
    FBands[i].Gain := 0.0; // Нейтральное положение
    FBands[i].Q := 1.0;    // Стандартная добротность
    freq := freq * 2.0; // Октавное увеличение частоты
  end;

  // Инициализация истории фильтров
  FillChar(FFilterHistory, SizeOf(FFilterHistory), 0);

  // Расчет начальных коэффициентов
  for i := 0 to BANDS_COUNT - 1 do
    CalculateFilterCoefficients(i);
end;

function TXmpAudioPlayer.GetBalance: Single;
begin
  FPositionLock.Enter;
  try
    Result := FBalance;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TXmpAudioPlayer.SetBalance(AValue: Single);
begin
  FPositionLock.Enter;
  try
    // Ограничиваем значение от -1.0 до +1.0
    FBalance := Max(-1.0, Min(1.0, AValue));
  finally
    FPositionLock.Leave;
  end;
end;

// IMusicPlayer property getters
function TXmpAudioPlayer.GetOnEnd: TEndEvent;
begin
  Result := FOnEnd;
end;

function TXmpAudioPlayer.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TXmpAudioPlayer.GetOnLoad: TLoadEvent;
begin
  Result := FOnLoad;
end;

function TXmpAudioPlayer.GetOnPause: TPauseEvent;
begin
  Result := FOnPause;
end;

function TXmpAudioPlayer.GetOnPlay: TPlayEvent;
begin
  Result := FOnPlay;
end;

function TXmpAudioPlayer.GetOnStop: TStopEvent;
begin
  Result := FOnStop;
end;

function TXmpAudioPlayer.GetPlayerState: TPlayerState;
begin
  Result := GetState;
end;

function TXmpAudioPlayer.GetTrackNumber: Integer;
begin
  Result := FCurrentTrack;
end;

procedure TXmpAudioPlayer.SetTrackNumber(AValue: Integer);
begin
  FCurrentTrack := AValue;
end;

procedure TXmpAudioPlayer.InitializeEqualizer;
var
  i: Integer;
  freq: Single;
begin
  // Настройка частотных полос (октавные полосы)
  freq := 32.0;
  for i := 0 to BANDS_COUNT - 1 do
  begin
    FBands[i].Frequency := freq;
    FBands[i].Gain := 0.0; // Нейтральное положение
    FBands[i].Q := 1.0;    // Стандартная добротность
    freq := freq * 2.0; // Октавное увеличение частоты
  end;
end;

procedure TXmpAudioPlayer.CalculateFilterCoefficients(bandIndex: Integer);
var
  band: TEqualizerBand;
  A, w0, alpha: Single;
  cosW0, sinW0: Single;
  coeffs: TFilterCoeffs;
begin
  band := FBands[bandIndex];

  // Преобразование dB в линейное значение
  A := Power(10.0, band.Gain / 40.0);
  w0 := 2.0 * PI * band.Frequency / DEFAULT_FREQ;
  alpha := sin(w0) / (2.0 * band.Q);

  cosW0 := cos(w0);
  sinW0 := sin(w0);

  // Расчет коэффициентов для peaking EQ filter
  coeffs.a0 := 1.0 + alpha / A;
  coeffs.a1 := -2.0 * cosW0;
  coeffs.a2 := 1.0 - alpha / A;
  coeffs.b1 := -2.0 * cosW0;
  coeffs.b2 := 1.0 - alpha * A;

  // Нормализация
  coeffs.a1 := coeffs.a1 / coeffs.a0;
  coeffs.a2 := coeffs.a2 / coeffs.a0;
  coeffs.b1 := coeffs.b1 / coeffs.a0;
  coeffs.b2 := coeffs.b2 / coeffs.a0;
  coeffs.a0 := 1.0;

  FFilterCoeffs[bandIndex] := coeffs;
end;

function TXmpAudioPlayer.ApplyFilter(input: Single; bandIndex,
  channel: Integer): Single;
var
  coeffs: TFilterCoeffs;
  output: Single;
begin
  coeffs := FFilterCoeffs[bandIndex];

  output := coeffs.a0 * input +
            coeffs.a1 * FFilterHistory[bandIndex, channel, 0] +
            coeffs.a2 * FFilterHistory[bandIndex, channel, 1] -
            coeffs.b1 * FFilterHistory[bandIndex, channel, 0] -
            coeffs.b2 * FFilterHistory[bandIndex, channel, 1];

  // Обновление истории
  FFilterHistory[bandIndex, channel, 1] := FFilterHistory[bandIndex, channel, 0];
  FFilterHistory[bandIndex, channel, 0] := output;

  Result := output;
end;

// Event properties setters
procedure TXmpAudioPlayer.SetOnEnd(AEvent: TEndEvent);
begin
  FOnEnd := AEvent;
end;

procedure TXmpAudioPlayer.SetOnError(AEvent: TErrorEvent);
begin
  FOnError := AEvent;
end;

procedure TXmpAudioPlayer.SetOnLoad(AEvent: TLoadEvent);
begin
  FOnLoad := AEvent;
end;

procedure TXmpAudioPlayer.SetOnPause(AEvent: TPauseEvent);
begin
  FOnPause := AEvent;
end;

procedure TXmpAudioPlayer.SetOnPlay(AEvent: TPlayEvent);
begin
  FOnPlay := AEvent;
end;

procedure TXmpAudioPlayer.SetOnStop(AEvent: TStopEvent);
begin
  FOnStop := AEvent;
end;

end.

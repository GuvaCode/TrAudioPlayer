unit OpenMptAudioPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, libopenmpt, libraudio,
  rAudioIntf, contnrs, syncobjs, math, ctypes;

type
  { TOpenMPTAudioPlayer }
  TOpenMPTAudioPlayer = class(TInterfacedObject, IMusicPlayer)
  private
    FStream: TAudioStream;
    FFilename: string;
    FFileData: Pointer;
    FFileSize: NativeUInt;
    FOpenMPTModule: Popenmpt_module;
    FIsPaused: Boolean;
    FLoopMode: Boolean;
    FCurrentTrack: Integer;
    FPositionLock: TCriticalSection;
    FTrackEndTriggered: Boolean;
    FDurationSeconds: Double;

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
    class var FCurrentPlayer: TOpenMPTAudioPlayer;

    class constructor ClassCreate;
    class destructor ClassDestroy;

    procedure InitializeAudioStream;
    procedure ResetPlayback;
    class procedure AudioCallback(bufferData: pointer; frames: LongWord); static; cdecl;
    class procedure AudioProcessEqualizer({%H-}buffer: pointer; {%H-}frames: LongWord); static; cdecl;
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

{ TOpenMPTAudioPlayer }

class constructor TOpenMPTAudioPlayer.ClassCreate;
begin
  FPlayers := TFPHashList.Create;
  FCurrentPlayer := nil;
end;

class destructor TOpenMPTAudioPlayer.ClassDestroy;
begin
  FPlayers.Free;
end;

constructor TOpenMPTAudioPlayer.Create;
begin
  inherited Create;
  FTrackEndTriggered := False;
  FIsPaused := False;
  FLoopMode := False;
  FCurrentTrack := 0;
  FPositionLock := TCriticalSection.Create;
  FFileData := nil;
  FFileSize := 0;
  FOpenMPTModule := nil;
  FDurationSeconds := 0;
  FBalance := 0.0;
  InitializeAudioStream;
  InitializeEqualizer;
end;

destructor TOpenMPTAudioPlayer.Destroy;
begin
  InternalStop;
  FreeModuleData;
  FPositionLock.Free;
  inherited Destroy;
end;

procedure TOpenMPTAudioPlayer.InitializeAudioStream;
begin
  SetAudioStreamBufferSizeDefault(BUFFER_SIZE);
  FStream := LoadAudioStream(DEFAULT_FREQ, DEFAULT_BITS, DEFAULT_CHANNELS);
  if not IsAudioStreamReady(FStream) then
    raise Exception.Create('Failed to initialize audio stream');

  FPlayers.Add(IntToStr(PtrInt(Self)), Self);
  SetAudioStreamVolume(FStream,1.0);
  SetAudioStreamCallback(FStream, @AudioCallback);
  AttachAudioStreamProcessor(FStream, @AudioProcessEqualizer);
end;

procedure TOpenMPTAudioPlayer.LoadModuleFile(const MusicFile: string);
var
  FileStream: TFileStream;
  Error: cint;
  ErrorMessage: pchar;
begin
  FreeModuleData;

  try
    FileStream := TFileStream.Create(MusicFile, fmOpenRead or fmShareDenyWrite);
    try
      FFileSize := FileStream.Size;
      GetMem(FFileData, FFileSize);
      FileStream.ReadBuffer(FFileData^, FFileSize);
    finally
      FileStream.Free;
    end;

    // Создаем OpenMPT модуль из памяти
    FOpenMPTModule := openmpt_module_create_from_memory2(
      FFileData, FFileSize,
      nil, nil,  // logfunc, loguser
      nil, nil,   // errfunc, erruser
      @Error, @ErrorMessage,
      nil         // ctls
    );

    if FOpenMPTModule = nil then
    begin
      if ErrorMessage <> nil then
        raise Exception.Create('Failed to create OpenMPT module: ' + string(ErrorMessage))
      else
        raise Exception.Create('Failed to create OpenMPT module: Error ' + IntToStr(Error));
    end;

    // Получаем длительность трека
    FDurationSeconds := openmpt_module_get_duration_seconds(FOpenMPTModule);

    FFilename := MusicFile;

    if Assigned(FOnLoad) then
      FOnLoad(Self, MusicFile, 0); // OpenMPT обычно однодорожечные модули

  except
    FreeModuleData;
    raise;
  end;
end;

procedure TOpenMPTAudioPlayer.FreeModuleData;
begin
  if FOpenMPTModule <> nil then
  begin
    openmpt_module_destroy(FOpenMPTModule);
    FOpenMPTModule := nil;
  end;

  if FFileData <> nil then
  begin
    FreeMem(FFileData);
    FFileData := nil;
    FFileSize := 0;
  end;
end;

procedure TOpenMPTAudioPlayer.ResetPlayback;
begin
  if FOpenMPTModule <> nil then
  begin
    openmpt_module_set_position_seconds(FOpenMPTModule, 0);
  end;
end;

class procedure TOpenMPTAudioPlayer.AudioCallback(bufferData: pointer; frames: LongWord); cdecl;
var
  SamplesRendered: csize_t;
  CurrentPosition: Double;
begin
  if FCurrentPlayer = nil then Exit;

  with FCurrentPlayer do
  begin
    FPositionLock.Enter;
    try
      if (FOpenMPTModule = nil) or FIsPaused then
      begin
        FillChar(bufferData^, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8), 0);
        Exit;
      end;

      // Рендерим звук в буфер
      SamplesRendered := openmpt_module_read_interleaved_stereo(
        FOpenMPTModule,
        DEFAULT_FREQ,
        frames,
        pcint16(bufferData)
      );

      // Проверяем окончание трека
      CurrentPosition := openmpt_module_get_position_seconds(FOpenMPTModule);

      if (SamplesRendered = 0) or (CurrentPosition >= FDurationSeconds) then
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

class procedure TOpenMPTAudioPlayer.AudioProcessEqualizer(buffer: pointer;
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

procedure TOpenMPTAudioPlayer.CheckError(Condition: Boolean; const Msg: string);
begin
  if Condition and Assigned(FOnError) then
    FOnError(Self, Msg);
end;

procedure TOpenMPTAudioPlayer.InternalStop(ClearModule: Boolean);
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
procedure TOpenMPTAudioPlayer.OpenMusicFile(const MusicFile: String);
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

procedure TOpenMPTAudioPlayer.Play;
begin
  FPositionLock.Enter;
  try
    if (FOpenMPTModule <> nil) and not IsAudioStreamPlaying(FStream) then
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

procedure TOpenMPTAudioPlayer.Pause;
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

procedure TOpenMPTAudioPlayer.Resume;
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

procedure TOpenMPTAudioPlayer.Stop;
begin
  InternalStop(False);
  SetPosition(0);
end;

procedure TOpenMPTAudioPlayer.SetPosition(PositionMs: Integer);
var
  PositionSeconds: Double;
begin
  FPositionLock.Enter;
  try
    if FOpenMPTModule <> nil then
    begin
      PositionSeconds := PositionMs / 1000;
      openmpt_module_set_position_seconds(FOpenMPTModule, PositionSeconds);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TOpenMPTAudioPlayer.GetPosition: Integer;
var
  PositionSeconds: Double;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if FOpenMPTModule <> nil then
    begin
      PositionSeconds := openmpt_module_get_position_seconds(FOpenMPTModule);
      Result := Round(PositionSeconds * 1000);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TOpenMPTAudioPlayer.GetDuration: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if FOpenMPTModule <> nil then
    begin
      Result := Round(FDurationSeconds * 1000);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TOpenMPTAudioPlayer.SetLoopMode(Mode: Boolean);
begin
  if FOpenMPTModule <> nil then
  begin
    if Mode then
      openmpt_module_set_repeat_count(FOpenMPTModule, -1) // Бесконечное повторение
    else
      openmpt_module_set_repeat_count(FOpenMPTModule, 0); // Без повторения

    FLoopMode := Mode;
  end;
end;

function TOpenMPTAudioPlayer.GetLoopMode: Boolean;
var
  RepeatCount: cint32;
begin
  if FOpenMPTModule <> nil then
  begin
    RepeatCount := openmpt_module_get_repeat_count(FOpenMPTModule);
    FLoopMode := (RepeatCount = -1);
  end;
  Result := FLoopMode;
end;

function TOpenMPTAudioPlayer.IsPlaying: Boolean;
begin
  Result := (FCurrentPlayer = Self) and not FIsPaused and (FOpenMPTModule <> nil);
end;

function TOpenMPTAudioPlayer.IsPaused: Boolean;
begin
  Result := FIsPaused;
end;

function TOpenMPTAudioPlayer.IsStopped: Boolean;
begin
  Result := (FOpenMPTModule = nil) or (FCurrentPlayer <> Self) or
            (not IsAudioStreamPlaying(FStream) and not FIsPaused);
end;

function TOpenMPTAudioPlayer.GetState: TPlayerState;
begin
  if IsPlaying then
    Result := psPlaying
  else if IsPaused then
    Result := psPaused
  else
    Result := psStopped;
end;

function TOpenMPTAudioPlayer.GetCurrentTrack: Integer;
begin
  Result := FCurrentTrack;
end;

function TOpenMPTAudioPlayer.GetCurrentFile: String;
begin
  Result := FFilename;
end;

function TOpenMPTAudioPlayer.GetTrackCount: Integer;
begin
  Result := 0; // OpenMPT обычно обрабатывает однодорожечные модули
end;


procedure TOpenMPTAudioPlayer.SetEqualizerBand(BandIndex: Integer; Gain: Single
  );
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
  begin
    // Всегда сохраняем настройки
    FBands[BandIndex].Gain := Max(-12.0, Min(12.0, Gain));
    CalculateFilterCoefficients(BandIndex);
  end;
end;

function TOpenMPTAudioPlayer.GetEqualizerBand(BandIndex: Integer): Single;
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
    Result := FBands[BandIndex].Gain
  else
    Result := 0.0;
end;

procedure TOpenMPTAudioPlayer.ResetEqualizer;
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

function TOpenMPTAudioPlayer.GetBalance: Single;
begin
   FPositionLock.Enter;
  try
    Result := FBalance;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TOpenMPTAudioPlayer.SetBalance(AValue: Single);
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
function TOpenMPTAudioPlayer.GetOnEnd: TEndEvent;
begin
  Result := FOnEnd;
end;

function TOpenMPTAudioPlayer.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TOpenMPTAudioPlayer.GetOnLoad: TLoadEvent;
begin
  Result := FOnLoad;
end;

function TOpenMPTAudioPlayer.GetOnPause: TPauseEvent;
begin
  Result := FOnPause;
end;

function TOpenMPTAudioPlayer.GetOnPlay: TPlayEvent;
begin
  Result := FOnPlay;
end;

function TOpenMPTAudioPlayer.GetOnStop: TStopEvent;
begin
  Result := FOnStop;
end;

function TOpenMPTAudioPlayer.GetPlayerState: TPlayerState;
begin
  Result := GetState;
end;

function TOpenMPTAudioPlayer.GetTrackNumber: Integer;
begin
  Result := FCurrentTrack;
end;

procedure TOpenMPTAudioPlayer.SetTrackNumber(AValue: Integer);
begin
  FCurrentTrack := AValue;
end;

procedure TOpenMPTAudioPlayer.InitializeEqualizer;
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

procedure TOpenMPTAudioPlayer.CalculateFilterCoefficients(bandIndex: Integer);
var
  band: TEqualizerBand;
  A, w0, alpha: Single;
  cosW0, sinW0: Single;
  coeffs: TFilterCoeffs;
begin
  band := FBands[bandIndex]; // Исправлено: FBands вместо Bands

  // Преобразование dB в линейное значение
  A := Power(10.0, band.Gain / 40.0);
  w0 := 2.0 * PI * band.Frequency / 44100; // Исправлено: SAMPLE_RATE вместо 44100
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

function TOpenMPTAudioPlayer.ApplyFilter(input: Single; bandIndex,
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
  FFilterHistory[bandIndex, channel, 0] := output;//input;

  Result := output;
end;

// Event properties setters
procedure TOpenMPTAudioPlayer.SetOnEnd(AEvent: TEndEvent);
begin
  FOnEnd := AEvent;
end;

procedure TOpenMPTAudioPlayer.SetOnError(AEvent: TErrorEvent);
begin
  FOnError := AEvent;
end;

procedure TOpenMPTAudioPlayer.SetOnLoad(AEvent: TLoadEvent);
begin
  FOnLoad := AEvent;
end;

procedure TOpenMPTAudioPlayer.SetOnPause(AEvent: TPauseEvent);
begin
  FOnPause := AEvent;
end;

procedure TOpenMPTAudioPlayer.SetOnPlay(AEvent: TPlayEvent);
begin
  FOnPlay := AEvent;
end;

procedure TOpenMPTAudioPlayer.SetOnStop(AEvent: TStopEvent);
begin
  FOnStop := AEvent;
end;

end.

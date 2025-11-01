unit ZxTuneAudioPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, libZxTune, libraudio,
  rAudioIntf, contnrs, syncobjs, math;

type
  { TZxTuneAudioPlayer }
  TZxTuneAudioPlayer = class(TInterfacedObject, IMusicPlayer)
  private
    FStream: TAudioStream;
    FFilename: string;
    FFileData: Pointer;
    FFileSize: NativeUInt;
    FZxTuneData: ZXTuneHandle;
    FZxTuneModule: ZXTuneHandle;
    FZxTunePlayer: ZXTuneHandle;
    FIsPaused: Boolean;
    FLoopMode: Boolean;
    FCurrentTrack: Integer;
    FPositionLock: TCriticalSection;
    FTrackEndTriggered: Boolean;
    FModuleInfo: ZXTuneModuleInfo;
    FLoopCount: Integer;

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
    class var FCurrentPlayer: TZxTuneAudioPlayer;

    class constructor ClassCreate;
    class destructor ClassDestroy;

    procedure InitializeAudioStream;
    procedure ResetPlayback;

    class procedure AudioCallback(bufferData: pointer; frames: LongWord); static; cdecl;
    class procedure AudioProcessEqualizer({%H-}buffer: pointer; {%H-}frames: LongWord); static; cdecl;

    procedure InternalStop(ClearTune: Boolean = True);
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
      BUFFER_SIZE = 8192 * 2;

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

{ TZxTuneAudioPlayer }

class constructor TZxTuneAudioPlayer.ClassCreate;
begin
  FPlayers := TFPHashList.Create;
  FCurrentPlayer := nil;
end;

class destructor TZxTuneAudioPlayer.ClassDestroy;
begin
  FPlayers.Free;
end;

constructor TZxTuneAudioPlayer.Create;
begin
  inherited Create;
  FTrackEndTriggered := False;
  FIsPaused := False;
  FLoopMode := False;
  FCurrentTrack := 0;
  FPositionLock := TCriticalSection.Create;
  FFileData := nil;
  FFileSize := 0;
  FZxTuneData := nil;
  FZxTuneModule := nil;
  FZxTunePlayer := nil;
  FLoopCount := 0;
  FBalance := 0.0;
  InitializeAudioStream;
  InitializeEqualizer;
end;

destructor TZxTuneAudioPlayer.Destroy;
begin
  InternalStop;

  FreeModuleData;
  FPositionLock.Free;
  inherited Destroy;
end;

procedure TZxTuneAudioPlayer.InitializeAudioStream;
begin
  SetAudioStreamBufferSizeDefault(BUFFER_SIZE);
  FStream := LoadAudioStream(DEFAULT_FREQ, DEFAULT_BITS, DEFAULT_CHANNELS);
  if not IsAudioStreamReady(FStream) then
    raise Exception.Create('Failed to initialize audio stream');

  FPlayers.Add(IntToStr(PtrInt(Self)), Self);
  SetAudioStreamCallback(FStream, @AudioCallback);
  AttachAudioStreamProcessor(FStream, @AudioProcessEqualizer);

end;

procedure TZxTuneAudioPlayer.LoadModuleFile(const MusicFile: string);
var
  FileStream: TFileStream;
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

    FZxTuneData := ZXTune_CreateData(FFileData, FFileSize);
    if FZxTuneData = nil then
      raise Exception.Create('Failed to create ZXTune data');

    FZxTuneModule := ZXTune_OpenModule(FZxTuneData);
    if FZxTuneModule = nil then
    begin
      if Assigned(FOnError) then
      begin
        FOnError(self,'ZXTune: Failed to open module');
        Exit;
      end else
      raise Exception.Create('ZXTune: Failed to open module');
    end;

    if not ZXTune_GetModuleInfo(FZxTuneModule, FModuleInfo) then
    if Assigned(FOnError) then
      begin
        FOnError(self,'ZXTune: Failed to get module info');
        Exit;
      end else
     raise Exception.Create('ZXTune: Failed to get module info');

    FZxTunePlayer := ZXTune_CreatePlayer(FZxTuneModule);
    if FZxTunePlayer = nil then
      raise Exception.Create('Failed to create ZXTune player');

    FFilename := MusicFile;

    if Assigned(FOnLoad) then
      FOnLoad(Self, MusicFile, 0); // ZXTune обычно однодорожечные модули

  except
    FreeModuleData;
    raise;
  end;
end;

procedure TZxTuneAudioPlayer.FreeModuleData;
begin
  if FZxTunePlayer <> nil then
  begin
    ZXTune_DestroyPlayer(FZxTunePlayer);
    FZxTunePlayer := nil;
  end;

  if FZxTuneModule <> nil then
  begin
    ZXTune_CloseModule(FZxTuneModule);
    FZxTuneModule := nil;
  end;

  if FZxTuneData <> nil then
  begin
    ZXTune_CloseData(FZxTuneData);
    FZxTuneData := nil;
  end;

  if FFileData <> nil then
  begin
    FreeMem(FFileData);
    FFileData := nil;
    FFileSize := 0;
  end;
end;

procedure TZxTuneAudioPlayer.ResetPlayback;
begin
  if FZxTunePlayer <> nil then
  begin
    ZXTune_ResetSound(FZxTunePlayer);
  end;
end;

class procedure TZxTuneAudioPlayer.AudioCallback(bufferData: pointer; frames: LongWord); cdecl;
var
  SamplesRendered: Integer;
begin
  if FCurrentPlayer = nil then Exit;

  with FCurrentPlayer do
  begin
    FPositionLock.Enter;
    try
      if (FZxTunePlayer = nil) or FIsPaused then
      begin
        FillChar(bufferData^, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8), 0);
        Exit;
      end;

      // Рендерим звук в буфер
      SamplesRendered := ZXTune_RenderSound(FZxTunePlayer, bufferData, frames);

      if GetPosition >= GetDuration then
      begin
        if Assigned(FOnEnd) and (not FLoopMode) then
        begin
          FOnEnd(FCurrentPlayer, FCurrentTrack, true);
          FTrackEndTriggered := True;
        end;
        if FCurrentPlayer.GetLoopMode then
        begin
          if FModuleInfo.LoopFrame >= 0 then Inc(FLoopCount);
        end;
      end;
    finally
      FPositionLock.Leave;
      if FTrackEndTriggered = True then InternalStop(True);
    end;
  end;
end;

class procedure TZxTuneAudioPlayer.AudioProcessEqualizer(buffer: pointer;
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

procedure TZxTuneAudioPlayer.CheckError(Condition: Boolean; const Msg: string);
begin
  if Condition and Assigned(FOnError) then
    FOnError(Self, Msg);
end;

procedure TZxTuneAudioPlayer.InternalStop(ClearTune: Boolean);
begin
  FPositionLock.Enter;

  try
    if (FCurrentPlayer = Self) and IsAudioStreamPlaying(FStream) then
    begin
      if FCurrentPlayer = Self then
      begin
        StopAudioStream(FStream);
        FCurrentPlayer := nil;
      end;

      if ClearTune then
        FreeModuleData;

      FIsPaused := False;
      FTrackEndTriggered := False;
      FLoopCount := 0;

      if Assigned(FOnStop) then
        FOnStop(Self, FCurrentTrack);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

// IMusicPlayer implementation
procedure TZxTuneAudioPlayer.OpenMusicFile(const MusicFile: String);
begin
  if not FileExists(MusicFile) then
  begin
    CheckError(True, 'File not found: ' + MusicFile);
    Exit;
  end;

  FPositionLock.Enter;
  try
    // Stop current playback
    if IsAudioStreamPlaying(FStream) then
      InternalStop;

    // Load new module
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

procedure TZxTuneAudioPlayer.Play;
begin
  FPositionLock.Enter;
  try
    if (FZxTunePlayer <> nil) and not IsAudioStreamPlaying(FStream) then
    begin
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

procedure TZxTuneAudioPlayer.Pause;
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

procedure TZxTuneAudioPlayer.Resume;
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

procedure TZxTuneAudioPlayer.Stop;
begin
   InternalStop(False);
  SetPosition(0);
end;

procedure TZxTuneAudioPlayer.SetPosition(PositionMs: Integer);
var
  SamplePos: NativeUInt;
  FullDuration: Integer;
  FrameDuration: Integer;
  LoopPositionMs: Integer;
  EffectivePositionMs: Integer;
begin
  FPositionLock.Enter;
  try
    if FZxTunePlayer <> nil then
    begin
      // Получаем полную длительность трека
      FullDuration := GetDuration();
      if FullDuration <= 0 then Exit;

      // Получаем позицию лупа
      if FModuleInfo.LoopFrame >= 0 then
      begin
        FrameDuration := ZXTune_GetDuration(FZxTunePlayer);
        if FrameDuration <= 0 then
          FrameDuration := 20000;
        LoopPositionMs := (FModuleInfo.LoopFrame * FrameDuration) div 1000;
      end
      else
      begin
        LoopPositionMs := 0;
      end;

      // Вычисляем эффективную позицию с учетом повторов
      if FLoopCount > 0 then
      begin
        EffectivePositionMs := PositionMs - (FLoopCount * FullDuration) + (FLoopCount * LoopPositionMs);
        if EffectivePositionMs < LoopPositionMs then
        begin
          Dec(FLoopCount);
          EffectivePositionMs := PositionMs - (FLoopCount * FullDuration) + (FLoopCount * LoopPositionMs);
        end;
      end
      else
      begin
        EffectivePositionMs := PositionMs;
      end;

      // Ограничиваем позицию пределами трека
      if EffectivePositionMs > FullDuration then
        EffectivePositionMs := FullDuration;
      if EffectivePositionMs < 0 then
        EffectivePositionMs := 0;

      // Конвертируем миллисекунды в сэмплы
      SamplePos := (EffectivePositionMs * DEFAULT_FREQ) div 1000;

      // Устанавливаем позицию
      ZXTune_SeekSound(FZxTunePlayer, SamplePos);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TZxTuneAudioPlayer.GetPosition: Integer;
var
  Samples: NativeUInt;
  Frequency: Integer;
  FrameDuration: Integer;
  LoopPositionMs: Integer;
  FullDuration: Integer;
  CurrentPlaybackPos: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if FZxTunePlayer <> nil then
    begin
      // Get current position in samples
      Samples := ZXTune_GetCurrentPosition(FZxTunePlayer);
      Frequency := DEFAULT_FREQ;

      // Convert samples to milliseconds
      CurrentPlaybackPos := Round((Samples / DEFAULT_CHANNELS) / Frequency * 1000) * 2;

      // Получаем полную длительность трека
      FullDuration := GetDuration();

      // Получаем позицию лупа
      if FModuleInfo.LoopFrame >= 0 then
      begin
        FrameDuration := ZXTune_GetDuration(FZxTunePlayer);
        if FrameDuration <= 0 then
          FrameDuration := 20000;
        LoopPositionMs := (FModuleInfo.LoopFrame * FrameDuration) div 1000;
      end
      else
      begin
        LoopPositionMs := 0;
      end;

      // Рассчитываем общую позицию с учетом повторов
      if FLoopCount > 0 then
      begin
        Result := CurrentPlaybackPos + (LoopPositionMs * FLoopCount) - (FullDuration * FLoopCount);
      end
      else
      begin
        Result := CurrentPlaybackPos;
      end;

      // Корректируем, если позиция вышла за пределы
      if Result < 0 then Result := 0;
      if (FullDuration > 0) and (Result > FullDuration + (LoopPositionMs * FLoopCount)) then
        Result := FullDuration + (LoopPositionMs * FLoopCount);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TZxTuneAudioPlayer.GetDuration: Integer;
var
  FrameDuration: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if FZxTuneModule <> nil then
    begin
      FrameDuration := ZXTune_GetDuration(FZxTunePlayer);
      if FrameDuration <= 0 then
        FrameDuration := 20000;

      Result := Round((FModuleInfo.Frames * FrameDuration) / 1000);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TZxTuneAudioPlayer.SetLoopMode(Mode: Boolean);
begin
  if FZxTunePlayer <> nil then
  begin
    if (Mode = True) and (ZXTune_SetPlayerLoopTrack(FZxTunePlayer, 1)) then
      FLoopMode := True
    else if (Mode = False) and (ZXTune_SetPlayerLoopTrack(FZxTunePlayer, 0)) then
      FLoopMode := False;
  end;
end;

function TZxTuneAudioPlayer.GetLoopMode: Boolean;
begin
  if FZxTunePlayer <> nil then
  begin
    if ZXTune_GetPlayerLoopTrack(FZxTunePlayer) > 0 then
      FLoopMode := True
    else
      FLoopMode := False;
  end;
  Result := FLoopMode;
end;

function TZxTuneAudioPlayer.IsPlaying: Boolean;
begin
  Result := (FCurrentPlayer = Self) and not FIsPaused and (FZxTunePlayer <> nil);
end;

function TZxTuneAudioPlayer.IsPaused: Boolean;
begin
  Result := FIsPaused;
end;

function TZxTuneAudioPlayer.IsStopped: Boolean;
begin
  Result := (FZxTunePlayer = nil) or (FCurrentPlayer <> Self) or
            (not IsAudioStreamPlaying(FStream) and not FIsPaused);
end;

function TZxTuneAudioPlayer.GetState: TPlayerState;
begin
  if IsPlaying then
    Result := psPlaying
  else if IsPaused then
    Result := psPaused
  else
    Result := psStopped;
end;

function TZxTuneAudioPlayer.GetCurrentTrack: Integer;
begin
  Result := FCurrentTrack;
end;

function TZxTuneAudioPlayer.GetCurrentFile: String;
begin
  Result := FFilename;
end;

function TZxTuneAudioPlayer.GetTrackCount: Integer;
begin
  Result := 0; // ZXTune обычно обрабатывает однодорожечные модули
end;


procedure TZxTuneAudioPlayer.SetEqualizerBand(BandIndex: Integer; Gain: Single);
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
  begin
    // Всегда сохраняем настройки
    FBands[BandIndex].Gain := Max(-12.0, Min(12.0, Gain));
    CalculateFilterCoefficients(BandIndex);
  end;
end;

function TZxTuneAudioPlayer.GetEqualizerBand(BandIndex: Integer): Single;
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
    Result := FBands[BandIndex].Gain
  else
    Result := 0.0;
end;

procedure TZxTuneAudioPlayer.ResetEqualizer;
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

// IMusicPlayer property getters
function TZxTuneAudioPlayer.GetOnEnd: TEndEvent;
begin
  Result := FOnEnd;
end;

function TZxTuneAudioPlayer.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TZxTuneAudioPlayer.GetOnLoad: TLoadEvent;
begin
  Result := FOnLoad;
end;

function TZxTuneAudioPlayer.GetOnPause: TPauseEvent;
begin
  Result := FOnPause;
end;

function TZxTuneAudioPlayer.GetOnPlay: TPlayEvent;
begin
  Result := FOnPlay;
end;

function TZxTuneAudioPlayer.GetOnStop: TStopEvent;
begin
  Result := FOnStop;
end;

function TZxTuneAudioPlayer.GetPlayerState: TPlayerState;
begin
  Result := GetState;
end;

function TZxTuneAudioPlayer.GetTrackNumber: Integer;
begin
  Result := FCurrentTrack;
end;

procedure TZxTuneAudioPlayer.SetTrackNumber(AValue: Integer);
begin
  FCurrentTrack := AValue;
end;

procedure TZxTuneAudioPlayer.InitializeEqualizer;
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

procedure TZxTuneAudioPlayer.CalculateFilterCoefficients(bandIndex: Integer);
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

function TZxTuneAudioPlayer.ApplyFilter(input: Single; bandIndex,
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

function TZxTuneAudioPlayer.GetBalance: Single;
begin
  FPositionLock.Enter;
  try
    Result := FBalance;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TZxTuneAudioPlayer.SetBalance(AValue: Single);
begin
  FPositionLock.Enter;
  try
    // Ограничиваем значение от -1.0 до +1.0
    FBalance := Max(-1.0, Min(1.0, AValue));
  finally
    FPositionLock.Leave;
  end;
end;

// Event properties setters
procedure TZxTuneAudioPlayer.SetOnEnd(AEvent: TEndEvent);
begin
  FOnEnd := AEvent;
end;

procedure TZxTuneAudioPlayer.SetOnError(AEvent: TErrorEvent);
begin
  FOnError := AEvent;
end;

procedure TZxTuneAudioPlayer.SetOnLoad(AEvent: TLoadEvent);
begin
  FOnLoad := AEvent;
end;

procedure TZxTuneAudioPlayer.SetOnPause(AEvent: TPauseEvent);
begin
  FOnPause := AEvent;
end;

procedure TZxTuneAudioPlayer.SetOnPlay(AEvent: TPlayEvent);
begin
  FOnPlay := AEvent;
end;

procedure TZxTuneAudioPlayer.SetOnStop(AEvent: TStopEvent);
begin
  FOnStop := AEvent;
end;

end.

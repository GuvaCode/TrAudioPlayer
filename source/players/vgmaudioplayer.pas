unit VgmAudioPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, libvgmplay, libraudio,
  rAudioIntf, contnrs, syncobjs, math;

type
  { TVgmAudioPlayer }
  TVgmAudioPlayer = class(TInterfacedObject, IMusicPlayer)
  private
    FStream: TAudioStream;
    FFilename: string;
    FIsPaused: Boolean;
    FLoopMode: Boolean;
    FCurrentTrack: Integer;
    FTrackCount: Integer;
    FPositionLock: TCriticalSection;
    FEqLock: TCriticalSection;
    FTrackEndTriggered: Boolean;
    FVGMHeader: VGM_HEADER;
    FVGMTag: VGM_TAG;

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
    class var FCurrentPlayer: TVgmAudioPlayer;

    class constructor ClassCreate;
    class destructor ClassDestroy;

    procedure InitializeAudioStream;
    procedure ResetPlayback;
    class procedure AudioCallback(bufferData: pointer; frames: LongWord); static; cdecl;
    class procedure AudioProcessEqualizer({%H-}buffer: pointer; {%H-}frames: LongWord); static; cdecl;
    procedure InternalStop(ClearData: Boolean = True);
    procedure CheckError(Condition: Boolean; const Msg: string);
    procedure LoadVGMFile(const MusicFile: string);
    procedure FreeVGMData;
    function IsPlaybackFinished: Boolean;

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

{ TVgmAudioPlayer }

class constructor TVgmAudioPlayer.ClassCreate;
begin
  FPlayers := TFPHashList.Create;
  FCurrentPlayer := nil;
end;

class destructor TVgmAudioPlayer.ClassDestroy;
begin
  FPlayers.Free;
end;

constructor TVgmAudioPlayer.Create;
begin
  inherited Create;
  FTrackEndTriggered := False;
  FIsPaused := False;
  FLoopMode := False;
  FCurrentTrack := 0;
  FTrackCount := 0; // VGM файлы обычно содержат один трек
  FPositionLock := TCriticalSection.Create;
  FEqLock := TCriticalSection.Create;

  FBalance := 0.0;
  // Инициализируем структуры VGM
  FillChar(FVGMHeader, SizeOf(VGM_HEADER), 0);
  FillChar(FVGMTag, SizeOf(VGM_TAG), 0);

  InitializeAudioStream;
  InitializeEqualizer;
end;

destructor TVgmAudioPlayer.Destroy;
begin
  InternalStop;
  FreeVGMData;
  FPositionLock.Free;
  DetachAudioStreamProcessor(FStream, @AudioProcessEqualizer);
  SetAudioStreamCallback(FStream, nil);
  FEqLock.Free;
  inherited Destroy;
end;

procedure TVgmAudioPlayer.InitializeAudioStream;
begin
  SetAudioStreamBufferSizeDefault(BUFFER_SIZE);
  FStream := LoadAudioStream(DEFAULT_FREQ, DEFAULT_BITS, DEFAULT_CHANNELS);
  if not IsAudioStreamReady(FStream) then
    raise Exception.Create('Failed to initialize audio stream');

  FPlayers.Add(IntToStr(PtrInt(Self)), Self);
  SetAudioStreamCallback(FStream, @AudioCallback);
  AttachAudioStreamProcessor(FStream, @AudioProcessEqualizer);
end;

procedure TVgmAudioPlayer.LoadVGMFile(const MusicFile: string);
var
  FileName: String;
  ResultCode: Integer;
begin
    // StopAudioStream(FStream);
    FreeVGMData;
      // Инициализируем воспроизведение
    VGMPlay_Init;
    VGMPlay_Init2;
  try
    // Проверяем, загружена ли библиотека VGMPlay
    if not VGMLoaded then
      begin
      if Assigned(FOnError) then
      begin
        FOnError(self,'VGMPlay: library not loaded');
        Exit;
      end else
        raise Exception.Create('VGMPlay library not loaded');
      end;


    // Открываем VGM файл
    FileName := MusicFile;
    if not OpenVGMFile(PChar(FileName)) then
    begin
      if Assigned(FOnError) then
      begin
      FOnError(self,'VGMPlay: Failed to open VGM file');
      Exit;
      end
      else
        raise Exception.Create('Failed to open VGM file');
    end else
    if Assigned(FOnLoad) then
      FOnLoad(Self, MusicFile, 0); //  обычно однодорожечные модули


    PlayVGM();

    // Получаем информацию о файле

    ResultCode := GetVGMFileInfo(PChar(FileName), @FVGMHeader, FVGMTag);


    if ResultCode = 0 then
    begin
      if Assigned(FOnError) then
      begin
      FOnError(self,'Failed to get VGM file info');
      Exit;
       end else
      raise Exception.Create('Failed to get VGM file info');
    end;

    // Устанавливаем режим зацикливания
   if FLoopMode then
     RefreshPlaybackOptions; // VGMPlay автоматически обрабатывает зацикливание

   FFilename := MusicFile;
  except
    FreeVGMData;
    raise;
  end;
end;

procedure TVgmAudioPlayer.FreeVGMData;
begin
  if VGMLoaded then
  begin
    StopVGM;
    CloseVGMFile;

    // Освобождаем GD3 тег, если он был выделен
    if FVGMTag.fccGD3 = FCC_GD3 then
      FreeGD3Tag(@FVGMTag);

    VGMPlay_Deinit;
    sleep(50);
  end;

  FillChar(FVGMHeader, SizeOf(VGM_HEADER), 0);
  FillChar(FVGMTag, SizeOf(VGM_TAG), 0);
  FTrackCount := 0;
end;

function TVgmAudioPlayer.IsPlaybackFinished: Boolean;
var
  CurrentPos, DataLength: UInt32;
  SamplesPlayed, TotalSamples: UInt32;
begin
  Result := False;

  if not VGMLoaded then Exit(True);

  // Проверка 1: через флаг завершения библиотеки
  if IsVGMPlayEnded() then Exit(True);

  // Проверка 2: позиция в данных
  CurrentPos := GetVGMPos();
  DataLength := GetVGMDataLen();
  if CurrentPos >= DataLength then Exit(True);

  // Проверка 3: сэмплы
  SamplesPlayed := GetVGMSamplesPlayed();
  TotalSamples := GetVGMTotalSamples();
  if (TotalSamples > 0) and (SamplesPlayed >= TotalSamples) then Exit(True);

  // Проверка 4: fadeout завершен
  if (IsFadePlay()) and (GetMasterVolume() <= 0.0) then Exit(True);
end;

procedure TVgmAudioPlayer.ResetPlayback;
begin
  if VGMLoaded then
  begin
    SeekVGM(False, 0); // Перемещаемся в начало
  end;
end;

class procedure TVgmAudioPlayer.AudioCallback(bufferData: pointer; frames: LongWord); cdecl;
var
  BytesRendered: Integer;
  LocalPlayer: TVgmAudioPlayer;
  ShouldStop: Boolean;
begin
  if FCurrentPlayer = nil then Exit;
  BytesRendered := 0;
  LocalPlayer := FCurrentPlayer;
  ShouldStop := False;

  with LocalPlayer do
  begin
    FPositionLock.Enter;
    try
      if not VGMLoaded or FIsPaused then
      begin
        FillChar(bufferData^, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8), 0);
        Exit;
      end;

      // Рендерим аудио через VGMPlay
      BytesRendered := FillBuffer(bufferData, frames);

      if (IsPlaybackFinished) then
      begin
        // Конец трека или ошибка
        if Assigned(FOnEnd) and (not FLoopMode)  then
        begin
          FOnEnd(LocalPlayer, FCurrentTrack, True);
          FTrackEndTriggered := True;
        end;

        if FLoopMode then
        begin
          ResetPlayback;
          FTrackEndTriggered := False;
        end
        else
        begin
          FillChar(bufferData^, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8), 0);
          ShouldStop := FTrackEndTriggered;
        end;
      end
      else
      begin
        // TTF analysis только если есть данные
        //AnalyzeAudioBuffer(bufferData, BytesRendered);
      end;
    finally
      FPositionLock.Leave;
    end;

    // Вызов остановки ВНЕ блокировки
    if ShouldStop then
      InternalStop(True);
  end;
end;

  class procedure TVgmAudioPlayer.AudioProcessEqualizer(buffer: pointer;
    frames: LongWord); cdecl;
  var
    Current: TVgmAudioPlayer;
    BufferData: PSingle;
    frame, band: LongWord;
    leftGain, rightGain: Single;
    inputLeft, inputRight, outputLeft, outputRight: Single;
  begin
    // Быстрый выход, если ничего не проигрывается
    if (FCurrentPlayer = nil) or (frames = 0) or (buffer = nil) then
      Exit;



    if Current = nil then
      Exit;

    // Проверка: не вышли ли мы за пределы BANDS_COUNT
    if BANDS_COUNT <= 0 then
      Exit;

    FCurrentPlayer.FEqLock.Enter;
     try

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

     finally
       Current.FEqLock.Leave;
   end;

end;

procedure TVgmAudioPlayer.CheckError(Condition: Boolean; const Msg: string);
begin
  if Condition and Assigned(FOnError) then
    FOnError(Self, Msg);
end;

procedure TVgmAudioPlayer.InternalStop(ClearData: Boolean);
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

      if ClearData then
        FreeVGMData;

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
procedure TVgmAudioPlayer.OpenMusicFile(const MusicFile: String);
begin
  if not FileExists(MusicFile) then
  begin
    CheckError(True, 'File not found: ' + MusicFile);
    Exit;
  end;

  FPositionLock.Enter;
  try
    // Останавливаем текущее воспроизведение
    if IsAudioStreamPlaying(FStream) then
      InternalStop;

    // Загружаем новый VGM файл
    try
      LoadVGMFile(MusicFile);
      FCurrentTrack := 0;
    except
      on E: Exception do
      begin
        CheckError(True, 'Error loading VGM file: ' + E.Message);
        InternalStop;
      end;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TVgmAudioPlayer.Play;
begin
  FPositionLock.Enter;
  try
    if VGMLoaded and not IsAudioStreamPlaying(FStream) then
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

procedure TVgmAudioPlayer.Pause;
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

procedure TVgmAudioPlayer.Resume;
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

procedure TVgmAudioPlayer.Stop;
begin
  InternalStop(False);
  SetPosition(0);
end;

procedure TVgmAudioPlayer.SetPosition(PositionMs: Integer);
var
  Samples: UInt32;
  SampleRate: UInt32;
begin
  FPositionLock.Enter;
  try
    if VGMLoaded then
    begin
      SampleRate := GetVGMSampleRate();
      if SampleRate > 0 then
      begin
        Samples := Round((PositionMs / 1000) * SampleRate);
        SeekVGM(False, Samples);
      end;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TVgmAudioPlayer.GetPosition: Integer;
var
  Samples: UInt32;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if VGMLoaded then
    begin
      Samples := GetVGMSamplesPlayed();
      Result := Round((Samples / GetVGMSampleRate()) * 1000);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TVgmAudioPlayer.GetDuration: Integer;
var
  TotalSamples, SampleRate: UInt32;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if VGMLoaded then
    begin
      TotalSamples := GetVGMTotalSamples();
      SampleRate := GetVGMSampleRate();
      if SampleRate > 0 then
        Result := Round((TotalSamples / SampleRate) * 1000)
      else
        Result := 180000; // 3 минуты по умолчанию
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TVgmAudioPlayer.SetLoopMode(Mode: Boolean);
begin
  FLoopMode := Mode;
  FPositionLock.Enter;
  try
    if VGMLoaded then
    begin
      RefreshPlaybackOptions; // Обновляем настройки воспроизведения
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TVgmAudioPlayer.GetLoopMode: Boolean;
begin
  Result := FLoopMode;
end;

function TVgmAudioPlayer.IsPlaying: Boolean;
begin
  Result := (FCurrentPlayer = Self) and not FIsPaused and VGMLoaded;
end;

function TVgmAudioPlayer.IsPaused: Boolean;
begin
  Result := FIsPaused;
end;

function TVgmAudioPlayer.IsStopped: Boolean;
begin
  Result := (not VGMLoaded) or (FCurrentPlayer <> Self) or
            (not IsAudioStreamPlaying(FStream) and not FIsPaused);
end;

function TVgmAudioPlayer.GetState: TPlayerState;
begin
  if IsPlaying then
    Result := psPlaying
  else if IsPaused then
    Result := psPaused
  else
    Result := psStopped;
end;

function TVgmAudioPlayer.GetCurrentTrack: Integer;
begin
  Result := FCurrentTrack;
end;

function TVgmAudioPlayer.GetCurrentFile: String;
begin
  Result := FFilename;
end;

function TVgmAudioPlayer.GetTrackCount: Integer;
begin
  Result := FTrackCount; // VGM файлы обычно содержат 1 трек
end;

procedure TVgmAudioPlayer.SetEqualizerBand(BandIndex: Integer; Gain: Single);
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
  begin
    // Всегда сохраняем настройки
    FBands[BandIndex].Gain := Max(-12.0, Min(12.0, Gain));
    CalculateFilterCoefficients(BandIndex);
  end;
end;

function TVgmAudioPlayer.GetEqualizerBand(BandIndex: Integer): Single;
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
    Result := FBands[BandIndex].Gain
  else
    Result := 0.0;
end;

procedure TVgmAudioPlayer.ResetEqualizer;
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
///  FillChar(FFilterHistory, SizeOf(FFilterHistory), 0);

  // Расчет начальных коэффициентов
  for i := 0 to BANDS_COUNT - 1 do
    CalculateFilterCoefficients(i);
end;

function TVgmAudioPlayer.GetBalance: Single;
begin
  FPositionLock.Enter;
  try
    Result := FBalance;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TVgmAudioPlayer.SetBalance(AValue: Single);
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
function TVgmAudioPlayer.GetOnEnd: TEndEvent;
begin
  Result := FOnEnd;
end;

function TVgmAudioPlayer.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TVgmAudioPlayer.GetOnLoad: TLoadEvent;
begin
  Result := FOnLoad;
end;

function TVgmAudioPlayer.GetOnPause: TPauseEvent;
begin
  Result := FOnPause;
end;

function TVgmAudioPlayer.GetOnPlay: TPlayEvent;
begin
  Result := FOnPlay;
end;

function TVgmAudioPlayer.GetOnStop: TStopEvent;
begin
  Result := FOnStop;
end;

function TVgmAudioPlayer.GetPlayerState: TPlayerState;
begin
  Result := GetState;
end;

function TVgmAudioPlayer.GetTrackNumber: Integer;
begin
  Result := FCurrentTrack;
end;

procedure TVgmAudioPlayer.SetTrackNumber(AValue: Integer);
begin
  FCurrentTrack := AValue;
end;


procedure TVgmAudioPlayer.InitializeEqualizer;
var
  i: Integer;
  freq: Single;
begin
  FEqLock.Enter;
  try
    freq := 32.0;
    for i := 0 to BANDS_COUNT - 1 do
    begin
      FBands[i].Frequency := freq;
      FBands[i].Gain := 0.0;
      FBands[i].Q := 1.0;
      freq := freq * 2.0;
      CalculateFilterCoefficients(i);
    end;
    FillChar(FFilterHistory, SizeOf(FFilterHistory), 0);
  finally
    FEqLock.Leave;
  end;
end;

procedure TVgmAudioPlayer.CalculateFilterCoefficients(bandIndex: Integer);
var
  band: TEqualizerBand;
  A, w0, alpha: Single;
  cosW0, sinW0: Single;
  coeffs: TFilterCoeffs;
begin
  band := FBands[bandIndex];

  // Преобразование dB в линейное значение
  A := Power(10.0, band.Gain / 40.0);
  w0 := 2.0 * PI * band.Frequency / 44100;
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

function TVgmAudioPlayer.ApplyFilter(input: Single; bandIndex, channel: Integer): Single;
var
  coeffs: TFilterCoeffs;
  output: Single;
begin
  // ИСПРАВЛЕННАЯ проверка границ
  if (bandIndex < 0) or (bandIndex >= BANDS_COUNT) or
     (channel < 0) or (channel > 1) then
  begin
    Result := input; // Возвращаем исходный сигнал при ошибке
    Exit;
  end;

  // Безопасное получение коэффициентов
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
procedure TVgmAudioPlayer.SetOnEnd(AEvent: TEndEvent);
begin
  FOnEnd := AEvent;
end;

procedure TVgmAudioPlayer.SetOnError(AEvent: TErrorEvent);
begin
  FOnError := AEvent;
end;

procedure TVgmAudioPlayer.SetOnLoad(AEvent: TLoadEvent);
begin
  FOnLoad := AEvent;
end;

procedure TVgmAudioPlayer.SetOnPause(AEvent: TPauseEvent);
begin
  FOnPause := AEvent;
end;

procedure TVgmAudioPlayer.SetOnPlay(AEvent: TPlayEvent);
begin
  FOnPlay := AEvent;
end;

procedure TVgmAudioPlayer.SetOnStop(AEvent: TStopEvent);
begin
  FOnStop := AEvent;
end;

end.

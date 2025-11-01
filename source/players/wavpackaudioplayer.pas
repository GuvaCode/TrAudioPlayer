unit WavPackAudioPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, libwavpack, libraudio,
  rAudioIntf,rAudioPlayer, contnrs, syncobjs, math;

type
  { TWavPackAudioPlayer }
  TWavPackAudioPlayer = class(TInterfacedObject, IMusicPlayer)
  private
    FStream: TAudioStream;
    FFilename: string;
    FIsPaused: Boolean;
    FLoopMode: Boolean;
    FCurrentTrack: Integer;
    FTrackCount: Integer;
    FPositionLock: TCriticalSection;
    FTrackEndTriggered: Boolean;
    FWavPackContext: WavpackContext;
    FSampleRate: Integer;
    FChannels: Integer;
    FBitsPerSample: Integer;
    {$if defined(cpu64)}
    FTotalSamples: int64_t;
    FCurrentSample: int64_t;
    {$else}
    FTotalSamples: uint32_t;
    FCurrentSample: uint32_t;
    {$endif}
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
    class var FCurrentPlayer: TWavPackAudioPlayer;

    class constructor ClassCreate;
    class destructor ClassDestroy;

    procedure InitializeAudioStream;
    procedure ResetPlayback;
    class procedure AudioCallback(bufferData: pointer; frames: LongWord); static; cdecl;
    class procedure AudioProcessEqualizer({%H-}buffer: pointer; {%H-}frames: LongWord); static; cdecl;
    procedure InternalStop(ClearData: Boolean = True);
    procedure CheckError(Condition: Boolean; const Msg: string);
    procedure LoadWavPackFile(const MusicFile: string);
    procedure FreeWavPackData;
    function IsPlaybackFinished: Boolean;
    function RenderSamples(buffer: Pointer; frames: LongWord): LongWord;

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

{ TWavPackAudioPlayer }

class constructor TWavPackAudioPlayer.ClassCreate;
begin
  FPlayers := TFPHashList.Create;
  FCurrentPlayer := nil;
end;

class destructor TWavPackAudioPlayer.ClassDestroy;
begin
  FPlayers.Free;
end;

constructor TWavPackAudioPlayer.Create;
begin
  inherited Create;
  FTrackEndTriggered := False;
  FIsPaused := False;
  FLoopMode := False;
  FCurrentTrack := 0;
  FTrackCount := 1; // WavPack файлы обычно содержат один трек
  FPositionLock := TCriticalSection.Create;
  
  FWavPackContext := nil;
  FSampleRate := DEFAULT_FREQ;
  FChannels := DEFAULT_CHANNELS;
  FBitsPerSample := DEFAULT_BITS;
  FTotalSamples := 0;
  FCurrentSample := 0;
  FBalance := 0.0;

  InitializeAudioStream;
  InitializeEqualizer;
end;

destructor TWavPackAudioPlayer.Destroy;
begin
  InternalStop;
  FreeWavPackData;
  FPositionLock.Free;
  inherited Destroy;
end;

procedure TWavPackAudioPlayer.InitializeAudioStream;
begin
  SetAudioStreamBufferSizeDefault(BUFFER_SIZE);
  FStream := LoadAudioStream(DEFAULT_FREQ, DEFAULT_BITS, DEFAULT_CHANNELS);
  if not IsAudioStreamReady(FStream) then
    raise Exception.Create('Failed to initialize audio stream');

  FPlayers.Add(IntToStr(PtrInt(Self)), Self);
  SetAudioStreamCallback(FStream, @AudioCallback);
  AttachAudioStreamProcessor(FStream, @AudioProcessEqualizer);
end;

procedure TWavPackAudioPlayer.LoadWavPackFile(const MusicFile: string);
var
  ErrorMsg: array[0..255] of AnsiChar;
  ModeFlags: Integer;
begin
  FreeWavPackData;

  try
    // Проверяем, загружена ли библиотека WavPack
    if not WavPackLoaded then
    begin
      if Assigned(FOnError) then
      begin
        FOnError(Self, 'WavPack: library not loaded');
        Exit;
      end
      else
        raise Exception.Create('WavPack library not loaded');
    end;

    // Открываем WavPack файл с правильными флагами
    FillChar(ErrorMsg{%H-}, SizeOf(ErrorMsg), 0);
    FWavPackContext := WavpackOpenFileInput(PAnsiChar(MusicFile), ErrorMsg,
      OPEN_NORMALIZE or OPEN_2CH_MAX, 0); // OPEN_2CH_MAX для безопасности, лучше перебздеть.

    // FWavPackContext := WavpackOpenFileInput(PAnsiChar(MusicFile), ErrorMsg, OPEN_NORMALIZE, 0);

    if FWavPackContext = nil then
    begin
      if Assigned(FOnError) then
      begin
        FOnError(Self, 'WavPack: Failed to open file: ' + string(ErrorMsg));
        Exit;
      end
      else
        raise Exception.Create('Failed to open WavPack file: ' + string(ErrorMsg));
    end;

    // Получаем информацию о файле
    FSampleRate := WavpackGetSampleRate(FWavPackContext);
    FChannels := WavpackGetNumChannels(FWavPackContext);
    FBitsPerSample := WavpackGetBitsPerSample(FWavPackContext);
    {$if defined(cpu64)}
    FTotalSamples := WavpackGetNumSamples64(FWavPackContext);
    {$else}
    FTotalSamples := WavpackGetNumSamples(FWavPackContext);
    {$endif}
    FCurrentSample := 0;

    // Проверяем корректность данных
    if (FSampleRate = 0) or (FChannels = 0) or (FBitsPerSample = 0) then
    begin
      if Assigned(FOnError) then
      begin
        FOnError(Self, 'WavPack: Invalid audio data in file');
        Exit;
      end
      else
        raise Exception.Create('Invalid WavPack audio data');
    end;

    // Ограничиваем количество каналов на всяки случай
    if FChannels > 2 then
      FChannels := 2;

    // Проверяем режим файла
    ModeFlags := WavpackGetMode(FWavPackContext);

    // Логируем информацию о файле для отладки
    if Assigned(FOnLoad) then
      FOnLoad(Self, Format('WavPack: %dHz, %dch, %dbits, Mode: 0x%x',
        [FSampleRate, FChannels, FBitsPerSample, ModeFlags]), 0);

    FFilename := MusicFile;

  except
    FreeWavPackData;
    raise;
  end;
end;

procedure TWavPackAudioPlayer.FreeWavPackData;
begin
  if (FWavPackContext <> nil) and WavPackLoaded then
  begin
    WavpackCloseFile(FWavPackContext);
    FWavPackContext := nil;
  end;

  FSampleRate := DEFAULT_FREQ;
  FChannels := DEFAULT_CHANNELS;
  FBitsPerSample := DEFAULT_BITS;
  FTotalSamples := 0;
  FCurrentSample := 0;
  FTrackCount := 0;
end;

function TWavPackAudioPlayer.IsPlaybackFinished: Boolean;
begin
  Result := False;
  
  if (FWavPackContext = nil) or not WavPackLoaded then
    Exit(True);

  // Проверяем, достигли ли конца файла
  if FTotalSamples > 0 then
    Result := FCurrentSample >= FTotalSamples
  else
    // Если общее количество сэмплов неизвестно, проверяем через позицию
    {$if defined(cpu64)}
    Result := WavpackGetSampleIndex64(FWavPackContext) >= FTotalSamples;
    {$else}
    Result := WavpackGetSampleIndex(FWavPackContext) >= FTotalSamples;
    {$endif}
end;

function TWavPackAudioPlayer.RenderSamples(buffer: Pointer; frames: LongWord): LongWord;
var
  SamplesToRead: LongWord;
  SamplesRead: LongWord;
  TempBuffer: array of Int32;
  OutputBuffer: array of Int16;
  SampleValue, i: Integer;
begin
  Result := 0;

  if (FWavPackContext = nil) or (frames = 0) then
    Exit;

  // Вычисляем количество сэмплов для чтения (в сэмплах на канал)
  SamplesToRead := frames * FChannels;

  // Создаем временный буфер для декодированных сэмплов (32-битные)
  SetLength(TempBuffer{%H-}, SamplesToRead);

  try
    // Декодируем сэмплы через WavPack
    SamplesRead := WavpackUnpackSamples(FWavPackContext, @TempBuffer[0], frames);

    if SamplesRead > 0 then
    begin
      // Конвертируем 32-битные сэмплы в 16-битные для аудиопотока
      SetLength(OutputBuffer{%H-}, SamplesRead * FChannels);

      for i := 0 to (SamplesRead * FChannels) - 1 do
      begin
        // Масштабируем 32-битный сэмпл в 16-битный диапазон
        SampleValue := TempBuffer[i];

        // Ограничиваем значение до 16-битного диапазона
        if SampleValue > 32767 then
          SampleValue := 32767
        else if SampleValue < -32768 then
          SampleValue := -32768;

        OutputBuffer[i] := Int16(SampleValue);
      end;

      // Копируем преобразованные сэмплы в выходной буфер
      Move(OutputBuffer[0], buffer^, SamplesRead * FChannels * SizeOf(Int16));

      // Обновляем текущую позицию
      {$if defined(cpu64)}
      FCurrentSample := WavpackGetSampleIndex64(FWavPackContext);
      {$else}
      FCurrentSample := WavpackGetSampleIndex(FWavPackContext);
      {$endif}

      Result := SamplesRead;
    end
    else
    begin
      // Не удалось прочитать сэмплы - конец файла или ошибка
      FillChar(buffer^, frames * FChannels * SizeOf(Int16), 0);
    end;

  finally
    SetLength(TempBuffer, 0);
    SetLength(OutputBuffer, 0);
  end;
end;

procedure TWavPackAudioPlayer.ResetPlayback;
begin
  if (FWavPackContext <> nil) and WavPackLoaded then
  begin
    // Перемещаемся в начало файла
    {$if defined(cpu64)}
    WavpackSeekSample64(FWavPackContext, 0);
    {$else}
    WavpackSeekSample(FWavPackContext, 0);
    {$endif}

    FCurrentSample := 0;
  end;
end;

class procedure TWavPackAudioPlayer.AudioCallback(bufferData: pointer; frames: LongWord); cdecl;
var
  BytesRendered: LongWord;
  LocalPlayer: TWavPackAudioPlayer;
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
      if (FWavPackContext = nil) or FIsPaused or not WavPackLoaded then
      begin
        FillChar(bufferData^, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8), 0);
        Exit;
      end;

      // Рендерим аудио через WavPack
      BytesRendered := RenderSamples(bufferData, frames);

      if (BytesRendered = 0) or IsPlaybackFinished then
      begin
        // Конец трека или ошибка
        if Assigned(FOnEnd) and (not FLoopMode) then
        begin
          FOnEnd(LocalPlayer, FCurrentTrack, True);
          FTrackEndTriggered := True;
        end;

        if FLoopMode then
        begin
          ResetPlayback;
          FTrackEndTriggered := False;
          // Повторно рендерим сэмплы после сброса
          BytesRendered := RenderSamples(bufferData, frames);
        end
        else
        begin
          FillChar(bufferData^, frames * DEFAULT_CHANNELS * (DEFAULT_BITS div 8), 0);
          ShouldStop := FTrackEndTriggered;
        end;
      end;

    finally
      FPositionLock.Leave;
    end;

    // Вызов остановки ВНЕ блокировки
    if ShouldStop then
      InternalStop(True);
  end;
end;

class procedure TWavPackAudioPlayer.AudioProcessEqualizer(buffer: pointer;
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

procedure TWavPackAudioPlayer.CheckError(Condition: Boolean; const Msg: string);
begin
  if Condition and Assigned(FOnError) then
    FOnError(Self, Msg);
end;

procedure TWavPackAudioPlayer.InternalStop(ClearData: Boolean);
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
        FreeWavPackData;

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
procedure TWavPackAudioPlayer.OpenMusicFile(const MusicFile: String);
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

    // Загружаем новый WavPack файл
    try
      LoadWavPackFile(MusicFile);
      FCurrentTrack := 0;
    except
      on E: Exception do
      begin
        CheckError(True, 'Error loading WavPack file: ' + E.Message);
        InternalStop;
      end;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TWavPackAudioPlayer.Play;
begin
  FPositionLock.Enter;
  try
    if (FWavPackContext <> nil) and not IsAudioStreamPlaying(FStream) then
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

procedure TWavPackAudioPlayer.Pause;
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

procedure TWavPackAudioPlayer.Resume;
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

procedure TWavPackAudioPlayer.Stop;
begin
 InternalStop(False);
 SetPosition(0);
end;

procedure TWavPackAudioPlayer.SetPosition(PositionMs: Integer);
var
  {$if defined(cpu64)}
  TargetSample: int64_t;
  {$else}
  TargetSample: uint32_t;
  {$endif}

begin
  FPositionLock.Enter;
  try
    if (FWavPackContext <> nil) and WavPackLoaded then
    begin
      if FSampleRate > 0 then
      begin
        TargetSample := Round((PositionMs / 1000) * FSampleRate);
        if TargetSample < 0 then TargetSample := 0;
        if (FTotalSamples > 0) and (TargetSample > FTotalSamples) then
          TargetSample := FTotalSamples;
        {$if defined(cpu64)}  
        if WavpackSeekSample64(FWavPackContext, TargetSample) <> 0 then
        {$else}
        if WavpackSeekSample(FWavPackContext, TargetSample) <> 0 then
        {$endif}
        begin
          FCurrentSample := TargetSample;
        end
        else
        begin
          CheckError(True, 'Failed to seek in WavPack file');
        end;
      end;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TWavPackAudioPlayer.GetPosition: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if (FWavPackContext <> nil) and WavPackLoaded then
    begin
      if FSampleRate > 0 then
        Result := Round((FCurrentSample / FSampleRate) * 1000);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TWavPackAudioPlayer.GetDuration: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if (FWavPackContext <> nil) and WavPackLoaded then
    begin
      if FSampleRate > 0 then
        Result := Round((FTotalSamples / FSampleRate) * 1000)
      else
        Result := 0;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TWavPackAudioPlayer.SetLoopMode(Mode: Boolean);
begin
  FLoopMode := Mode;
end;

function TWavPackAudioPlayer.GetLoopMode: Boolean;
begin
  Result := FLoopMode;
end;

function TWavPackAudioPlayer.IsPlaying: Boolean;
begin
  Result := (FCurrentPlayer = Self) and not FIsPaused and 
            (FWavPackContext <> nil) and WavPackLoaded;
end;

function TWavPackAudioPlayer.IsPaused: Boolean;
begin
  Result := FIsPaused;
end;

function TWavPackAudioPlayer.IsStopped: Boolean;
begin
  Result := (FWavPackContext = nil) or (FCurrentPlayer <> Self) or
            (not IsAudioStreamPlaying(FStream) and not FIsPaused);
end;

function TWavPackAudioPlayer.GetState: TPlayerState;
begin
  if IsPlaying then
    Result := psPlaying
  else if IsPaused then
    Result := psPaused
  else
    Result := psStopped;
end;

function TWavPackAudioPlayer.GetCurrentTrack: Integer;
begin
  Result := FCurrentTrack;
end;

function TWavPackAudioPlayer.GetCurrentFile: String;
begin
  Result := FFilename;
end;

function TWavPackAudioPlayer.GetTrackCount: Integer;
begin
  Result := FTrackCount; // WavPack файлы обычно содержат 1 трек
end;

procedure TWavPackAudioPlayer.SetEqualizerBand(BandIndex: Integer; Gain: Single
  );
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
  begin
    // Всегда сохраняем настройки
    FBands[BandIndex].Gain := Max(-12.0, Min(12.0, Gain));
    CalculateFilterCoefficients(BandIndex);
  end;
end;

function TWavPackAudioPlayer.GetEqualizerBand(BandIndex: Integer): Single;
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
    Result := FBands[BandIndex].Gain
  else
    Result := 0.0;
end;

procedure TWavPackAudioPlayer.ResetEqualizer;
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

function TWavPackAudioPlayer.GetBalance: Single;
begin
  FPositionLock.Enter;
  try
    Result := FBalance;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TWavPackAudioPlayer.SetBalance(AValue: Single);
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
function TWavPackAudioPlayer.GetOnEnd: TEndEvent;
begin
  Result := FOnEnd;
end;

function TWavPackAudioPlayer.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TWavPackAudioPlayer.GetOnLoad: TLoadEvent;
begin
  Result := FOnLoad;
end;

function TWavPackAudioPlayer.GetOnPause: TPauseEvent;
begin
  Result := FOnPause;
end;

function TWavPackAudioPlayer.GetOnPlay: TPlayEvent;
begin
  Result := FOnPlay;
end;

function TWavPackAudioPlayer.GetOnStop: TStopEvent;
begin
  Result := FOnStop;
end;

function TWavPackAudioPlayer.GetPlayerState: TPlayerState;
begin
  Result := GetState;
end;

function TWavPackAudioPlayer.GetTrackNumber: Integer;
begin
  Result := FCurrentTrack;
end;

procedure TWavPackAudioPlayer.SetTrackNumber(AValue: Integer);
begin
  FCurrentTrack := AValue;
end;

procedure TWavPackAudioPlayer.InitializeEqualizer;
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

procedure TWavPackAudioPlayer.CalculateFilterCoefficients(bandIndex: Integer);
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

function TWavPackAudioPlayer.ApplyFilter(input: Single; bandIndex,
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
procedure TWavPackAudioPlayer.SetOnEnd(AEvent: TEndEvent);
begin
  FOnEnd := AEvent;
end;

procedure TWavPackAudioPlayer.SetOnError(AEvent: TErrorEvent);
begin
  FOnError := AEvent;
end;

procedure TWavPackAudioPlayer.SetOnLoad(AEvent: TLoadEvent);
begin
  FOnLoad := AEvent;
end;

procedure TWavPackAudioPlayer.SetOnPause(AEvent: TPauseEvent);
begin
  FOnPause := AEvent;
end;

procedure TWavPackAudioPlayer.SetOnPlay(AEvent: TPlayEvent);
begin
  FOnPlay := AEvent;
end;

procedure TWavPackAudioPlayer.SetOnStop(AEvent: TStopEvent);
begin
  FOnStop := AEvent;
end;

end.

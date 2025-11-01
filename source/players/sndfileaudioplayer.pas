unit SndFileAudioPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, libsndfile, libraudio,
  rAudioIntf, contnrs, syncobjs, math;

type
  { TSndFileAudioPlayer }
  TSndFileAudioPlayer = class(TInterfacedObject, IMusicPlayer)
  private
    FStream: TAudioStream;
    FFilename: string;
    FIsPaused: Boolean;
    FLoopMode: Boolean;
    FCurrentTrack: Integer;
    FTrackCount: Integer;
    FPositionLock: TCriticalSection;
    FTrackEndTriggered: Boolean;
    FSndFile: TSNDFILE_HANDLE;
    FFileInfo: TSF_INFO;
    FSampleRate: Integer;
    FChannels: Integer;
    FBitsPerSample: Integer;
    FTotalSamples: Int64;
    FCurrentSample: Int64;

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
    class var FCurrentPlayer: TSndFileAudioPlayer;

    class constructor ClassCreate;
    class destructor ClassDestroy;

    procedure InitializeAudioStream;
    procedure ResetPlayback;
    class procedure AudioCallback(bufferData: pointer; frames: LongWord); static; cdecl;
    class procedure AudioProcessEqualizer({%H-}buffer: pointer; {%H-}frames: LongWord); static; cdecl;
    procedure InternalStop(ClearData: Boolean = True);
    procedure CheckError(Condition: Boolean; const Msg: string);
    procedure LoadSndFile(const MusicFile: string);
    procedure FreeSndFileData;
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

{ TSndFileAudioPlayer }

class constructor TSndFileAudioPlayer.ClassCreate;
begin
  FPlayers := TFPHashList.Create;
  FCurrentPlayer := nil;
end;

class destructor TSndFileAudioPlayer.ClassDestroy;
begin
  FPlayers.Free;
end;

constructor TSndFileAudioPlayer.Create;
begin
  inherited Create;
  FTrackEndTriggered := False;
  FIsPaused := False;
  FLoopMode := False;
  FCurrentTrack := 0;
  FTrackCount := 1;
  FPositionLock := TCriticalSection.Create;
  
  FSndFile := nil;
  FSampleRate := DEFAULT_FREQ;
  FChannels := DEFAULT_CHANNELS;
  FBitsPerSample := DEFAULT_BITS;
  FTotalSamples := 0;
  FCurrentSample := 0;
  FBalance := 0.0;
  InitializeAudioStream;
  InitializeEqualizer;
end;

destructor TSndFileAudioPlayer.Destroy;
begin
  InternalStop;
  FreeSndFileData;
  FPositionLock.Free;
  inherited Destroy;
end;

procedure TSndFileAudioPlayer.InitializeAudioStream;
begin
  SetAudioStreamBufferSizeDefault(BUFFER_SIZE);
  FStream := LoadAudioStream(DEFAULT_FREQ, DEFAULT_BITS, DEFAULT_CHANNELS);
  if not IsAudioStreamReady(FStream) then
    raise Exception.Create('Failed to initialize audio stream');

  FPlayers.Add(IntToStr(PtrInt(Self)), Self);
  SetAudioStreamCallback(FStream, @AudioCallback);
  AttachAudioStreamProcessor(FStream, @AudioProcessEqualizer);
end;

procedure TSndFileAudioPlayer.LoadSndFile(const MusicFile: string);
var
  FormatInfo: string;
begin
  FreeSndFileData;
  try
    // Проверяем, загружена ли библиотека libsndfile
    if not sf_IsLoaded then
    begin
      if Assigned(FOnError) then
      begin
        FOnError(Self, 'libsndfile: library not loaded');
        Exit;
      end
      else
        raise Exception.Create('libsndfile library not loaded');
    end;

    // Открываем файл через libsndfile
    FillChar(FFileInfo{%H-}, SizeOf(FFileInfo), 0);
    FSndFile := sf_open(MusicFile, SFM_READ, FFileInfo);

    if FSndFile = nil then
    begin
      if Assigned(FOnError) then
      begin
        FOnError(Self, 'libsndfile: Failed to open file: ' + string(sf_strerror(nil)));
        Exit;
      end
      else
        raise Exception.Create('Failed to open audio file: ' + string(sf_strerror(nil)));
    end;

    // Получаем информацию о файле
    FSampleRate := FFileInfo.samplerate;
    FChannels := FFileInfo.channels;
    
    // Определяем битность на основе формата
    case (FFileInfo.format and SF_FORMAT_SUBMASK) of
      SF_FORMAT_PCM_S8: FBitsPerSample := 8;
      SF_FORMAT_PCM_U8: FBitsPerSample := 8;
      SF_FORMAT_PCM_16: FBitsPerSample := 16;
      SF_FORMAT_PCM_24: FBitsPerSample := 16;
      SF_FORMAT_PCM_32: FBitsPerSample := 32;
      SF_FORMAT_FLOAT: FBitsPerSample := 32;
      SF_FORMAT_DOUBLE: FBitsPerSample := 64;
    else
      FBitsPerSample := 16; // По умолчанию
    end;

    FBitsPerSample := 16; // По умолчанию
    UnloadAudioStream(FStream);
    FStream := LoadAudioStream(FSampleRate, FBitsPerSample, FChannels);

    SetAudioStreamCallback(FStream, @AudioCallback);
    AttachAudioStreamProcessor(FStream, @AudioProcessEqualizer);

    FTotalSamples := FFileInfo.frames;
    FCurrentSample := 0;

    // Проверяем корректность данных
    if (FSampleRate = 0) or (FChannels = 0) or (FBitsPerSample = 0) then
    begin
      if Assigned(FOnError) then
      begin
        FOnError(Self, 'libsndfile: Invalid audio data in file');
        Exit;
      end
      else
        raise Exception.Create('Invalid audio data');
    end;

    // Ограничиваем количество каналов для безопасности
    if FChannels > 2 then
      FChannels := 2;

    // Формируем информацию о формате для лога
    FormatInfo := Format('%dHz, %dch, %dbits, Frames: %d',
      [FSampleRate, FChannels, FBitsPerSample, FTotalSamples]);

    if Assigned(FOnLoad) then
      FOnLoad(Self, 'libsndfile: ' + FormatInfo, 0);

    FFilename := MusicFile;

  except
    FreeSndFileData;
    raise;
  end;
end;

procedure TSndFileAudioPlayer.FreeSndFileData;
begin
  if (FSndFile <> nil) and sf_IsLoaded then
  begin
    sf_close(FSndFile);
    FSndFile := nil;
  end;

  FillChar(FFileInfo, SizeOf(FFileInfo), 0);
  FSampleRate := DEFAULT_FREQ;
  FChannels := DEFAULT_CHANNELS;
  FBitsPerSample := DEFAULT_BITS;
  FTotalSamples := 0;
  FCurrentSample := 0;
  FTrackCount := 0;
end;

function TSndFileAudioPlayer.IsPlaybackFinished: Boolean;
begin
  Result := False;
  
  if (FSndFile = nil) or not sf_IsLoaded then
    Exit(True);

  // Проверяем, достигли ли конца файла
  if FTotalSamples > 0 then
    Result := FCurrentSample >= FTotalSamples
  else
    // Если общее количество сэмплов неизвестно, проверяем через позицию
    Result := sf_seek(FSndFile, 0, SEEK_CUR) >= FTotalSamples;
end;

function TSndFileAudioPlayer.RenderSamples(buffer: Pointer; frames: LongWord): LongWord;
var
  SamplesToRead: LongWord;
  SamplesRead: LongWord;
  TempBuffer: array of Int16;
  //i: Integer;
begin
  Result := 0;

  if (FSndFile = nil) or (frames = 0) then
    Exit;

  // Вычисляем количество сэмплов для чтения
  SamplesToRead := frames * FChannels;

  // Создаем временный буфер для сэмплов
  SetLength(TempBuffer{%H-}, SamplesToRead);

  try
    // Читаем сэмплы через libsndfile (16-битные)
    SamplesRead := sf_readf_short(FSndFile, @TempBuffer[0], frames);

    if SamplesRead > 0 then
    begin
      // Копируем сэмплы в выходной буфер
      Move(TempBuffer[0], buffer^, SamplesRead * FChannels * SizeOf(Int16));

      // Обновляем текущую позицию
      FCurrentSample := sf_seek(FSndFile, 0, SEEK_CUR);
      Result := SamplesRead;
    end
    else
    begin
      // Не удалось прочитать сэмплы - конец файла или ошибка
      FillChar(buffer^, frames * FChannels * SizeOf(Int16), 0);
    end;

  finally
    SetLength(TempBuffer, 0);
  end;
end;

procedure TSndFileAudioPlayer.ResetPlayback;
begin
  if (FSndFile <> nil) and sf_IsLoaded then
  begin
    // Перемещаемся в начало файла
    sf_seek(FSndFile, 0, SEEK_SET);
    FCurrentSample := 0;
  end;
end;

class procedure TSndFileAudioPlayer.AudioCallback(bufferData: pointer; frames: LongWord); cdecl;
var
  BytesRendered: LongWord;
  LocalPlayer: TSndFileAudioPlayer;
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
      if (FSndFile = nil) or FIsPaused or not sf_IsLoaded then
      begin
        FillChar(bufferData^, frames * FChannels * (DEFAULT_BITS div 8), 0);
        Exit;
      end;

      // Рендерим аудио через libsndfile
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
          FillChar(bufferData^, frames * FChannels * (DEFAULT_BITS div 8), 0);
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

class procedure TSndFileAudioPlayer.AudioProcessEqualizer(buffer: pointer;
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

procedure TSndFileAudioPlayer.CheckError(Condition: Boolean; const Msg: string);
begin
  if Condition and Assigned(FOnError) then
    FOnError(Self, Msg);
end;

procedure TSndFileAudioPlayer.InternalStop(ClearData: Boolean);
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
        FreeSndFileData;

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
procedure TSndFileAudioPlayer.OpenMusicFile(const MusicFile: String);
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

    // Загружаем новый аудио файл
    try
      LoadSndFile(MusicFile);
      FCurrentTrack := 0;
    except
      on E: Exception do
      begin
        CheckError(True, 'Error loading audio file: ' + E.Message);
        InternalStop;
      end;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TSndFileAudioPlayer.Play;
begin
  FPositionLock.Enter;
  try
    if (FSndFile <> nil) and not IsAudioStreamPlaying(FStream) then
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

procedure TSndFileAudioPlayer.Pause;
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

procedure TSndFileAudioPlayer.Resume;
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

procedure TSndFileAudioPlayer.Stop;
begin
  InternalStop(False);
  SetPosition(0);
end;

procedure TSndFileAudioPlayer.SetPosition(PositionMs: Integer);
var
  TargetSample: Int64;
begin
  FPositionLock.Enter;
  try
    if (FSndFile <> nil) and sf_IsLoaded then
    begin
      if FSampleRate > 0 then
      begin
        TargetSample := Round((PositionMs / 1000) * FSampleRate);
        if TargetSample < 0 then TargetSample := 0;
        if (FTotalSamples > 0) and (TargetSample > FTotalSamples) then
          TargetSample := FTotalSamples;
          
        if sf_seek(FSndFile, TargetSample, SEEK_SET) >= 0 then
        begin
          FCurrentSample := TargetSample;
        end
        else
        begin
          CheckError(True, 'Failed to seek in audio file');
        end;
      end;
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TSndFileAudioPlayer.GetPosition: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if (FSndFile <> nil) and sf_IsLoaded then
    begin
      if FSampleRate > 0 then
        Result := Round((FCurrentSample / FSampleRate) * 1000);
    end;
  finally
    FPositionLock.Leave;
  end;
end;

function TSndFileAudioPlayer.GetDuration: Integer;
begin
  Result := 0;
  FPositionLock.Enter;
  try
    if (FSndFile <> nil) and sf_IsLoaded then
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

procedure TSndFileAudioPlayer.SetLoopMode(Mode: Boolean);
begin
  FLoopMode := Mode;
end;

function TSndFileAudioPlayer.GetLoopMode: Boolean;
begin
  Result := FLoopMode;
end;

function TSndFileAudioPlayer.IsPlaying: Boolean;
begin
  Result := (FCurrentPlayer = Self) and not FIsPaused and 
            (FSndFile <> nil) and sf_IsLoaded;
end;

function TSndFileAudioPlayer.IsPaused: Boolean;
begin
  Result := FIsPaused;
end;

function TSndFileAudioPlayer.IsStopped: Boolean;
begin
  Result := (FSndFile = nil) or (FCurrentPlayer <> Self) or
            (not IsAudioStreamPlaying(FStream) and not FIsPaused);
end;

function TSndFileAudioPlayer.GetState: TPlayerState;
begin
  if IsPlaying then
    Result := psPlaying
  else if IsPaused then
    Result := psPaused
  else
    Result := psStopped;
end;

function TSndFileAudioPlayer.GetCurrentTrack: Integer;
begin
  Result := FCurrentTrack;
end;

function TSndFileAudioPlayer.GetCurrentFile: String;
begin
  Result := FFilename;
end;

function TSndFileAudioPlayer.GetTrackCount: Integer;
begin
  Result := FTrackCount;
end;


procedure TSndFileAudioPlayer.SetEqualizerBand(BandIndex: Integer; Gain: Single
  );
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
  begin
    // Всегда сохраняем настройки
    FBands[BandIndex].Gain := Max(-12.0, Min(12.0, Gain));
    CalculateFilterCoefficients(BandIndex);
  end;
end;

function TSndFileAudioPlayer.GetEqualizerBand(BandIndex: Integer): Single;
begin
  if (BandIndex >= 0) and (BandIndex < BANDS_COUNT) then
    Result := FBands[BandIndex].Gain
  else
    Result := 0.0;
end;

procedure TSndFileAudioPlayer.ResetEqualizer;
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

function TSndFileAudioPlayer.GetBalance: Single;
begin
  FPositionLock.Enter;
  try
    Result := FBalance;
  finally
    FPositionLock.Leave;
  end;
end;

procedure TSndFileAudioPlayer.SetBalance(AValue: Single);
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
function TSndFileAudioPlayer.GetOnEnd: TEndEvent;
begin
  Result := FOnEnd;
end;

function TSndFileAudioPlayer.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TSndFileAudioPlayer.GetOnLoad: TLoadEvent;
begin
  Result := FOnLoad;
end;

function TSndFileAudioPlayer.GetOnPause: TPauseEvent;
begin
  Result := FOnPause;
end;

function TSndFileAudioPlayer.GetOnPlay: TPlayEvent;
begin
  Result := FOnPlay;
end;

function TSndFileAudioPlayer.GetOnStop: TStopEvent;
begin
  Result := FOnStop;
end;

function TSndFileAudioPlayer.GetPlayerState: TPlayerState;
begin
  Result := GetState;
end;

function TSndFileAudioPlayer.GetTrackNumber: Integer;
begin
  Result := FCurrentTrack;
end;

procedure TSndFileAudioPlayer.SetTrackNumber(AValue: Integer);
begin
  FCurrentTrack := AValue;
end;

procedure TSndFileAudioPlayer.InitializeEqualizer;
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

procedure TSndFileAudioPlayer.CalculateFilterCoefficients(bandIndex: Integer);
var
  band: TEqualizerBand;
  A, w0, alpha: Single;
  cosW0, sinW0: Single;
  coeffs: TFilterCoeffs;
begin
  band := FBands[bandIndex]; // Исправлено: FBands вместо Bands

  // Преобразование dB в линейное значение
  A := Power(10.0, band.Gain / 40.0);
  w0 := 2.0 * PI * band.Frequency / FSampleRate;
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

function TSndFileAudioPlayer.ApplyFilter(input: Single; bandIndex,
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
procedure TSndFileAudioPlayer.SetOnEnd(AEvent: TEndEvent);
begin
  FOnEnd := AEvent;
end;

procedure TSndFileAudioPlayer.SetOnError(AEvent: TErrorEvent);
begin
  FOnError := AEvent;
end;

procedure TSndFileAudioPlayer.SetOnLoad(AEvent: TLoadEvent);
begin
  FOnLoad := AEvent;
end;

procedure TSndFileAudioPlayer.SetOnPause(AEvent: TPauseEvent);
begin
  FOnPause := AEvent;
end;

procedure TSndFileAudioPlayer.SetOnPlay(AEvent: TPlayEvent);
begin
  FOnPlay := AEvent;
end;

procedure TSndFileAudioPlayer.SetOnStop(AEvent: TStopEvent);
begin
  FOnStop := AEvent;
end;

end.

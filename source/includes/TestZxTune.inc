function TrAudioPlayer.TestZxTune(const MusicFile: string; var PlayerTag: TPlayerTag): Boolean;
var
  ZxTuneData: ZXTuneHandle;
  ZxTuneModule: ZXTuneHandle;
  ZxTunePlayer: ZXTuneHandle; // Добавляем плеер для получения длительности
  ZxModuleInfo: ZXTuneModuleInfo;
  FileStream: TFileStream;
  FileData: Pointer;
  FileSize: NativeUInt;
  FrameDuration: Integer;
  Success: Boolean;
  SuccesBuffer: array[0..255] of AnsiChar;
begin
  Result := False;

  // Инициализация Tag
  PlayerTag.PlayerType := ptUnknown;
  PlayerTag.Title := '';
  PlayerTag.Artist := '';
  PlayerTag.ChipType := '';
  PlayerTag.System := '';
  PlayerTag.Tracker := '';
  PlayerTag.Duration := 0;
  PlayerTag.TrackCount := 0;

  if not FileExists(MusicFile) then
    Exit;

  ZxModuleInfo := Default(ZXTuneModuleInfo);

  // Инициализируем структуру
   FillChar(ZxModuleInfo, SizeOf(ZxModuleInfo), 0);

  try
    // Загружаем файл в память
    FileStream := TFileStream.Create(MusicFile, fmOpenRead or fmShareDenyWrite);
    try
      FileSize := FileStream.Size;
      GetMem(FileData, FileSize);
      FileStream.ReadBuffer(FileData^, FileSize);
    finally
      FileStream.Free;
    end;

    // Создаем ZXTune данные
    ZxTuneData := ZXTune_CreateData(FileData, FileSize);
    if ZxTuneData = nil then
    begin
      FreeMem(FileData);
      Exit;
    end;

    // Пытаемся открыть модуль
    ZxTuneModule := ZXTune_OpenModule(ZxTuneData);
    if ZxTuneModule = nil then
    begin
      ZXTune_CloseData(ZxTuneData);
      FreeMem(FileData);
      Exit;
    end;

    Result := True;
    PlayerTag.PlayerType := ptZxTune;
    PlayerTag.TrackCount:=0;
    try
      // Получаем информацию о модуле
      if not ZXTune_GetModuleInfo(ZxTuneModule, ZxModuleInfo) then
        raise Exception.Create('Failed to get module info');

      // Создаем временный плеер для получения длительности
      ZxTunePlayer := ZXTune_CreatePlayer(ZxTuneModule);
      if ZxTunePlayer <> nil then
      begin
        try
          // Получаем длительность фрейма в микросекундах
          FrameDuration := ZXTune_GetDuration(ZxTunePlayer);
          if FrameDuration <= 0 then
            FrameDuration := 0; // 20ms по умолчанию

          // Рассчитываем общую длительность: (frames * frame_duration) / 1000
          PlayerTag.Duration := Round((ZxModuleInfo.Frames * FrameDuration) / 1000);
        finally
          ZXTune_DestroyPlayer(ZxTunePlayer);
        end;
      end;

      // Получаем метаданные
      Success := ZXTune_GetModuleAttribute(ZxTuneModule, 'Title', @SuccesBuffer[0], SizeOf(SuccesBuffer));
      if Success then
        PlayerTag.Title := string(SuccesBuffer)
      else
        PlayerTag.Title := ExtractFileName(MusicFile);

      Success := ZXTune_GetModuleAttribute(ZxTuneModule, 'Author', @SuccesBuffer[0], SizeOf(SuccesBuffer));
      if Success then
        PlayerTag.Artist := string(SuccesBuffer)
      else
        PlayerTag.Artist := '';

      // Определяем тип трекера и системы
      Success := ZXTune_GetModuleAttribute(ZxTuneModule, 'Type', @SuccesBuffer[0], SizeOf(SuccesBuffer));

        case string(SuccesBuffer) of
          'AS0':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'ASC Sound Master v0.xx';
            end;
          'ASC':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'ASC Sound Master v1.xx-2.xx';
            end;
          'AY':
            begin
              PlayerTag.System := 'ZX Spectrum/Amstrad CPC';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'AY Emulator';
            end;
          'FTC':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum Fast Tracker';
            end;
          'GTR':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Global Tracker';
            end;
          'PSC':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Pro Sound Creator';
            end;
          'PSG':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum PSG';
            end;
          'PSM':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Pro Sound Maker';
            end;
          'PT1':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum Pro Tracker 1';
            end;
          'PT2':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum Pro Tracker 2';
            end;
          'PT3':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum Pro Tracker 3';
            end;
          'SQT':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'SQ Tracker';
            end;
          'ST1':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum Sound Tracker 1';
            end;
          'ST3':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum Sound Tracker 3';
            end;
          'STC':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Compiled Spectrum Sound Tracker 1';
            end;
          'STP':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum Sound Tracker Pro 1';
            end;
          'TXT', 'VTX':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Vortextracker';
            end;
          'TS':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910 x2';
              PlayerTag.Tracker := 'TurboSound module for AY Emulator';
            end;
          'YM':
            begin
              PlayerTag.System := 'Atari ST/Amstrad CPC';
              PlayerTag.ChipType := 'YM2149';
              PlayerTag.Tracker := 'YM';
            end;
          'STR':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Sample Tracker';
            end;
          'CHI':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Spectrum Chip Tracker';
            end;
          'SQD':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'SG Digital Tracker';
            end;
          'DMM':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Digital Music Maker';
            end;
          'PDT':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910';
              PlayerTag.Tracker := 'Prodigi Tracker';
            end;
          'DST':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'AY-3-8910/Covox';
              PlayerTag.Tracker := 'Digital Studio';
            end;
          'COP':
            begin
              PlayerTag.System := 'Sam Coupe';
              PlayerTag.ChipType := 'SAA1099';
              PlayerTag.Tracker := 'E-Tracker';
            end;
          'TFE', 'TF0':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'YM2203/OPN';
              PlayerTag.Tracker := 'TFM Music Maker';
            end;
          'TFD', 'TFC':
            begin
              PlayerTag.System := 'ZX Spectrum';
              PlayerTag.ChipType := 'YM2203/OPN';
              PlayerTag.Tracker := 'TurboFM';
            end;
          else
            begin
              PlayerTag.System := '8-bit Computer';
              PlayerTag.ChipType := 'Various';
              PlayerTag.Tracker := 'Chiptune';
            end;
        end;

    except
      on E: Exception do
      begin
        // В случае ошибок при получении метаданных, используем базовую информацию
        PlayerTag.Title := ExtractFileName(MusicFile);
        PlayerTag.Artist := 'Unknown Composer';
        PlayerTag.System := '8-bit Computer';
        PlayerTag.ChipType := 'Various';
        PlayerTag.Tracker := 'Chiptune';
        PlayerTag.TrackCount := 0;
        PlayerTag.Duration := 0;
      end;
    end;

    // Освобождаем ресурсы в правильном порядке
    ZXTune_CloseModule(ZxTuneModule);
    ZXTune_CloseData(ZxTuneData);
    FreeMem(FileData);

  except
    on E: Exception do
    begin
      // Освобождаем ресурсы в случае общей ошибки
         if ZxTuneModule <> nil then
           ZXTune_CloseModule(ZxTuneModule);
         if ZxTuneData <> nil then
           ZXTune_CloseData(ZxTuneData);
         if FileData <> nil then
           FreeMem(FileData);

         Result := False;
    end;
  end;
end;





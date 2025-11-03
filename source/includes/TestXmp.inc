function TrAudioPlayer.TestXMP(const MusicFile: string; var PlayerTag: TPlayerTag): Boolean;
var
  XmpContext: xmp_context = nil;
  FileStream: TFileStream = nil;
  FileData: Pointer = nil;
  FileSize: LongInt;
  Error: Integer;
  TestInfo: xmp_test_info;
  ModuleInfo: xmp_module_info;
  FrameInfo: xmp_frame_info; // Добавляем локальную переменную для FrameInfo
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

  try
    // Загружаем файл в память
    FileStream := TFileStream.Create(MusicFile, fmOpenRead or fmShareDenyWrite);
    try
      FileSize := FileStream.Size;
      GetMem(FileData, FileSize);
      FileStream.ReadBuffer(FileData^, FileSize);
    finally
      FreeAndNil(FileStream);
    end;

    // Инициализируем структуры
    FillChar(TestInfo{%H-}, SizeOf(TestInfo), 0);
    FillChar(ModuleInfo{%H-}, SizeOf(ModuleInfo), 0);
    FillChar(FrameInfo{%H-}, SizeOf(FrameInfo), 0);

    // Пытаемся протестировать XMP модуль
    Error := xmp_test_module_from_memory(FileData, FileSize, TestInfo);

    if Error = 0 then
    begin
      Result := True; // Модуль успешно распознан
      PlayerTag.PlayerType := ptXMP;

      // Создаем контекст для получения дополнительной информации
      XmpContext := xmp_create_context();
      if XmpContext <> nil then
      begin
        try
          // Загружаем модуль для получения метаданных
          if xmp_load_module_from_memory(XmpContext, FileData, FileSize) = 0 then
          begin
            // Получаем информацию о модуле
            xmp_get_module_info(XmpContext, ModuleInfo);

            // Заполняем метаданные
            try
              // Получаем заголовок
              if (ModuleInfo.module <> nil) and (ModuleInfo.module^.name[0] <> #0) then
                PlayerTag.Title := string(ModuleInfo.module^.name)
              else
                PlayerTag.Title := ExtractFileName(MusicFile);

              // Получаем тип модуля (трекер/система)
              if (ModuleInfo.module <> nil) and (ModuleInfo.module^.typ[0] <> #0) then
              begin
                PlayerTag.Tracker := string(ModuleInfo.module^.typ);
                PlayerTag.System := string(ModuleInfo.module^.typ);
              end
              else
              begin
                PlayerTag.Tracker := 'Module';
                PlayerTag.System := 'Module';
              end;

              // Получаем тип формата из TestInfo
              if TestInfo.type_[0] <> #0 then
                PlayerTag.ChipType := string(TestInfo.type_);

              // Получаем продолжительность
              if xmp_start_player(XmpContext, 44100, 0) = 0 then
              begin
                try
                  xmp_get_frame_info(XmpContext, FrameInfo);
                  PlayerTag.Duration := FrameInfo.total_time;
                finally
                  xmp_end_player(XmpContext);
                end;
              end;

              // Получаем количество треков (паттернов)
              if ModuleInfo.module <> nil then
                PlayerTag.TrackCount := ModuleInfo.module^.pat;

              // Артист обычно не хранится в XMP метаданных
              PlayerTag.Artist := 'Unknown Artist';

            except
              // В случае ошибок при получении метаданных, используем базовую информацию
              on E: Exception do
              begin
                PlayerTag.Title := ExtractFileName(MusicFile);
                PlayerTag.System := 'Module';
                PlayerTag.Tracker := string(TestInfo.type_);
              end;
            end;

            // Освобождаем модуль
            xmp_release_module(XmpContext);
          end;
        finally
          xmp_free_context(XmpContext);
        end;
      end
      else
      begin
        // Если не удалось создать контекст, используем базовую информацию из TestInfo
        PlayerTag.Title := ExtractFileName(MusicFile);
        if TestInfo.name[0] <> #0 then
          PlayerTag.Title := string(TestInfo.name);
        PlayerTag.System := string(TestInfo.type_);
        PlayerTag.Tracker := string(TestInfo.type_);
        PlayerTag.ChipType := string(TestInfo.type_);
      end;
    end;

  except
    on E: Exception do
    begin
      Result := False;
      // Освобождаем ресурсы в случае исключения
      if FileData <> nil then
        FreeMem(FileData);
    end;
  end;

  // Гарантированное освобождение ресурсов
  if FileData <> nil then
  begin
    FreeMem(FileData);
  end;
end;

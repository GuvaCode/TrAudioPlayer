function TrAudioPlayer.TestOpenMPT(const MusicFile: string; var PlayerTag: TPlayerTag): Boolean;
var
  OpenMPTModule: Popenmpt_module = nil;
  FileStream: TFileStream = nil;
  FileData: Pointer = nil;
  FileSize: csize_t;
  Error: cint;
  ErrorMessage: pchar = nil;
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

    // Пытаемся создать OpenMPT модуль
    OpenMPTModule := openmpt_module_create_from_memory2(
      FileData, FileSize,
      nil, nil,  // logfunc, loguser
      nil, nil,   // errfunc, erruser
      @Error, @ErrorMessage,
      nil         // ctls
    );

    if OpenMPTModule <> nil then
    begin
      Result := True; // Модуль успешно распознан
      PlayerTag.PlayerType := ptOpenMPT;

      // Заполняем метаданные
      try
        // Получаем заголовок
        PlayerTag.Title := openmpt_module_get_metadata(OpenMPTModule, 'title');
        if PlayerTag.Title = '' then
          PlayerTag.Title := ExtractFileName(MusicFile);

        // Получаем артиста
        PlayerTag.Artist := openmpt_module_get_metadata(OpenMPTModule, 'artist');

        // Получаем трекер
        PlayerTag.Tracker := openmpt_module_get_metadata(OpenMPTModule, 'tracker');
        if PlayerTag.Tracker = '' then
          PlayerTag.Tracker := openmpt_module_get_metadata(OpenMPTModule, 'type_long');

        // Получаем тип системы/формата
        PlayerTag.System := 'AMIGA / PC';  // todo
        //openmpt_module_get_metadata(OpenMPTModule, 'originaltype_long');
        //if Tag.System = '' then
        //  Tag.System := openmpt_module_get_metadata(OpenMPTModule, 'container');

        // Получаем продолжительность
        PlayerTag.Duration := Trunc(openmpt_module_get_duration_seconds(OpenMPTModule)) * 1000;

        // Получаем количество треков
        PlayerTag.TrackCount := openmpt_module_get_num_subsongs(OpenMPTModule) - 1;

        // Получаем тип чипа (если доступно)
        PlayerTag.ChipType := openmpt_module_get_metadata(OpenMPTModule, 'type');

      except
        // В случае ошибок при получении метаданных, просто используем базовую информацию
        on E: Exception do
        begin
          PlayerTag.Title := ExtractFileName(MusicFile);
          PlayerTag.System := 'Module';
        end;
      end;

      // Освобождаем модуль
      openmpt_module_destroy(OpenMPTModule);
      OpenMPTModule := nil;
    end
    else
    begin
      // Освобождаем строку с ошибкой, если она есть
      if ErrorMessage <> nil then
      begin
        openmpt_free_string(ErrorMessage);
        ErrorMessage := nil;
      end;
    end;

  except
    on E: Exception do
    begin
      Result := False;
      // Добавить освобождение ресурсов
      if FileData <> nil then
        FreeMem(FileData);
      if ErrorMessage <> nil then
        openmpt_free_string(ErrorMessage);
    end;
  end;

  // Гарантированное освобождение ресурсов
  if OpenMPTModule <> nil then
  begin
    openmpt_module_destroy(OpenMPTModule);
  end;

  if ErrorMessage <> nil then
  begin
    openmpt_free_string(ErrorMessage);
  end;

  if FileData <> nil then
  begin
    FreeMem(FileData);
  end;
end;     

function GetBitsPerSampleFromFormat(Format: Integer): Integer;
begin
  case (Format and SF_FORMAT_SUBMASK) of
    SF_FORMAT_PCM_S8: Result := 8;
    SF_FORMAT_PCM_U8: Result := 8;
    SF_FORMAT_PCM_16: Result := 16;
    SF_FORMAT_PCM_24: Result := 24;
    SF_FORMAT_PCM_32: Result := 32;
    SF_FORMAT_FLOAT: Result := 32;
    SF_FORMAT_DOUBLE: Result := 64;
    SF_FORMAT_ULAW: Result := 8;
    SF_FORMAT_ALAW: Result := 8;
    SF_FORMAT_IMA_ADPCM: Result := 4;
    SF_FORMAT_MS_ADPCM: Result := 4;
    SF_FORMAT_GSM610: Result := 16;
    SF_FORMAT_VOX_ADPCM: Result := 12;
    SF_FORMAT_G721_32: Result := 4;
    SF_FORMAT_G723_24: Result := 3;
    SF_FORMAT_G723_40: Result := 5;
    SF_FORMAT_DWVW_12: Result := 12;
    SF_FORMAT_DWVW_16: Result := 16;
    SF_FORMAT_DWVW_24: Result := 24;
    SF_FORMAT_DWVW_N: Result := 16; // по умолчанию
    SF_FORMAT_DPCM_8: Result := 8;
    SF_FORMAT_DPCM_16: Result := 16;
    SF_FORMAT_VORBIS: Result := 16; // Vorbis обычно 16 бит
  else
    Result := 16; // по умолчанию
  end;
end;

function TrAudioPlayer.TestSndFile(const MusicFile: string; var PlayerTag: TPlayerTag): Boolean;
var
  SndFile: TSNDFILE_HANDLE = nil;
  FileInfo: TSF_INFO;
  FormatInfo: TSF_FORMAT_INFO;
  FormatName: string;
  SubtypeName: string;
  TempPChar: PChar;
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

  if not sf_IsLoaded then
    Exit;

  try
    FillChar(FileInfo{%H-}, SizeOf(FileInfo), 0);
    SndFile := sf_open(MusicFile, SFM_READ, FileInfo);

    if SndFile <> nil then
    begin
      Result := True;
      PlayerTag.PlayerType := ptSndFile;

      try
        // Базовое имя файла
        PlayerTag.Title := ExtractFileName(MusicFile);

        // Получаем информацию о формате - БЕЗОПАСНО
        FillChar(FormatInfo{%H-}, SizeOf(FormatInfo), 0);
        FormatInfo.format := FileInfo.format and SF_FORMAT_TYPEMASK;

        if sf_command_pointer(SndFile, SFC_GET_FORMAT_INFO, @FormatInfo, SizeOf(FormatInfo)) = 0 then
        begin
          TempPChar := PChar(FormatInfo.Name);
          if TempPChar <> nil then
            FormatName := StrPas(TempPChar)
          else
            FormatName := 'Unknown Format';
        end
        else
          FormatName := 'Unknown Format';

        // Получаем информацию о подтипе - БЕЗОПАСНО
        FillChar(FormatInfo, SizeOf(FormatInfo), 0);
        FormatInfo.format := FileInfo.format and SF_FORMAT_SUBMASK;

        if sf_command_pointer(SndFile, SFC_GET_FORMAT_INFO, @FormatInfo, SizeOf(FormatInfo)) = 0 then
        begin
          TempPChar := PChar(FormatInfo.Name);
          if TempPChar <> nil then
            SubtypeName := StrPas(TempPChar)
          else
            SubtypeName := 'Unknown Subtype';
        end
        else
          SubtypeName := 'Unknown Subtype';

        // Заполняем системную информацию
        PlayerTag.System := FormatName;
        PlayerTag.ChipType := SubtypeName;

        // Безопасное получение метаданных
        TempPChar := sf_get_string(SndFile, SF_STR_ARTIST);
        if TempPChar <> nil then
          PlayerTag.Artist := StrPas(TempPChar)
        else
        begin
          TempPChar := sf_get_string(SndFile, SF_STR_COPYRIGHT);
          if TempPChar <> nil then
            PlayerTag.Artist := StrPas(TempPChar);
        end;

        // Заголовок
        TempPChar := sf_get_string(SndFile, SF_STR_TITLE);
        if TempPChar <> nil then
          PlayerTag.Title := StrPas(TempPChar);

        // Комментарий
        TempPChar := sf_get_string(SndFile, SF_STR_COMMENT);
        if TempPChar <> nil then
          PlayerTag.Tracker := StrPas(TempPChar);

        // Продолжительность
        if (FileInfo.samplerate > 0) and (FileInfo.frames > 0) then
          PlayerTag.Duration := Trunc((FileInfo.frames / FileInfo.samplerate) * 1000)
        else
          PlayerTag.Duration := 0;

          PlayerTag.TrackCount := 0;

        // Дополнительная информация о качестве
        PlayerTag.ChipType := PlayerTag.ChipType + Format(' (%dHz, %dch, %dbits)',
          [FileInfo.samplerate, FileInfo.channels, GetBitsPerSampleFromFormat(FileInfo.format)]);

      except
        on E: Exception do
        begin
          // Резервная информация при ошибках
          PlayerTag.Title := ExtractFileName(MusicFile);
          PlayerTag.System := 'Audio File';
          PlayerTag.ChipType := Format('%dHz, %dch', [FileInfo.samplerate, FileInfo.channels]);
          PlayerTag.Duration := 0;
          PlayerTag.TrackCount := 1;
        end;
      end;

      sf_close(SndFile);
      SndFile := nil;
    end;

  except
    on E: Exception do
    begin
      Result := False;
      if SndFile <> nil then
      begin
        sf_close(SndFile);
        SndFile := nil;
      end;
    end;
  end;
end;
    

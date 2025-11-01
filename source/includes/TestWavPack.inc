function TrAudioPlayer.TestWavPack(const MusicFile: string;
  var PlayerTag: TPlayerTag): Boolean;
var
  FileStream: TFileStream;
  Header: array[0..31] of Byte;
  WavpackSig: array[0..3] of AnsiChar;
  WPontext: WavpackContext;
  ErrorMsg: array[0..255] of AnsiChar;
  TagValue: array[0..1023] of AnsiChar;
  TotalSamples: Int64;
  SampleRate: Integer;
  DurationMs: Integer;
begin
  Result := False;
  PlayerTag.PlayerType := ptUnknown;

  // Быстрая проверка по расширению
  if not SameText(ExtractFileExt(MusicFile), '.wv') then
    Exit(False);

  if not FileExists(MusicFile) then
    Exit(False);

  try
    FileStream := TFileStream.Create(MusicFile, fmOpenRead or fmShareDenyNone);
    try
      // Читаем начало файла для проверки сигнатуры
      if FileStream.Size < 32 then
        Exit(False);

      FileStream.Read(Header, SizeOf(Header));

      // Проверяем сигнатуру WavPack ("wvpk")
      Move(Header[0], WavpackSig, 4);
      Result := (WavpackSig[0] = 'w') and
                (WavpackSig[1] = 'v') and
                (WavpackSig[2] = 'p') and
                (WavpackSig[3] = 'k');

      if Result then
      begin
        PlayerTag.PlayerType := ptWavPack;

        // === ЗАПОЛНЯЕМ ОСНОВНЫЕ ТЕГИ ЗНАЧЕНИЯМИ ПО УМОЛЧАНИЮ ===
        PlayerTag.Title := ExtractFileName(MusicFile);
        PlayerTag.Artist := 'Unknown Artist';
        PlayerTag.ChipType := 'PCM Audio';
        PlayerTag.System := 'WavPack Audio';
        PlayerTag.Tracker := 'WavPack Encoder';
        PlayerTag.Duration := 0;
        PlayerTag.TrackCount := 0;

        // === ПЫТАЕМСЯ ПОЛУЧИТЬ ДЕТАЛЬНУЮ ИНФОРМАЦИЮ ИЗ WAVPACK ===
        if WavPackLoaded then
        begin
          FillChar(ErrorMsg{%H-}, SizeOf(ErrorMsg), 0);
          WPontext := WavpackOpenFileInput(PAnsiChar(MusicFile), ErrorMsg,
            OPEN_TAGS or OPEN_NORMALIZE, 0);

          if WPontext <> nil then
          try
            // Получаем техническую информацию
            SampleRate := WavpackGetSampleRate(WPontext);
            TotalSamples := WavpackGetNumSamples64(WPontext);

            // Рассчитываем длительность
            if (SampleRate > 0) and (TotalSamples > 0) then
            begin
              DurationMs := Round((TotalSamples / SampleRate) * 1000);
              PlayerTag.Duration := DurationMs;
            end;

            // === ЧИТАЕМ МЕТАДАННЫЕ ИЗ ТЕГОВ WAVPACK ===
            FillChar(TagValue{%H-}, SizeOf(TagValue), 0);

            // Заголовок/название трека
            if WavpackGetTagItem(WPontext, 'title', TagValue, SizeOf(TagValue)) > 0 then
              PlayerTag.Title := string(Trim(TagValue));

            FillChar(TagValue, SizeOf(TagValue), 0);
            // Исполнитель
            if WavpackGetTagItem(WPontext, 'artist', TagValue, SizeOf(TagValue)) > 0 then
              PlayerTag.Artist := string(Trim(TagValue));

            FillChar(TagValue, SizeOf(TagValue), 0);
            // Альбом (может использоваться как система)
            if WavpackGetTagItem(WPontext, 'album', TagValue, SizeOf(TagValue)) > 0 then
              PlayerTag.System := string(Trim(TagValue));

            // Получаем битность для уточнения типа чипа
            FillChar(TagValue, SizeOf(TagValue), 0);
            if WavpackGetTagItem(WPontext, 'bits', TagValue, SizeOf(TagValue)) > 0 then
            begin
              case StrToIntDef(string(TagValue), 16) of
                4..8: PlayerTag.ChipType := 'PCM 8-bit';
                9..16: PlayerTag.ChipType := 'PCM 16-bit CD Quality';
                17..24: PlayerTag.ChipType := 'PCM 24-bit Hi-Res';
                25..32: PlayerTag.ChipType := 'PCM 32-bit Hi-Res';
              end;
            end;

            // Дополнительная информация о год выпуска
            FillChar(TagValue, SizeOf(TagValue), 0);
            if WavpackGetTagItem(WPontext, 'year', TagValue, SizeOf(TagValue)) > 0 then
            begin
              if PlayerTag.System = 'WavPack Audio' then
                PlayerTag.System := 'WavPack Audio (' + string(TagValue) + ')'
              else
                PlayerTag.System := PlayerTag.System + ' (' + string(TagValue) + ')';
            end;

          finally
            WavpackCloseFile(WPontext);
          end;
        end;

        Result := True;
      end;

    finally
      FileStream.Free;
    end;
  except
    Result := False;
  end;

  if not Result then
    PlayerTag.PlayerType := ptUnknown;
end;    

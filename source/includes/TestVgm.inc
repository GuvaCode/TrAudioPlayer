function TrAudioPlayer.TestVGM(const MusicFile: string; var PlayerTag: TPlayerTag
  ): Boolean;
var
  FS: TFileStream;
  VGMHeader: VGM_HEADER;
  GD3PlayerTag: VGM_TAG;
  GD3Offset: UInt32;
 // TempLng: UInt32;
  CurPos: UInt32;
  //TempWChar: WideChar;
  GameName, SystemName, ReleaseDate, Creator, Notes: string; // Вынесены в начало

  function ReadWideString(FS: TFileStream; var Offset: UInt32; MaxPos: UInt32): string;
  var
    WStr: WideString;
    WChar: WideChar;
  begin
    WStr := '';
    WChar := Default(WideChar);
    VGMHeader := Default(VGM_HEADER);
    GD3PlayerTag := Default(VGM_TAG);
    FS.Position := Offset;

    while (Offset < MaxPos - 1) do
    begin
      FS.Read(WChar, SizeOf(WideChar));
      Inc(Offset, SizeOf(WideChar));

      if WChar = #0 then
        Break;

      WStr := WStr + WChar;

      // Защита от бесконечного цикла
      if Length(WStr) > 1000 then
        Break;
    end;

    Result := UTF8Encode(WStr);
  end;

  function GetChipTypeFromHeader(const Header: VGM_HEADER): string;
  begin
    Result := 'Multiple Chips';

    if Header.lngHzYM2413 > 0 then Exit('YM2413');
    if Header.lngHzYM2612 > 0 then Exit('YM2612');
    if Header.lngHzYM2151 > 0 then Exit('YM2151');
    if Header.lngHzPSG > 0 then Exit('SN76489');
    if Header.lngHzAY8910 > 0 then Exit('AY-3-8910');
    if Header.lngHzSAA1099 > 0 then Exit('SAA1099');
    if Header.lngHzYM2203 > 0 then Exit('YM2203');
    if Header.lngHzYM2608 > 0 then Exit('YM2608');
    if Header.lngHzYM2610 > 0 then Exit('YM2610');
    if Header.lngHzYM3812 > 0 then Exit('YM3812');
    if Header.lngHzY8950 > 0 then Exit('Y8950');
    if Header.lngHzRF5C68 > 0 then Exit('RF5C68');
    if Header.lngHzGBDMG > 0 then Exit('Game Boy');
    if Header.lngHzNESAPU > 0 then Exit('NES APU');
  end;

  function GetSystemFromChipType(const ChipType: string): string;
  begin
    if Pos('YM2612', ChipType) > 0 then
      Result := 'Sega Genesis/Mega Drive'
    else if Pos('YM2413', ChipType) > 0 then
      Result := 'MSX/Master System'
    else if Pos('YM2151', ChipType) > 0 then
      Result := 'Arcade'
    else if (Pos('AY-3-8910', ChipType) > 0) then
      Result := 'Various 8-bit'
    else if Pos('SN76489', ChipType) > 0 then
      Result := 'Sega Master System/Game Gear'
    else if Pos('SAA1099', ChipType) > 0 then
      Result := 'Sam Coupe/Sound Blaster'
    else if Pos('Game Boy', ChipType) > 0 then
      Result := 'Nintendo Game Boy'
    else if Pos('NES APU', ChipType) > 0 then
      Result := 'Nintendo NES'
    else if Pos('RF5C68', ChipType) > 0 then
      Result := 'Sega CD'
    else
      Result := 'Arcade/Console';
  end;

  function CalculateDuration(const Header: VGM_HEADER): Integer;
  var
    TotalSamples: UInt32;
    SampleRate: UInt32;
  begin
    Result := 180000; // 3 минуты по умолчанию

    TotalSamples := Header.lngTotalSamples;
    if TotalSamples = 0 then
      Exit;

    SampleRate := Header.lngRate;
    if SampleRate = 0 then
      SampleRate := 44100; // Частота по умолчанию

    if SampleRate > 0 then
      Result := Round((TotalSamples / SampleRate) * 1000);

    // Ограничиваем максимальную длительность
    if Result > 600000 then // 10 минут
      Result := 180000;
  end;

begin
  Result := False;

  // Инициализация PlayerTag
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

  // Инициализация локальных переменных
  GameName := '';
  SystemName := '';
  ReleaseDate := '';
  Creator := '';
  Notes := '';

  try
    FS := TFileStream.Create(MusicFile, fmOpenRead or fmShareDenyWrite);
    try
      // Проверяем размер файла
      if FS.Size < SizeOf(VGM_HEADER) then
        Exit;

      // Читаем VGM заголовок
      FS.Position := 0;
      FS.Read(VGMHeader, SizeOf(VGM_HEADER));

      // Проверяем сигнатуру VGM файла
      if VGMHeader.fccVGM <> $206D6756 then // 'Vgm '
        Exit;

      Result := True;
      PlayerTag.PlayerType := ptVGMPlay;
      PlayerTag.TrackCount := 0;

      // Базовая информация
      PlayerTag.Title := ExtractFileName(MusicFile);
      PlayerTag.Artist := 'Unknown Composer';
      PlayerTag.Tracker := 'Video Game Music';

      // Определяем тип чипа
      PlayerTag.ChipType := GetChipTypeFromHeader(VGMHeader);

      // Определяем систему
      PlayerTag.System := GetSystemFromChipType(PlayerTag.ChipType);

      // Рассчитываем длительность
      PlayerTag.Duration := CalculateDuration(VGMHeader);

      // Получаем смещение GD3 тега
      GD3Offset := VGMHeader.lngGD3Offset;
      if GD3Offset = 0 then
        Exit;

      // Переходим к GD3 тегу
      FS.Position := GD3Offset;

      // Читаем GD3 заголовок
      FS.Read(GD3PlayerTag.fccGD3, SizeOf(GD3PlayerTag.fccGD3));
      FS.Read(GD3PlayerTag.lngVersion, SizeOf(GD3PlayerTag.lngVersion));
      FS.Read(GD3PlayerTag.lngTagLength, SizeOf(GD3PlayerTag.lngTagLength));

      // Проверяем сигнатуру GD3
      if GD3PlayerTag.fccGD3 <> $20336447 then // 'Gd3 '
        Exit;

      // Парсим GD3 тег
      CurPos := GD3Offset + 12; // Пропускаем заголовок GD3

      // Track Name (English)
      PlayerTag.Title := ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength);
      if PlayerTag.Title = '' then
        PlayerTag.Title := ExtractFileName(MusicFile);

      // Track Name (Japanese)
      ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength); // Пропускаем

      // Game Name (English)
      GameName := ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength);
      if GameName <> '' then
        PlayerTag.System := GameName;

      // Game Name (Japanese)
      ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength); // Пропускаем

      // System Name (English)
      SystemName := ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength);
      if SystemName <> '' then
      begin
        if PlayerTag.System = GetSystemFromChipType(PlayerTag.ChipType) then
          PlayerTag.System := SystemName
        else
          PlayerTag.System := PlayerTag.System + ' (' + SystemName + ')';
      end;

      // System Name (Japanese)
      ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength); // Пропускаем

      // Author Name (English)
      PlayerTag.Artist := ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength);
      if PlayerTag.Artist = '' then
        PlayerTag.Artist := 'Unknown Composer';

      // Author Name (Japanese)
      ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength); // Пропускаем

      // Release Date
      ReleaseDate := ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength);
      if ReleaseDate <> '' then
        PlayerTag.System := PlayerTag.System + ' [' + ReleaseDate + ']';

      // Creator
      Creator := ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength);
      if Creator <> '' then
        PlayerTag.Tracker := 'VGM - ' + Creator;

      // Notes
      Notes := ReadWideString(FS, CurPos, GD3Offset + 12 + GD3PlayerTag.lngTagLength);
      if (Notes <> '') and (PlayerTag.Tracker = 'Video Game Music') then
        PlayerTag.Tracker := 'VGM - ' + Copy(Notes, 1, 50);

    finally
      FS.Free;
    end;

  except
    on E: Exception do
    begin
      // В случае ошибок используем базовую информацию
      if Result then
      begin
        PlayerTag.Title := ExtractFileName(MusicFile);
        PlayerTag.Artist := 'Unknown Composer';
        PlayerTag.Tracker := 'Video Game Music';
        PlayerTag.ChipType := 'VGM';
        PlayerTag.System := 'Arcade/Console';
        PlayerTag.TrackCount := 1;
        PlayerTag.Duration := 180000;
      end;
      Result := False;
    end;
  end;
end; 

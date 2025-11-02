function TrAudioPlayer.TestRAudio(const MusicFile: string; var PlayerTag: TPlayerTag): Boolean;
var
  Buffer: array[0..11] of Byte;
  F: File;
begin
  Result := False;
  PlayerTag.PlayerType := ptUnknown;
  PlayerTag.Title := ExtractFileName(MusicFile);

  try
    AssignFile(F, MusicFile);
    Reset(F, 1);
    try
      if FileSize(F) > 11 then
      begin
        BlockRead(F, Buffer, 12);

        // WAV: RIFF....WAVE
        if (Buffer[0] = $52) and (Buffer[1] = $49) and  // RI
           (Buffer[2] = $46) and (Buffer[3] = $46) and  // FF
           (Buffer[8] = $57) and (Buffer[9] = $41) and  // WA
           (Buffer[10] = $56) and (Buffer[11] = $45) then // VE
        begin
          if (play_WAV in FDefaultFormat) then
          begin
            Result := True;
            PlayerTag.PlayerType := ptDefault;
            PlayerTag.System := 'WAV';
          end;
        end
        // MP3: ID3 или MPEG frame
        else if ((Buffer[0] = $49) and (Buffer[1] = $44) and (Buffer[2] = $33)) or  // ID3
                ((Buffer[0] = $FF) and ((Buffer[1] and $E0) = $E0)) then  // MPEG sync
        begin
          if (play_MP3 in FDefaultFormat) then
          begin
            Result := True;
            PlayerTag.PlayerType := ptDefault;
            PlayerTag.System := 'MP3';
          end;
        end
        // OGG: OggS
        else if (Buffer[0] = $4F) and (Buffer[1] = $67) and  // Og
                (Buffer[2] = $67) and (Buffer[3] = $53) then // gS
        begin
          if (play_OGG in FDefaultFormat) then
          begin
            Result := True;
            PlayerTag.PlayerType := ptDefault;
            PlayerTag.System := 'OGG';
          end;
        end
        // Quite OK Audio
        else if ((Buffer[0] = $71) and (Buffer[1] = $6F) and (Buffer[2] = $61) and
                 (Buffer[3] = $66)) then  // The Quite OK Audio Format for Fast, Lossy Compression
        begin
          if (play_QOA in FDefaultFormat) then
          begin
            Result := True;
            PlayerTag.PlayerType := ptDefault;
            PlayerTag.System := 'Quite OK Audio Format';
          end;
        end
      end;
    finally
      CloseFile(F);
    end;
  except
    Result := False;
  end;
end;

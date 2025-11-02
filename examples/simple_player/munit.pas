unit mUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, rAudioPlayer, rAudioIntf;

type

  { TForm1 }
  // Main form class for the audio player application
  TForm1 = class(TForm)
    // Audio player component that handles all playback functionality
    AudioPlayer: TrAudioPlayer;
    // UI controls
    bntPlay: TButton;
    bntPause: TButton;
    btnOpen: TButton;
    btnStop: TButton;
    eqBar1: TTrackBar;
    ComboBox1: TComboBox;
    eqBar2: TTrackBar;
    eqBar3: TTrackBar;
    eqBar4: TTrackBar;
    eqBar5: TTrackBar;
    eqBar6: TTrackBar;
    eqBar7: TTrackBar;
    eqBar8: TTrackBar;
    eqBar9: TTrackBar;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    msgBox: TListBox;        // Displays status messages and events
    infoBox: TListBox;       // Displays track information
    eqBar0: TTrackBar;
    OpenDialog: TOpenDialog; // File selection dialog
    Panel1: TPanel;
    StatusBar: TStatusBar;   // Shows current player status
    Timer1: TTimer;          // Updates UI periodically
    topPnl: TPanel;          // Top panel container
    PosBar: TTrackBar;       // Progress bar for track position
    mVolBar: TTrackBar;
    TrackBar2: TTrackBar;

    // Event handlers for audio player
    procedure AudioPlayerEnd(Sender: TObject; Track: integer;
      FinishedNormally: Boolean);
    procedure AudioPlayerError(Sender: TObject; const ErrorMsg: string);
    procedure AudioPlayerLoad(Sender: TObject; const FileName: string;
      TrackCount: Integer);
    procedure AudioPlayerPause(Sender: TObject; Track: integer);
    procedure AudioPlayerPlay(Sender: TObject; Track: integer);
    procedure AudioPlayerStop(Sender: TObject; Track: integer);

    // Event handlers for UI controls
    procedure bntPauseClick(Sender: TObject);
    procedure bntPlayClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);

    procedure ComboBox1Change(Sender: TObject);

    procedure FormActivate(Sender: TObject);
    procedure mVolBarChange(Sender: TObject);

    // Event handlers for progress bar
    procedure PosBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PosBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    // Timer event for updating UI
    procedure Timer1Timer(Sender: TObject);

    procedure TrackBar2Change(Sender: TObject);

    // Event handlers for individual EQ bands
    procedure eqBar0Change(Sender: TObject);
    procedure eqBar1Change(Sender: TObject);
    procedure eqBar2Change(Sender: TObject);
    procedure eqBar3Change(Sender: TObject);
    procedure eqBar4Change(Sender: TObject);
    procedure eqBar5Change(Sender: TObject);
    procedure eqBar6Change(Sender: TObject);
    procedure eqBar7Change(Sender: TObject);
    procedure eqBar8Change(Sender: TObject);
    procedure eqBar9Change(Sender: TObject);

  private
    // Private members would go here
    procedure ApplyEqualizerPreset(PresetIndex: Integer);
    procedure UpdateEQTrackBars;
    function TrackBarToGain(Position_: Integer): Single;
    function GainToTrackBar(Gain: Single): Integer;
  public
    // Public members would go here
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function MillisecondsToMinSec(Milliseconds: Integer): string;
var
  TotalSeconds: Integer;
  Minutes: Integer;
  Seconds: Integer;
begin
  // Convert milliseconds to total seconds (rounding down)
  TotalSeconds := Milliseconds div 1000;

  // Calculate minutes and seconds
  Minutes := TotalSeconds div 60;
  Seconds := TotalSeconds mod 60;

  // Format as "min:ss" with leading zero for seconds
  Result := Format('%d:%.2d', [Minutes, Seconds]);
end;

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
var
  libPath: string; // Path to audio libraries
begin
  // Set path to audio libraries based on platform and architecture
  // This is important for the audio player to find the required DLLs/so files

  {$if defined(cpu64) and defined(windows)}
   // libPath := 'win64';  // 64-bit Windows
    libPath := '..\..\library\win64';  // 64-bit Windows
  {$endif}

  {$if defined(cpu64) and defined(linux)}
    libPath := '../../library/lin64';  // 64-bit Linux
  {$endif}


  AudioPlayer.LibrariesPath := libPath;

  // Specify which audio libraries to use
  // These libraries support different audio formats
  AudioPlayer.LibrariesUses := [lib_rAudio, lib_Xmp, lib_SndFile,
                                 lib_VgmPlay, lib_WavPack, lib_ZxTune];

  // Initialize the audio player and load all specified libraries
  // This must be called before any playback operations
  AudioPlayer.InitializePlayers;
  AudioPlayer.LoopMode := True;

  // Initialize equalizer presets combo box
  ComboBox1.Clear;
  ComboBox1.Items.Add('Flat');
  ComboBox1.Items.Add('Bass Boost');
  ComboBox1.Items.Add('Treble Boost');
  ComboBox1.Items.Add('Rock');
  ComboBox1.Items.Add('Jazz');
  ComboBox1.Items.Add('Classical');
  ComboBox1.Items.Add('Pop');
  ComboBox1.Items.Add('Electronic');
  ComboBox1.Items.Add('Vocal Boost');
  ComboBox1.Items.Add('Custom');

  ComboBox1.ItemIndex := 0; // Flat preset by default

  // Initialize EQ trackbars range
  eqBar0.Min := -12;
  eqBar0.Max := 12;
  eqBar1.Min := -12;
  eqBar1.Max := 12;
  eqBar2.Min := -12;
  eqBar2.Max := 12;
  eqBar3.Min := -12;
  eqBar3.Max := 12;
  eqBar4.Min := -12;
  eqBar4.Max := 12;
  eqBar5.Min := -12;
  eqBar5.Max := 12;
  eqBar6.Min := -12;
  eqBar6.Max := 12;
  eqBar7.Min := -12;
  eqBar7.Max := 12;
  eqBar8.Min := -12;
  eqBar8.Max := 12;
  eqBar9.Min := -12;
  eqBar9.Max := 12;

  // Apply default flat preset
  ApplyEqualizerPreset(0);
end;

function TForm1.TrackBarToGain(Position_: Integer): Single;
begin
  Result := Position_; // Прямое преобразование, так как TrackBar уже настроен на -12..12
end;

function TForm1.GainToTrackBar(Gain: Single): Integer;
begin
  Result := Round(Gain); // Прямое преобразование
end;

procedure TForm1.UpdateEQTrackBars;
var
  i: Integer;
  Gain: Single;
begin
  // Обновляем позиции TrackBar'ов согласно текущим настройкам эквалайзера
  for i := 0 to 9 do
  begin
    Gain := AudioPlayer.GetEqualizerBand(i);
    case i of
      0: eqBar0.Position := GainToTrackBar(Gain);
      1: eqBar1.Position := GainToTrackBar(Gain);
      2: eqBar2.Position := GainToTrackBar(Gain);
      3: eqBar3.Position := GainToTrackBar(Gain);
      4: eqBar4.Position := GainToTrackBar(Gain);
      5: eqBar5.Position := GainToTrackBar(Gain);
      6: eqBar6.Position := GainToTrackBar(Gain);
      7: eqBar7.Position := GainToTrackBar(Gain);
      8: eqBar8.Position := GainToTrackBar(Gain);
      9: eqBar9.Position := GainToTrackBar(Gain);
    end;
  end;
end;

procedure TForm1.ApplyEqualizerPreset(PresetIndex: Integer);
var
  i: Integer;
begin
  case PresetIndex of
    0: // Flat - все полосы на 0 dB
      for i := 0 to 9 do
        AudioPlayer.SetEqualizerBand(i, 0.0);

    1: // Bass Boost - усиление низких частот
      begin
        AudioPlayer.SetEqualizerBand(0, 8.0);   // 32 Hz
        AudioPlayer.SetEqualizerBand(1, 6.0);   // 64 Hz
        AudioPlayer.SetEqualizerBand(2, 4.0);   // 125 Hz
        AudioPlayer.SetEqualizerBand(3, 2.0);   // 250 Hz
        AudioPlayer.SetEqualizerBand(4, 0.0);   // 500 Hz
        AudioPlayer.SetEqualizerBand(5, 0.0);   // 1 kHz
        AudioPlayer.SetEqualizerBand(6, 0.0);   // 2 kHz
        AudioPlayer.SetEqualizerBand(7, 0.0);   // 4 kHz
        AudioPlayer.SetEqualizerBand(8, 0.0);   // 8 kHz
        AudioPlayer.SetEqualizerBand(9, 0.0);   // 16 kHz
      end;

    2: // Treble Boost - усиление высоких частот
      begin
        AudioPlayer.SetEqualizerBand(0, 0.0);   // 32 Hz
        AudioPlayer.SetEqualizerBand(1, 0.0);   // 64 Hz
        AudioPlayer.SetEqualizerBand(2, 0.0);   // 125 Hz
        AudioPlayer.SetEqualizerBand(3, 0.0);   // 250 Hz
        AudioPlayer.SetEqualizerBand(4, 0.0);   // 500 Hz
        AudioPlayer.SetEqualizerBand(5, 0.0);   // 1 kHz
        AudioPlayer.SetEqualizerBand(6, 2.0);   // 2 kHz
        AudioPlayer.SetEqualizerBand(7, 4.0);   // 4 kHz
        AudioPlayer.SetEqualizerBand(8, 6.0);   // 8 kHz
        AudioPlayer.SetEqualizerBand(9, 8.0);   // 16 kHz
      end;

    3: // Rock - V-образная кривая
      begin
        AudioPlayer.SetEqualizerBand(0, 6.0);   // 32 Hz
        AudioPlayer.SetEqualizerBand(1, 4.0);   // 64 Hz
        AudioPlayer.SetEqualizerBand(2, 2.0);   // 125 Hz
        AudioPlayer.SetEqualizerBand(3, 0.0);   // 250 Hz
        AudioPlayer.SetEqualizerBand(4, -2.0);  // 500 Hz
        AudioPlayer.SetEqualizerBand(5, -1.0);  // 1 kHz
        AudioPlayer.SetEqualizerBand(6, 1.0);   // 2 kHz
        AudioPlayer.SetEqualizerBand(7, 3.0);   // 4 kHz
        AudioPlayer.SetEqualizerBand(8, 5.0);   // 8 kHz
        AudioPlayer.SetEqualizerBand(9, 6.0);   // 16 kHz
      end;

    4: // Jazz - теплый звук с акцентом на средние частоты
      begin
        AudioPlayer.SetEqualizerBand(0, 4.0);   // 32 Hz
        AudioPlayer.SetEqualizerBand(1, 3.0);   // 64 Hz
        AudioPlayer.SetEqualizerBand(2, 2.0);   // 125 Hz
        AudioPlayer.SetEqualizerBand(3, 1.0);   // 250 Hz
        AudioPlayer.SetEqualizerBand(4, 0.0);   // 500 Hz
        AudioPlayer.SetEqualizerBand(5, 2.0);   // 1 kHz
        AudioPlayer.SetEqualizerBand(6, 3.0);   // 2 kHz
        AudioPlayer.SetEqualizerBand(7, 2.0);   // 4 kHz
        AudioPlayer.SetEqualizerBand(8, 1.0);   // 8 kHz
        AudioPlayer.SetEqualizerBand(9, 0.0);   // 16 kHz
      end;

    5: // Classical - плоский с легким подъемом высоких
      begin
        AudioPlayer.SetEqualizerBand(0, 2.0);   // 32 Hz
        AudioPlayer.SetEqualizerBand(1, 1.0);   // 64 Hz
        AudioPlayer.SetEqualizerBand(2, 0.0);   // 125 Hz
        AudioPlayer.SetEqualizerBand(3, 0.0);   // 250 Hz
        AudioPlayer.SetEqualizerBand(4, 0.0);   // 500 Hz
        AudioPlayer.SetEqualizerBand(5, 0.0);   // 1 kHz
        AudioPlayer.SetEqualizerBand(6, 0.0);   // 2 kHz
        AudioPlayer.SetEqualizerBand(7, 1.0);   // 4 kHz
        AudioPlayer.SetEqualizerBand(8, 2.0);   // 8 kHz
        AudioPlayer.SetEqualizerBand(9, 3.0);   // 16 kHz
      end;

    6: // Pop - акцент на вокал и бас
      begin
        AudioPlayer.SetEqualizerBand(0, 4.0);   // 32 Hz
        AudioPlayer.SetEqualizerBand(1, 3.0);   // 64 Hz
        AudioPlayer.SetEqualizerBand(2, 1.0);   // 125 Hz
        AudioPlayer.SetEqualizerBand(3, 0.0);   // 250 Hz
        AudioPlayer.SetEqualizerBand(4, 1.0);   // 500 Hz
        AudioPlayer.SetEqualizerBand(5, 3.0);   // 1 kHz
        AudioPlayer.SetEqualizerBand(6, 4.0);   // 2 kHz
        AudioPlayer.SetEqualizerBand(7, 3.0);   // 4 kHz
        AudioPlayer.SetEqualizerBand(8, 2.0);   // 8 kHz
        AudioPlayer.SetEqualizerBand(9, 1.0);   // 16 kHz
      end;

    7: // Electronic - сильный бас и высокие
      begin
        AudioPlayer.SetEqualizerBand(0, 8.0);   // 32 Hz
        AudioPlayer.SetEqualizerBand(1, 6.0);   // 64 Hz
        AudioPlayer.SetEqualizerBand(2, 2.0);   // 125 Hz
        AudioPlayer.SetEqualizerBand(3, 0.0);   // 250 Hz
        AudioPlayer.SetEqualizerBand(4, -1.0);  // 500 Hz
        AudioPlayer.SetEqualizerBand(5, 0.0);   // 1 kHz
        AudioPlayer.SetEqualizerBand(6, 2.0);   // 2 kHz
        AudioPlayer.SetEqualizerBand(7, 4.0);   // 4 kHz
        AudioPlayer.SetEqualizerBand(8, 6.0);   // 8 kHz
        AudioPlayer.SetEqualizerBand(9, 6.0);   // 16 kHz
      end;

    8: // Vocal Boost - акцент на средние частоты для вокала
      begin
        AudioPlayer.SetEqualizerBand(0, 0.0);   // 32 Hz
        AudioPlayer.SetEqualizerBand(1, 0.0);   // 64 Hz
        AudioPlayer.SetEqualizerBand(2, 0.0);   // 125 Hz
        AudioPlayer.SetEqualizerBand(3, 1.0);   // 250 Hz
        AudioPlayer.SetEqualizerBand(4, 3.0);   // 500 Hz
        AudioPlayer.SetEqualizerBand(5, 6.0);   // 1 kHz
        AudioPlayer.SetEqualizerBand(6, 6.0);   // 2 kHz
        AudioPlayer.SetEqualizerBand(7, 4.0);   // 4 kHz
        AudioPlayer.SetEqualizerBand(8, 2.0);   // 8 kHz
        AudioPlayer.SetEqualizerBand(9, 0.0);   // 16 kHz
      end;

    9: // Custom - оставляем текущие настройки
      begin
        // Ничего не меняем - пользовательские настройки
      end;
  end;

  // Обновляем TrackBar'ы
  UpdateEQTrackBars;

  // Добавляем сообщение в лог
  msgBox.Items.Add('Equalizer preset: ' + ComboBox1.Items[PresetIndex]);
end;

procedure TForm1.mVolBarChange(Sender: TObject);
begin
  AudioPlayer.MasterVolume := mVolBar.Position;
end;

procedure TForm1.PosBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Stop timer when user starts dragging the progress bar
  // This prevents the timer from updating position while user is seeking
  Timer1.Enabled := False;
end;

procedure TForm1.PosBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Set new playback position when user releases the progress bar
  AudioPlayer.SetPosition(PosBar.Position);

  // Restart timer to continue UI updates
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  StatusText: string; // Text to display in status bar
begin
  // Update progress bar position to match current playback position
  PosBar.Position := AudioPlayer.GetPosition;

  // Set status text based on current player state
  case AudioPlayer.State of
    psStopped: StatusText := 'Stop';   // Player is stopped
    psPlaying: StatusText := 'Play';   // Player is playing
    psPaused:  StatusText := 'Pause';  // Player is paused
  end;

  // Update status bar with current state
  StatusBar.Panels[0].Text := StatusText;
  StatusBar.Panels[1].Text := MillisecondsToMinSec(AudioPlayer.GetPosition) +
  ' - ' + MillisecondsToMinSec(AudioPlayer.TrackInfo.Duration);
end;



procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  AudioPlayer.Balance := TrackBar2.Position ; // Масштабируем от -1.0 до +1.0
end;

// Обработчики изменений для отдельных полос эквалайзера
procedure TForm1.eqBar0Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(0, TrackBarToGain(eqBar0.Position));
end;

procedure TForm1.eqBar1Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(1, TrackBarToGain(eqBar1.Position));
end;

procedure TForm1.eqBar2Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(2, TrackBarToGain(eqBar2.Position));
end;

procedure TForm1.eqBar3Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(3, TrackBarToGain(eqBar3.Position));
end;

procedure TForm1.eqBar4Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(4, TrackBarToGain(eqBar4.Position));
end;

procedure TForm1.eqBar5Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(5, TrackBarToGain(eqBar5.Position));
end;

procedure TForm1.eqBar6Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(6, TrackBarToGain(eqBar6.Position));
end;

procedure TForm1.eqBar7Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(7, TrackBarToGain(eqBar7.Position));
end;

procedure TForm1.eqBar8Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(8, TrackBarToGain(eqBar8.Position));
end;

procedure TForm1.eqBar9Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := 9; // Переключаем на Custom
  AudioPlayer.SetEqualizerBand(9, TrackBarToGain(eqBar9.Position));
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  // Show file open dialog to select audio file
  if OpenDialog.Execute then begin
    // Load the selected audio file
    AudioPlayer.OpenMusicFile(OpenDialog.FileName);

    // Update window title with track title
    Caption := AudioPlayer.TrackInfo.Title;

    // Clear previous track info and display new information
    infoBox.Clear;
    infoBox.Items.Add('Library: ' + AudioPlayer.GetCurrentEngine); // Which library is handling playback
    infoBox.Items.Add('Title: ' + AudioPlayer.TrackInfo.Title);         // Track title
    infoBox.Items.Add('Artist: ' + AudioPlayer.TrackInfo.Artist);       // Artist name
    infoBox.Items.Add('Duration: ' + MillisecondsToMinSec(AudioPlayer.TrackInfo.Duration)); // Duration (commented out)
    infoBox.Items.Add('System: ' + AudioPlayer.TrackInfo.System);       // Audio system/game system
    infoBox.Items.Add('Tracker: ' + AudioPlayer.TrackInfo.Tracker);     // Tracker/module format
  end;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  // Stop audio playback
  AudioPlayer.Stop;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  // Применяем выбранный пресет эквалайзера
  ApplyEqualizerPreset(ComboBox1.ItemIndex);
end;

procedure TForm1.bntPlayClick(Sender: TObject);
begin
  // If audio is paused, resume playback
  if AudioPlayer.IsPaused then
    AudioPlayer.Resume
  else
    // Otherwise start playing from beginning
    AudioPlayer.Play;

  // Set progress bar maximum to track duration
  PosBar.Max := AudioPlayer.GetDuration;
  mVolBar.Position := Round(AudioPlayer.MasterVolume * 100);
end;

procedure TForm1.AudioPlayerLoad(Sender: TObject; const FileName: string;
  TrackCount: Integer);
begin
  // Event called when an audio file is successfully loaded
  msgBox.Items.Add('OnLoad: ' + FileName);
end;

procedure TForm1.AudioPlayerPause(Sender: TObject; Track: integer);
begin
  // Event called when playback is paused
  msgBox.Items.Add('OnPause: ' + AudioPlayer.CurrentFile);
end;

procedure TForm1.AudioPlayerPlay(Sender: TObject; Track: integer);
begin
  // Event called when playback starts
  msgBox.Items.Add('OnPlay: ' + AudioPlayer.CurrentFile);
end;

procedure TForm1.AudioPlayerStop(Sender: TObject; Track: integer);
begin
  // Event called when playback stops
  msgBox.Items.Add('OnStop: ' + AudioPlayer.CurrentFile);
end;

procedure TForm1.bntPauseClick(Sender: TObject);
begin
  // Pause current playback
  AudioPlayer.Pause;
end;

procedure TForm1.AudioPlayerError(Sender: TObject; const ErrorMsg: string);
begin
  // Event called when an error occurs during playback
  msgBox.Items.Add('OnError: ' + ErrorMsg);

  // Reset progress bar on error
  PosBar.Max := 0;
end;

procedure TForm1.AudioPlayerEnd(Sender: TObject; Track: integer;
  FinishedNormally: Boolean);
begin
  // Event called when track finishes playing
  msgBox.Items.Add('OnEnd: ' + AudioPlayer.CurrentFile);
end;

end.

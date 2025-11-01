unit rAudioIntf;

{$mode objfpc}{$H+}

interface
uses
  Classes;

const
   BANDS_COUNT = 10;

type
  TPlayerState = (psStopped, psPlaying, psPaused);
  TPlayEvent = procedure(Sender: TObject; Track: integer) of object;
  TPauseEvent = procedure(Sender: TObject; Track: integer) of object;
  TEndEvent = procedure(Sender: TObject; Track: integer; FinishedNormally: Boolean) of object;
  TStopEvent = procedure(Sender: TObject; Track: integer) of object;
  TErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TLoadEvent = procedure(Sender: TObject; const FileName: string; TrackCount: Integer) of object;

  // Типы для эквалайзера
  TEqualizerBand = record
    Gain: Single;    // Усиление/ослабление полосы (-12dB до +12dB)
    Frequency: Single; // Центральная частота
    Q: Single;       // Добротность
  end;

  // Коэффициенты фильтра для каждой полосы эквалайзера
  TFilterCoeffs = record
    a0, a1, a2: Single;
    b1, b2: Single;
  end;

  { IMusicPlayer }
  IMusicPlayer = interface // интерфейс аудиоплеера
    ['{D84CD174-8EB8-4289-9CD1-43C4E65E0950}']
    function GetBalance: Single;
    function GetOnEnd: TEndEvent;
    function GetOnError: TErrorEvent;
    function GetOnLoad: TLoadEvent;
    function GetOnPause: TPauseEvent;
    function GetOnPlay: TPlayEvent;
    function GetOnStop: TStopEvent;
    function GetPlayerState: TPlayerState;
    function GetTrackNumber: Integer;
    procedure SetBalance(AValue: Single);
    procedure SetTrackNumber(AValue: Integer);
    procedure SetOnLoad(AEvent: TLoadEvent);

    // Основные методы управления
    procedure OpenMusicFile(const MusicFile: String); // Загрузка файла без воспроизведения
    procedure Play; // Воспроизведение текущего трека
    procedure Pause;
    procedure Resume;
    procedure SetOnEnd(AEvent: TEndEvent);
    procedure SetOnError(AEvent: TErrorEvent);
    procedure SetOnPause(AEvent: TPauseEvent);
    procedure SetOnPlay(AEvent: TPlayEvent);
    procedure SetOnStop(AEvent: TStopEvent);
    procedure Stop;

    // Управление позицией
    procedure SetPosition(PositionMs: Integer); // Устанавливает позицию воспроизведения в миллисекундах (0 = начало трека)
    function GetPosition: Integer;
    function GetDuration: Integer;

    // Управление режимом повтора
    procedure SetLoopMode(Mode: Boolean);
    function GetLoopMode: Boolean;

    // Состояние плеера
    function IsPlaying: Boolean;
    function IsPaused: Boolean;
    function IsStopped: Boolean;
    function GetState: TPlayerState;

    // Информация о треке
    function GetCurrentTrack: Integer;
    function GetCurrentFile: String;
    function GetTrackCount: Integer;

    // Эквалайзер
    procedure SetEqualizerBand(BandIndex: Integer; Gain: Single);
    function GetEqualizerBand(BandIndex: Integer): Single;
    procedure ResetEqualizer;

    // Свойства
    property TrackNumber: Integer read GetTrackNumber write SetTrackNumber;
    property State: TPlayerState read GetPlayerState;
    property Balance: Single read GetBalance write SetBalance;

    // События
    property OnPlay: TPlayEvent read GetOnPlay write SetOnPlay;
    property OnPause: TPauseEvent read GetOnPause write SetOnPause;
    property OnStop: TStopEvent read GetOnStop write SetOnStop;
    property OnEnd: TEndEvent read GetOnEnd write SetOnEnd;
    property OnError: TErrorEvent read GetOnError write SetOnError;
    property OnLoad: TLoadEvent read GetOnLoad write SetOnLoad;
  end;

implementation

end.

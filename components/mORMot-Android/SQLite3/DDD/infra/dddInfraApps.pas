/// shared DDD Infrastructure: Application/Daemon implementation classes
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraApps;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2015 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2015
  the Initial Developer. All Rights Reserved.

  Contributor(s):


  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18

  TODO:
   - store settings in database
   - allow to handle authentication via a centralized service or REST server

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  mORMotService, // for running the daemon as a regular Windows Service
  {$else}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  Variants,
  SynCommons,
  SynLog,
  mORMot,
  mORMotDDD,
  dddInfraSettings,
  SynCrtSock,
  SynBidirSock,
  mORMotHttpServer, // for publishing a TSQLRestServer over HTTP
  mORMotHttpClient; // for publishing a TSQLRestClientURI over HTTP


{ ----- Implements Service/Daemon Applications }

type
  /// abstract class to handle any kind of service/daemon executable
  // - would implement either a Windows Service, a stand-alone remotely
  // administrated daemon, or a console application, according to the command line
  // - you should inherit from this class, then override the abstract NewDaemon
  // protected method to launch and return a IAdministratedDaemon instance
  {$M+}
  TDDDDaemon = class
  protected
    fSettings: TDDDAdministratedDaemonSettings;
    fSettingsRef: IUnknown;
    /// the service/daemon will be stopped when this interface is set to nil
    fDaemon: IAdministratedDaemon;
    /// this abstract method should be overriden to return a new service/daemon
    // instance, using the (inherited) fSettings as parameters
    function NewDaemon: TDDDAdministratedDaemon; virtual;
    {$ifdef MSWINDOWS} // to support Windows Services
    procedure DoStart(Sender: TService);
    procedure DoStop(Sender: TService);
    {$endif}
  public
    /// initialize the service/daemon application thanks to some information
    // - actual settings would inherit from TDDDAdministratedDaemonSettingsFile,
    // to define much more parameters, according to the service/daemon process
    // - the supplied settings will be owned by this TDDDDaemon instance
    constructor Create(aSettings: TDDDAdministratedDaemonSettings); virtual;
    /// finalize the service/daemon application, and release its resources
    destructor Destroy; override;
    /// interprect the command line to run the application as expected
    // - under Windows, /install /uninstall /start /stop would control the
    // daemon as a Windows Service - you should run the program with
    // administrator rights to be able to change the Services settings
    // - /console or /c would run the program as a console application
    // - /verbose would run the program as a console application, in verbose mode
    // - /daemon or /d would run the program as a remotely administrated
    // daemon, using a published IAdministratedDaemon service
    // - /version would show the current revision information of the application
    // - no command line argument will run the program as a Service dispatcher
    // under Windows (as a regular service), or display the syntax
    // - any invalid switch, or no switch under Linux, will display the syntax
    // - this method will output any error or information to the console
    // - as a result, a project .dpr could look like the following:
    //!begin
    //!  with TMyApplicationDaemon.Create(TMyApplicationDaemonSettings.Create) do
    //!  try
    //!    ExecuteCommandLine;
    //!  finally
    //!    Free;
    //!  end;
    //!end.
    procedure ExecuteCommandLine;
    /// start the daemon, until the instance is released
    procedure Execute;
    /// read-only access to the underlying daemon instance
    // - equals nil if the daemon is not started
    property Daemon: IAdministratedDaemon read fDaemon;
    /// returns the class instance implementing the underlying Daemon
    // - you should transtype the returned instance using e.g.
    // !  myDaemon := mainDaemon.DaemonInstance as TMyDaemonClass;
    function DaemonInstance: TObject;
  published
  end;
  {$M-}

  /// abstract class to implement a IAdministratedDaemon service via a TThread
  // - as hosted by TDDDDaemon service/daemon application
  TDDDThreadDaemon = class(TDDDAdministratedThreadDaemon)
  protected
    // returns the system memory info as current state
    function InternalRetrieveState(var Status: variant): boolean; override;
    function GetAdministrationHTTPServer: TSQLHttpServer;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// reference to the HTTP server publishing IAdministratedDaemon service
    // - may equal nil if TDDDAdministratedDaemonSettingsFile.AuthHttp.BindPort=''
    property AdministrationHTTPServer: TSQLHttpServer read GetAdministrationHTTPServer;
  end;

  /// abstract class to implement a IAdministratedDaemon service via a TSQLRestServer
  // - as hosted by TDDDDaemon service/daemon application
  TDDDRestDaemon = class(TDDDAdministratedRestDaemon)
  protected
    fPreviousMonitorTix: Int64;
    function Settings: TDDDAdministratedDaemonHttpSettings;
      {$ifdef HASINLINE}inline;{$endif}
    function GetAdministrationHTTPServer: TSQLHttpServer;
      {$ifdef HASINLINE}inline;{$endif}
    // returns the current state from fRest.Stat() + system memory
    function InternalRetrieveState(var Status: variant): boolean; override;
    procedure InternalLogMonitoring; virtual;
  public
    /// reference to the HTTP server publishing IAdministratedDaemon service
    // - may equal nil if TDDDAdministratedDaemonSettingsFile.AuthHttp.BindPort=''
    property AdministrationHTTPServer: TSQLHttpServer read GetAdministrationHTTPServer;
  end;

  /// abstract class to implement a IAdministratedDaemon service via a
  // TSQLRestServer, publishing its services as HTTP
  // - as hosted by TDDDDaemon service/daemon application
  TDDDRestHttpDaemon = class(TDDDRestDaemon)
  protected
    fHttpServer: TSQLHttpServer;
    // initialize HTTP Server into fHttpServer
    // (fRest should have been set by the overriden method)
    procedure InternalStart; override;
    // finalize HTTP Server
    procedure InternalStop; override;
  public
    /// reference to the main HTTP server publishing this daemon Services
    // - may be nil outside a Start..Stop range
    property HttpServer: TSQLHttpServer read fHttpServer;
  end;


{ ----- Implements ORM/SOA REST Client access }

type
  /// exception raised by TDDDRestClientSettings classes
  EDDDRestClient = class(EDDDException);

  /// advanced parameters for TDDDRestClientSettings definition 
  TDDDRestClient = class(TSynPersistentWithPassword)
  protected
    fRoot: RawUTF8;
  published
    /// the URI Root to be used for the REST Model
    property Root: RawUTF8 read fRoot write fRoot;
    /// the encrypted password to be used to connect with WebSockets
    property WebSocketsPassword: RawUTF8 read fPassWord write fPassWord;
  end;

  /// storage class for initializing an ORM/SOA REST Client class
  // - this class will contain some generic properties to initialize a
  // TSQLRestClientURI pointing to a remote server, using WebSockets by default
  // - WebSockets support is the reason why this class is defined in
  // dddInfraApps, and not dddInfraSettings
  TDDDRestClientSettings = class(TSynAutoCreateFields)
  protected
    fORM: TSynConnectionDefinition;
    fClient: TDDDRestClient;
    function OnAuthentificationFailed(Retry: integer;
      var aUserName,aPassword: string; out aPasswordHashed: boolean): boolean;
  public
    /// set the default values for Client.Root, ORM.ServerName,
    // Client.WebSocketsPassword and ORM.Password
    procedure SetDefaults(const Root,Port,WebSocketPassword,UserPassword: RawUTF8;
      const User: RawUTF8='User');
    /// is able to instantiate a Client REST instance for the stored definition
    // - Definition.Kind is expected to specify a TSQLRestClient class to be
    // instantiated, not a TSQLRestServer instance
    // - will return nil if the supplied Definition is not correct
    // - note that the supplied Model.Root is expected to be the default root
    // URI, which will be overriden with this TDDDRestSettings.Root property
    // - will also set the TSQLRest.LogFamily.Level from LogLevels value,
    function NewRestClientInstance(aRootSettings: TDDDAppSettingsAbstract;
      aModel: TSQLModel=nil;
      aOptions: TDDDNewRestInstanceOptions=[riOwnModel,riCreateVoidModelIfNone,
        riHandleAuthentication,riRaiseExceptionIfNoRest]): TSQLRestClientURI;
  published
    /// defines a mean of access to a TSQLRest instance
    // - using Kind/ServerName/DatabaseName/User/Password properties: Kind
    // would define the TSQLRest class to be instantiated by NewRestClientInstance()
    property ORM: TSynConnectionDefinition read fORM;
    /// advanced connection options
    // - ORM.Password defines the authentication main password, and
    // Client.WebSocketsPassword is used for WebSockets binary encryption
    property Client: TDDDRestClient read fClient;
  end;

/// create a client safe asynchronous connection to a IAdministratedDaemon service
function AdministratedDaemonClient(Definition: TDDDRestClientSettings;
   Model: TSQLModel=nil): TSQLHttpClientWebsockets;

/// create a WebSockets server instance, publishing a IAdministratedDaemon service
function AdministratedDaemonServer(Settings: TDDDAdministratedDaemonSettings;
  DaemonClass: TDDDAdministratedDaemonClass): TDDDAdministratedDaemon;


{ ----- Implements Thread Processing to access a TCP server }

type
  TDDDSocketThread = class;

  /// the current connection state of the TCP client associated to a
  // TDDDSocketThread thread
  TDDDSocketThreadState = (tpsDisconnected, tpsConnecting, tpsConnected);

  /// the monitoring information of a TDDDSocketThread thread
  TDDDSocketThreadMonitoring = class(TDDDAdministratedDaemonMonitor)
  protected
    FState: TDDDSocketThreadState;
    fOwner: TDDDSocketThread;
    function GetSocket: variant;
  published
    /// how this thread is currently connected to its associated TCP server
    property State: TDDDSocketThreadState read FState write FState;
    /// information about the associated socket
    property Socket: variant read GetSocket;
  end;

  /// interface allowing to customize/mock a socket connection
  IDDDSocket = interface
    /// connect to the host via the (mocked) socket
    // - should raise an exception on error
    procedure Connect;
    /// returns instance identifier
    // - e.g. the TCrtSocket.Sock number as text
    function Identifier: RawUTF8;
    /// get some low-level information about the last occurred error
    // - e.g. TCrtSocket.LastLowSocketError value
    function LastError: RawUTF8;
    /// returns the number of bytes pending in the (mocked) socket
    // - call e.g. TCrtSocket.SockInPending() method
    function DataInPending(aTimeOut: integer): integer;
    /// get Length bytes from the (mocked) socket
    // - returns the number of bytes read into the Content buffer
    // - call e.g. TCrtSocket.SockInRead() method
    function DataIn(Content: PAnsiChar; ContentLength: integer): integer;
    /// send Length bytes to the (mocked) socket
    // - returns false on any error, true on success
    // - call e.g. TCrtSocket.TrySndLow() method
    function DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
  end;

  /// implements IDDDSocket using a SynCrtSock.TCrtSocket instance
  // - used e.g. by TDDDSocketThread for its default network communication
  // - this class will also create two mutexes, one for DataIn/DataInPending,
  // another for DataOut thread-safe process
  TDDDSynCrtSocket = class(TInterfacedObjectLocked,IDDDSocket)
  protected
    fSocket: TCrtSocket;
    fOwner: TThread;
    fHost,fPort: SockString;
    fInternalBufferSize: integer;
    fOutput: TSynLocker; // input lock is TInterfacedObjectLocked.Safe
  public
    /// initialize the internal TCrtSocket instance
    constructor Create(aOwner: TThread; const aHost,aPort: SockString;
      aSocketTimeout,aInternalBufferSize: integer); reintroduce; virtual;
    /// finalize the internal TCrtSocket instance
    destructor Destroy; override;
    /// call TCrtSocket.OpenBind
    procedure Connect;
    /// returns TCrtSocket.Sock number as text
    function Identifier: RawUTF8;
    /// get information from TCrtSocket.LastLowSocketError
    function LastError: RawUTF8;
    /// call TCrtSocket.SockInPending() method
    function DataInPending(aTimeOut: integer): integer;
    /// call TCrtSocket.SockInRead() method
    function DataIn(Content: PAnsiChar; ContentLength: integer): integer;
    /// call TCrtSocket.TrySndLow() method
    function DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
    /// read-only access to the associated processing thread
    // - not published, to avoid stack overflow since TDDDSocketThreadMonitoring
    // would point to this instance
    property Owner: TThread read fOwner;
  published
    /// read-only access to the associated processing socket
    property Socket: TCrtSocket read fSocket;
  end;

  /// defines the potential mocked actions for TDDDMockedSocket.MockException()
  TDDDMockedSocketException = (
    msaConnectRaiseException,
    msaDataInPendingTimeout,
    msaDataInPendingFails,
    msaDataInRaiseException,
    msaDataOutRaiseException,
    msaDataOutReturnsFalse);

  /// defines a set of mocked actions for TDDDMockedSocket.MockException()
  TDDDMockedSocketExceptions = set of TDDDMockedSocketException;

  /// defines the potential mocked actions for TDDDMockedSocket.MockLatency()
  TDDDMockedSocketLatency = (
    mslConnect,
    mslDataIn,
    mslDataOut);

  /// defines a set of mocked actions for TDDDMockedSocket.MockLatency()
  TDDDMockedSocketLatencies = set of TDDDMockedSocketLatency;

  /// the default exception class raised by TDDDMockedSocket
  EDDDMockedSocket = class(EDDDInfraException);

  /// implements IDDDSocket using a fake/mocked in-memory input/ouput storage
  // - may be supplied to TDDDSocketThread to bypass its default network communication
  // - you could fake input/output of TCP/IP packets by calling MockDataIn() and
  // MockDataOut() methods - incoming and outgoing packets would be merged in
  // the internal in-memory buffers, as with a regular Socket
  // - you could fake exception, for any upcoming method call, via MockException() 
  // - you could emulate network latency, for any upcoming method call, via
  // MockLatency() - to emulate remote/wireless access, or thread pool contention 
  // - this implementation is thread-safe, so multiple threads could access
  // the same IDDDSocket instance, and settings be changed in real time
  TDDDMockedSocket = class(TInterfacedObjectLocked,IDDDSocket)
  protected
    fInput,fOutput: RawByteString;
    fExceptionActions: TDDDMockedSocketExceptions;
    fExceptionMessage: string;
    fExceptionClass: ExceptClass;
    fLatencyActions: TDDDMockedSocketLatencies;
    fLatencyMS: integer;
    fOwner: TThread;
    function GetPendingInBytes: integer;
    function GetPendingOutBytes: integer;
    procedure CheckLatency(Action: TDDDMockedSocketLatency);
    procedure CheckRaiseException(Action: TDDDMockedSocketException);
  public
    /// initialize the mocked socket instance 
    constructor Create(aOwner: TThread); reintroduce; virtual; 
    /// add some bytes to the internal fake input storage
    // - would be made accessible to the DataInPending/DataIn methods
    // - the supplied buffer would be gathered to any previous MockDataIn()
    // call, which has not been read yet by the DataIn() method
    procedure MockDataIn(const Content: RawByteString);
    /// return the bytes from the internal fake output storage
    // - as has be previously set by the DataOut() method
    // - will gather all data from several DataOut() calls in a single buffer 
    function MockDataOut: RawByteString;
    /// the specified methods would raise an exception
    // - only a single registration is memorized: once raised, any further
    // method execution would continue as usual
    // - optional Exception.Message which should be raised with the exception
    // - also optional exception class instead of default EDDDMockedSocket
    // - msaDataOutReturnsFalse won't raise any exception, but let DataOut
    // method return false (which is the normal way of indicating a socket
    // error) - in this case, ExceptionMessage would be available from LastError
    // - msaDataInPendingTimeout won't raise any exception, but let DataInPending
    // sleep for the timeout period, and return 0
    // - msaDataInPendingFails won't raise any exception, but let DataInPending
    // fails immediately, and return -1 (emulating a broken socket)
    // - you may use ALL_DDDMOCKED_EXCEPTIONS to set all possible actions
    // - you could reset any previous registered exception by calling
    // ! MockException([]);
    procedure MockException(NextActions: TDDDMockedSocketExceptions;
      const ExceptionMessage: string=''; ExceptionClass: ExceptClass=nil);
    /// will let the specified methods to wait for a given number of milliseconds
    // - allow to emulate network latency, on purpose
    // - you may use ALL_DDDMOCKED_LATENCIES to slow down all possible actions
    procedure MockLatency(NextActions: TDDDMockedSocketLatencies;
      MilliSeconds: integer);
  public 
    /// IDDDSocket method to connect to the host via the mocked socket
    // - won't raise any exception unless ConnectShouldCheckRaiseException is set
    procedure Connect;
    /// IDDDSocket method to return a fake instance identifier
    // - in fact, the hexa pointer of the TDDDMockedSocket instance
    function Identifier: RawUTF8;
    /// IDDDSocket method to  get some low-level information about the last error
    // - i.e. the latest ExceptionMessage value as set by MockException()
    function LastError: RawUTF8;
    /// IDDDSocket method to return the number of bytes pending
    // - note that the total length of all pending data is returned as once,
    // i.e. all previous calls to MockDataIn() would be sum as a single count
    // - this method will emulate blocking process, just like a regular socket:
    // if there is no pending data, it will wait up to aTimeOut milliseconds
    function DataInPending(aTimeOut: integer): integer;
    /// IDDDSocket method to get Length bytes from the mocked socket
    // - returns the number of bytes read into the Content buffer
    // - note that all pending data is returned as once, i.e. all previous
    // calls to MockDataIn() would be gathered in a single buffer
    function DataIn(Content: PAnsiChar; ContentLength: integer): integer;
    /// IDDDSocket method to send Length bytes to the mocked socket
    // - returns false on any error, true on success
    // - then MockDataOut could be used to retrieve the sent data
    function DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
    /// read-only access to the associated processing thread
    // - not published, to avoid stack overflow since TDDDSocketThreadMonitoring
    // would point to this instance
    property Owner: TThread read fOwner;
  published
    /// how many bytes are actually in the internal input buffer
    property PendingInBytes: integer read GetPendingInBytes;
    /// how many bytes are actually in the internal output buffer
    property PendingOutBytes: integer read GetPendingOutBytes;
  end;

  /// a generic TThread able to connect and reconnect to a TCP server
  // - initialize and own a TCrtSocket instance for TCP transmission
  // - allow automatic reconnection
  // - inherit from TSQLRestThread, so should be associated with a REST instance
  TDDDSocketThread = class(TSQLRestThread)
  protected
    fSettings: TDDDSocketThreadSettings;
    fMonitoring: TDDDSocketThreadMonitoring;
    fPreviousMonitorTix: Int64;
    fSocket: IDDDSocket;
    fPerformConnection: boolean;
    fHost, fPort: SockString;
    fSocketInputBuffer: RawByteString;
    fExecuteSocketLoopPeriod: integer;
    fShouldDisconnect: boolean;
    procedure InternalExecute; override;
    procedure ExecuteConnect;
    procedure ExecuteDisconnect;
    procedure ExecuteDisconnectAfterError;
    procedure ExecuteSocket;
    function TrySend(const aFrame: RawByteString;
      ImmediateDisconnectAfterError: boolean=true): Boolean; virtual;
    // inherited classes could override those methods for process customization
    procedure InternalExecuteConnected; virtual;
    procedure InternalExecuteDisconnect; virtual;
    procedure InternalExecuteIdle; virtual;
    procedure InternalExecuteSocket; virtual; abstract; // process FSocketInputBuffer
    procedure InternalLogMonitoring; virtual;
  public
    /// initialize the thread for a given REST instance
    constructor Create(aSettings: TDDDSocketThreadSettings; aRest: TSQLRest;
      aMonitoring: TDDDSocketThreadMonitoring);
    /// finalize the thread process, and its associted REST instance
    destructor Destroy; override;
    /// returns the Monitoring and Rest statistics as a JSON object
    // - resulting format is
    // $ {...MonitoringProperties...,"Rest":{...RestStats...}}
    function StatsAsJson: RawUTF8;
    /// the parameters used to setup this thread process
    property Settings: TDDDSocketThreadSettings read fSettings;
  published
    /// the IP Host name used to connect with TCP
    property Host: SockString read fHost;
    /// the IP Port value used to connect with TCP
    property Port: SockString read fPort;
  end;


const
  /// map realistic exceptions steps for a mocked socket
  // - could be used to simulate a global socket connection drop
  ALL_DDDMOCKED_EXCEPTIONS =
    [msaConnectRaiseException,msaDataInPendingFails,
     msaDataInRaiseException,msaDataOutReturnsFalse];

  /// map realistic latencies steps for a mocked socket
  // - could be used to simulate a slow network
  ALL_DDDMOCKED_LATENCIES =
    [Low(TDDDMockedSocketLatency)..high(TDDDMockedSocketLatency)];


var
  /// you could set a text to this global variable at runtime, so that
  // it would be displayed as copyright older name for the console
  GlobalCopyright: string = '';
  

implementation


{ ----- Implements Service/Daemon Applications }

{ TDDDDaemon }

constructor TDDDDaemon.Create(aSettings: TDDDAdministratedDaemonSettings);
begin
  inherited Create;
  if aSettings=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(settings=nil)',[self]);
  fSettings := aSettings;
  fSettingsRef := aSettings;
end;

function TDDDDaemon.DaemonInstance: TObject;
begin
  result := ObjectFromInterface(fDaemon);
end;

destructor TDDDDaemon.Destroy;
begin
  fDaemon := nil;
  inherited;
end;

{$ifdef MSWINDOWS} // to support Windows Services

procedure TDDDDaemon.DoStart(Sender: TService);
begin
  SQLite3Log.Enter(self);
  fDaemon := NewDaemon;
  fDaemon.Start;
end;

procedure TDDDDaemon.DoStop(Sender: TService);
begin
  SQLite3Log.Enter(self);
  fDaemon := nil; // will stop the daemon
end;

{$endif MSWINDOWS} // to support Windows Services

function TDDDDaemon.NewDaemon: TDDDAdministratedDaemon;
begin
  if Assigned(fSettings) and fSettings.Log.LowLevelWebSocketsFrames then begin
    WebSocketLog := SQLite3Log;
    HttpServerFullWebSocketsLog := true;
    HttpClientFullWebSocketsLog := true;
  end;
  result := nil;
end;

procedure TDDDDaemon.Execute;
begin
  SQLite3Log.Enter(self);
  fDaemon := NewDaemon;
  fDaemon.Start;
end;

type
  TExecuteCommandLineCmd = (
    cNone,cInstall,cUninstall,cStart,cStop,cState,cVersion,cVerbose,
    cHelp,cConsole,cDaemon);

procedure TDDDDaemon.ExecuteCommandLine;
var name,param: RawUTF8;
    cmd: TExecuteCommandLineCmd;
    daemon: TDDDAdministratedDaemon;
    {$ifdef MSWINDOWS}
    service: TServiceSingle;
    ctrl: TServiceController;
    depend: string;
    i: integer;
    {$endif}
{$I-} // no IO error for writeln() below

  function cmdText: RawUTF8;
  begin
    result := GetEnumNameTrimed(TypeInfo(TExecuteCommandLineCmd),cmd);
  end;
  procedure Show(Success: Boolean);
  var msg: RawUTF8;
      error: integer;
  begin
    if Success then begin
      msg := 'Successfully executed';
      TextColor(ccWhite);
    end else begin
      error := GetLastError;
      msg := FormatUTF8('Error % "%" occured with',
        [error,StringToUTF8(SysErrorMessage(error))]);
      TextColor(ccLightRed);
      ExitCode := 1; // notify error to caller batch
    end;
    msg := FormatUTF8('% "%" (%) on Service "%"',
      [msg,param,cmdText,fSettings.ServiceName]);
    AppendToTextFile(msg,ChangeFileExt(ExeVersion.ProgramFileName,'.txt'));
    writeln(msg);
  end;
  procedure Syntax;
  begin
    writeln('Try with one of the switches:');
    writeln(ExeVersion.ProgramName,' /console -c /verbose /daemon -d /help -h /version');
    {$ifdef MSWINDOWS}
    writeln(ExeVersion.ProgramName,' /install /uninstall /start /stop /state');
    {$endif}
  end;

begin
  try
    if fSettings.ServiceDisplayName='' then begin
      fDaemon := NewDaemon; // should initialize the default .settings
      fDaemon := nil;
    end;
    TextColor(ccLightGreen);
    name := StringToUTF8(fSettings.ServiceDisplayName);
    if name='' then // perhaps the settings file is still void
      name := ExeVersion.ProgramName;
    if ExeVersion.Version.Version32<>0 then
      name := FormatUTF8('% %',[name,ExeVersion.Version.Detailed]);
    writeln(#10' ',name);
    writeln(StringOfChar('-',length(name)+2));
    TextColor(ccGreen);
    if fSettings.Description<>'' then
      writeln(fSettings.Description);
    if GlobalCopyright<>'' then
      writeln('(c)',CurrentYear,' ',GlobalCopyright);
    writeln;
    TextColor(ccLightCyan);
    param := trim(StringToUTF8(paramstr(1)));
    if (param='') or not(param[1] in ['/','-']) then
      cmd := cNone else
      case param[2] of
      'c','C': cmd := cConsole;
      'd','D': cmd := cDaemon;
      'h','H': cmd := cHelp;
      else byte(cmd) := 1+IdemPCharArray(@param[2],[
        'INST','UNINST','START','STOP','STAT','VERS','VERB']);
      end;
    case cmd of
    cHelp:
      Syntax;
    cVersion: begin
      if ExeVersion.Version.Version32<>0 then
        writeln(ExeVersion.ProgramName,' Version ',ExeVersion.Version.Detailed);
      TextColor(ccCyan);
      writeln('Powered by Synopse mORMot '+SYNOPSE_FRAMEWORK_VERSION);
    end;
    cConsole,cDaemon,cVerbose: begin
      writeln('Launched in ',cmdText,' mode'#10);
      TextColor(ccLightGray);
      case cmd of
      cConsole:
        SQLite3Log.Family.EchoToConsole := LOG_STACKTRACE+[sllDDDInfo];
      cVerbose:
        SQLite3Log.Family.EchoToConsole := LOG_VERBOSE;
      end;
      daemon := NewDaemon;
      try
        fDaemon := daemon;
        {$ifdef WITHLOG}
        if cmd=cDaemon then
          if (daemon.AdministrationServer=nil) or
             not ({$ifdef MSWINDOWS}
                   daemon.AdministrationServer.ExportedAsMessageOrNamedPipe or{$endif}
                  (daemon.InheritsFrom(TDDDThreadDaemon) and
                   (TDDDThreadDaemon(daemon).fAdministrationHTTPServer<>nil))) then
            daemon.Log.Synlog.Log(sllWarning,'ExecuteCommandLine as Daemon '+
              'without external admnistrator acccess',self);
        {$endif}
        daemon.Execute(cmd=cDaemon);
      finally
        fDaemon := nil; // will stop the daemon
      end;
    end;
    else
    {$ifdef MSWINDOWS} // implement the daemon as a Windows Service
      with fSettings do
      if ServiceName='' then
        if cmd=cNone then
          Syntax else begin
          TextColor(ccLightRed);
          writeln('No ServiceName specified - please fix the setttings');
        end else
      case cmd of
      cNone:
        if param='' then begin // executed as a background service
          service := TServiceSingle.Create(ServiceName,ServiceDisplayName);
          try
            service.OnStart := DoStart;
            service.OnStop := DoStop;
            service.OnShutdown := DoStop; // sometimes, is called without Stop
            if ServicesRun then // blocking until service shutdown
              Show(true) else
              if GetLastError=1063 then
                Syntax else
                Show(false);
          finally
            service.Free;
          end;
        end else
          Syntax;
      cInstall: begin
        if (ParamCount>=3) and SameText(ParamStr(2),'/depend') then begin
          depend := ParamStr(3);
          for i := 4 to ParamCount do
            depend := depend+#0+ParamStr(i);
        end;
        Show(TServiceController.Install(ServiceName,ServiceDisplayName,
          Description,ServiceAutoStart,'',depend)<>ssNotInstalled);
      end;
      else begin
        ctrl := TServiceController.CreateOpenService('','',ServiceName);
        try
          case cmd of
          cStart:
            Show(ctrl.Start([]));
          cStop:
            Show(ctrl.Stop);
          cUninstall: begin
            ctrl.Stop;
            Show(ctrl.Delete);
          end;
          cState:
            writeln(ServiceName,' State=',ServiceStateText(ctrl.State));
          else Show(false);
          end;
        finally
          ctrl.Free;
        end;
      end;
      end;
    {$else}
      Syntax;
    {$endif MSWINDOWS}
    end;       
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
  TextColor(ccLightGray);
  ioresult;
end;
{$I+}


{ TDDDThreadDaemon }

function TDDDThreadDaemon.GetAdministrationHTTPServer: TSQLHttpServer;
begin
  result := fAdministrationHTTPServer as TSQLHttpServer;
end;

function TDDDThreadDaemon.InternalRetrieveState(
  var Status: variant): boolean;
begin
  Status := _ObjFast(['SystemMemory',TSynMonitorMemory.ToVariant]);
  result := true;
end;


{ TDDDRestDaemon }

function TDDDRestDaemon.GetAdministrationHTTPServer: TSQLHttpServer;
begin
  result := TSQLHttpServer(fAdministrationHTTPServer);
end;

procedure TDDDRestDaemon.InternalLogMonitoring;
var status: variant;
begin
  {$ifdef WITHLOG}
  if fLog<>nil then
    if (sllMonitoring in fLog.Level) and InternalRetrieveState(status) then
      fLog.SynLog.Log(sllMonitoring,'%',[status],Self);
  {$endif}
end;

function TDDDRestDaemon.InternalRetrieveState(
  var Status: variant): boolean;
begin
  if fRest<>nil then begin
    Status := _ObjFast(['Rest',fRest.FullStatsAsDocVariant,
      'SystemMemory',TSynMonitorMemory.ToVariant]);
    result := true;
  end else
    result := false;
end;

function TDDDRestDaemon.Settings: TDDDAdministratedDaemonHttpSettings;
begin
  result := TDDDAdministratedDaemonHttpSettings(fInternalSettings);
end;

{ TDDDRestHttpDaemon }

procedure TDDDRestHttpDaemon.InternalStart;
begin
  if Settings.Http.BindPort<>'' then
    fHttpServer := TSQLHttpServer.Create(fRest,Settings.Http);
end;

procedure TDDDRestHttpDaemon.InternalStop;
begin
  try
    FreeAndNil(fHttpServer);
  finally
    inherited InternalStop; // FreeAndNil(fRest)
  end;
end;


{ ----- Implements Thread Processing to access a TCP server }

{ TDDDSocketThreadMonitoring }

function TDDDSocketThreadMonitoring.GetSocket: variant;
begin
  if (fOwner=nil) or (fOwner.fSocket=nil) then
    SetVariantNull(result) else
    ObjectToVariant(ObjectFromInterface(fOwner.fSocket),result);
end;


{ TDDDSocketThread }

constructor TDDDSocketThread.Create(
  aSettings: TDDDSocketThreadSettings; aRest: TSQLRest;
  aMonitoring: TDDDSocketThreadMonitoring);
begin
  if aSettings=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(Settings=nil)',[self]);
  fSettings := aSettings;
  if aMonitoring=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(aMonitoring=nil)',[self]);
  fMonitoring := aMonitoring;
  fMonitoring.fOwner := self;
  if fSettings.Host='' then
    fSettings.Host := '127.0.0.1';
  fHost := fSettings.Host;
  if fSettings.Port=0 then
    raise EDDDInfraException.CreateUTF8('%.Create(Port=0)',[self]);
  fPort := UInt32ToUtf8(fSettings.Port);
  fExecuteSocketLoopPeriod := 300;
  if fSettings.SocketTimeout<fExecuteSocketLoopPeriod then
    fSettings.SocketTimeout := 2000;
  fPerformConnection := true;
  inherited Create(aRest,true); // aOwnRest=true
end;

destructor TDDDSocketThread.Destroy;
var timeOut: Int64;
begin
  {$ifdef WITHLOG}
  FLog.Enter(self);
  {$endif}
  Terminate;
  timeOut := GetTickCount64+10000;
  repeat // wait until properly disconnected from remote TCP server
    Sleep(10);
  until (FMonitoring.State=tpsDisconnected) or (GetTickCount64>timeOut);
  inherited Destroy;
  FreeAndNil(fMonitoring);
end;

procedure TDDDSocketThread.ExecuteConnect;
var tix: Int64;
begin
  {$ifdef WITHLOG}
  FLog.Enter(self);
  {$endif}
  if fSocket<>nil then
    raise EDDDInfraException.CreateUTF8('%.ExecuteConnect: fSocket<>nil',[self]);
  if FMonitoring.State<>tpsDisconnected then
    raise EDDDInfraException.CreateUTF8('%.ExecuteConnect: State=%',[self,ord(FMonitoring.State)]);
  fMonitoring.State := tpsConnecting;
  {$ifdef WITHLOG}
  FLog.Log(sllTrace,'ExecuteConnect: Connecting to %:%',[Host,Port],self);
  {$endif}
  try
    if Assigned(fSettings.OnIDDDSocketThreadCreate) then
      fSettings.OnIDDDSocketThreadCreate(self,fSocket) else
      fSocket := TDDDSynCrtSocket.Create(self,fHost,fPort,fSettings.SocketTimeout,32768);
    fSocket.Connect;
    FMonitoring.State := tpsConnected; // to be done ASAP to allow sending
    InternalExecuteConnected;
    {$ifdef WITHLOG}
    FLog.Log(sllTrace,'ExecuteConnect: Connected via Socket % - %',
      [fSocket.Identifier,FMonitoring],self);
    {$endif}
  except
    on E: Exception do begin
      {$ifdef WITHLOG}
      FLog.Log(sllTrace,'ExecuteConnect: Impossible to Connect to %:% (%) %',
        [Host,Port,E.ClassType,FMonitoring],self);
      {$endif}
      fSocket := nil;
      FMonitoring.State := tpsDisconnected;
    end;
  end;
  if (FMonitoring.State<>tpsConnected) and not Terminated then
    if fSettings.ConnectionAttemptsInterval>0 then begin // on error, retry
      tix := GetTickCount64+fSettings.ConnectionAttemptsInterval*1000;
      repeat
        sleep(50);
      until Terminated or (GetTickCount64>tix);
      {$ifdef WITHLOG}
      if Terminated then
        FLog.Log(sllTrace,'ExecuteConnect: thread terminated',self) else
        FLog.Log(sllTrace,'ExecuteConnect: wait finished -> retry connect',self);
      {$endif}
    end;
end;

procedure TDDDSocketThread.ExecuteDisconnect;
var info: RawUTF8;
begin
  {$ifdef WITHLOG}
  FLog.Enter(self);
  {$endif}
  try
    fSafe.Lock;
    try
      fShouldDisconnect := false;
      FMonitoring.State := tpsDisconnected;
      try
        if fSocket=nil then
          info := '[Unknown]' else
          info := fSocket.Identifier;
        InternalExecuteDisconnect;
      finally
        fSocket := nil;
      end;
      {$ifdef WITHLOG}
      FLog.Log(sllTrace,'Socket % disconnected',[info],self);
      {$endif}
      InternalLogMonitoring;
    finally
      fSafe.UnLock;
    end;
  except
    {$ifdef WITHLOG}
    on E: Exception do
      FLog.Log(sllTrace,'Socket disconnection error (%)',[E.ClassType],self);
    {$endif}
  end;
end;

procedure TDDDSocketThread.ExecuteDisconnectAfterError;
begin
  {$ifdef WITHLOG}
  if fSocket<>nil then
    FLog.Log(sllError,'%.ExecuteDisconnectAfterError: Sock=% LastError=%',
      [ClassType,fSocket.Identifier,fSocket.LastError],self);
  {$endif}
  ExecuteDisconnect;
  FSocketInputBuffer := '';
  if fSettings.AutoReconnectAfterSocketError then
    FPerformConnection := true;
end;

procedure TDDDSocketThread.ExecuteSocket;
var pending, len: integer;
begin
  pending := fSocket.DataInPending(fExecuteSocketLoopPeriod);
  if Terminated or (pending=0) then
    exit;
  if pending<0 then begin
    ExecuteDisconnectAfterError;
    exit;
  end;
  len := length(FSocketInputBuffer);
  SetLength(FSocketInputBuffer,len+pending);
  if fSocket.DataIn(@PByteArray(FSocketInputBuffer)[len],pending)<>pending then begin
    ExecuteDisconnectAfterError;
    exit;
  end;
  FMonitoring.AddSize(pending,0);
  InternalExecuteSocket;
end;

procedure TDDDSocketThread.InternalExecute;
begin
  fPreviousMonitorTix := GetTickCount64;
  try
    repeat
      if fMonitoring.State=tpsConnected then
        ExecuteSocket else
        if fPerformConnection then
          ExecuteConnect else
          sleep(200);
      if Terminated then
        break;
      try
        if Elapsed(fPreviousMonitorTix,fSettings.MonitoringLogInterval) then
          InternalLogMonitoring;
        InternalExecuteIdle;
      except
        {$ifdef WITHLOG}
        on E: Exception do
          FLog.Log(sllWarning,'Skipped % exception in %.InternalExecuteIdle',[E,ClassType],self);
        {$endif}
      end;
    until Terminated;
  finally
    ExecuteDisconnect;
  end;
end;

procedure TDDDSocketThread.InternalExecuteConnected;
begin
end;

procedure TDDDSocketThread.InternalExecuteDisconnect;
begin
end;

procedure TDDDSocketThread.InternalExecuteIdle;
begin
  fSafe.Lock;
  try
    if fShouldDisconnect then
      ExecuteDisconnectAfterError;
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDSocketThread.InternalLogMonitoring;
var cached,flushed: integer;
begin // CachedMemory method will also purge any outdated cached entries
  cached := fRest.CacheOrNil.CachedMemory(@flushed);
  {$ifdef WITHLOG}
  FLog.Log(sllMonitoring,'% CachedMemory=% Flushed=%',[FMonitoring,cached,flushed],Self);
  {$endif}
  fPreviousMonitorTix := GetTickCount64;
end;

function TDDDSocketThread.StatsAsJson: RawUTF8;
begin
  with TJSONSerializer.CreateOwnedStream do
  try
    WriteObject(FMonitoring);
    CancelLastChar('}');
    if fRest.InheritsFrom(TSQLRestServer) then begin
      AddShort(',"Rest":');
      AddNoJSONEscapeUTF8(TSQLRestServer(fRest).FullStatsAsJson);
    end;
    Add(',"Version":"%","DateTime":"%"}',
      [ExeVersion.Version.Detailed,NowUTCToString(True,'T')]);
    SetText(result);
  finally
    Free;
  end;
end;

function TDDDSocketThread.TrySend(const aFrame: RawByteString;
  ImmediateDisconnectAfterError: boolean): Boolean;
var tmpSock: IDDDSocket; // avoid GPF if fSocket=nil after fSafe.UnLock (unlikely)
begin
  fSafe.Lock;
  result := (aFrame<>'') and (fSocket<>nil) and
            (fMonitoring.State=tpsConnected) and not fShouldDisconnect;
  if result then
    tmpSock := fSocket;
  fSafe.UnLock;
  if not result then
    exit;
  result := tmpSock.DataOut(pointer(aFrame),length(aFrame));
  if result then
    FMonitoring.AddSize(0,length(aFrame)) else
    if ImmediateDisconnectAfterError then
      ExecuteDisconnectAfterError else begin
      fSafe.Lock;
      fShouldDisconnect := true; // notify for InternalExecuteIdle
      fSafe.UnLock;
    end;
end;


{ TDDDSynCrtSocket }

constructor TDDDSynCrtSocket.Create(aOwner: TThread; const aHost,aPort: SockString;
  aSocketTimeout,aInternalBufferSize: integer);
begin
  inherited Create;
  fOwner := aOwner;
  fHost := aHost;
  fPort := aPort;
  fSocket := TCrtSocket.Create(aSocketTimeout);
  if aInternalBufferSize<512 then
    aInternalBufferSize := 512;
  fInternalBufferSize := aInternalBufferSize;
  fOutput.Init;
end;

destructor TDDDSynCrtSocket.Destroy;
begin
  FreeAndNil(fSocket);
  fOutput.Done;
  inherited;
end;

procedure TDDDSynCrtSocket.Connect;
begin
  fSocket.OpenBind(fHost,fPort,False);
  fSocket.CreateSockIn(tlbsCRLF,fInternalBufferSize); // use SockIn safe buffer
end;

function TDDDSynCrtSocket.DataIn(Content: PAnsiChar; ContentLength: integer): integer;
begin
  fSafe.Lock;
  try
    result := fSocket.SockInRead(Content,ContentLength,false);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDSynCrtSocket.DataInPending(aTimeOut: integer): integer;
begin
  fSafe.Lock;
  try
    result := fSocket.SockInPending(aTimeOut);
  finally                            
    fSafe.UnLock;
  end;
end;

function TDDDSynCrtSocket.DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
begin
  fOutput.Lock;
  try
    result := fSocket.TrySndLow(Content,ContentLength);
  finally
    fOutput.UnLock;
  end;
end;

function TDDDSynCrtSocket.Identifier: RawUTF8;
begin
  result := Int32ToUtf8(fSocket.Sock);
end;

function TDDDSynCrtSocket.LastError: RawUTF8;
begin
  result := StringToUTF8(SocketErrorMessage(fSocket.LastLowSocketError));
end;


{ TDDDMockedSocket }

constructor TDDDMockedSocket.Create(aOwner: TThread);
begin
  inherited Create;
  fOwner := aOwner;
end;

procedure TDDDMockedSocket.Connect;
begin
  CheckLatency(mslConnect);
  fSafe.Lock;
  try
    CheckRaiseException(msaConnectRaiseException);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.DataIn(Content: PAnsiChar; ContentLength: integer): integer;
begin
  CheckLatency(mslDataIn);
  fSafe.Lock;
  try
    CheckRaiseException(msaDataInRaiseException);
    result := length(fInput);
    if ContentLength<result then
      result := ContentLength;
    if result<=0 then
      exit;
    MoveFast(pointer(fInput)^,Content^,result);
    Delete(fInput,1,result);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.DataInPending(aTimeOut: integer): integer;
var forcedTimeout: boolean;
    endTix: Int64;
begin
  forcedTimeout := false;
  endTix := GetTickCount64+aTimeOut;
  repeat
    fSafe.Lock;
    try
      CheckRaiseException(msaDataInRaiseException);
      if msaDataInPendingFails in fExceptionActions then begin
        fExceptionActions := [];
        result := -1; // cspSocketError
        exit;
      end;
      if not forcedTimeout then
        if msaDataInPendingTimeout in fExceptionActions then begin
          fExceptionActions := [];
          forcedTimeout := true;
        end;
      if forcedTimeout then
        result := 0 else
        result := length(fInput);
    finally
      fSafe.UnLock; // wait outside the instance lock
    end;
    if (result<>0) or ((fOwner<>nil) and TSQLRestThread(fOwner).Terminated) or
       (aTimeOut=0) then
      break;
    sleep(1); // emulate blocking process, just like a regular socket
    if (fOwner<>nil) and TSQLRestThread(fOwner).Terminated then
      break; 
  until GetTickCount64>endTix; // warning: 10-16 ms resolution under Windows
end;

function TDDDMockedSocket.DataOut(Content: PAnsiChar; ContentLength: integer): boolean;
var previous: integer;
begin
  CheckLatency(mslDataOut);
  fSafe.Lock;
  try
    CheckRaiseException(msaDataOutRaiseException);
    if msaDataOutReturnsFalse in fExceptionActions then begin
      fExceptionActions := [];
      result := false;
      exit;
    end;
    result := true;
    if ContentLength<=0 then
      exit;
    previous := length(fOutput);
    SetLength(fOutput,previous+ContentLength);
    MoveFast(Content^,PByteArray(fOutput)^[previous],ContentLength);
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.Identifier: RawUTF8;
begin
  result := PointerToHex(self);
end;

function TDDDMockedSocket.LastError: RawUTF8;
begin
  fSafe.Lock;
  try
    StringToUTF8(fExceptionMessage,result);
    fExceptionMessage := '';
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDMockedSocket.MockDataIn(const Content: RawByteString);
begin
  fSafe.Lock;
  try
    fInput := fInput+Content;
  finally
    fSafe.UnLock;
  end;
end;

function TDDDMockedSocket.MockDataOut: RawByteString;
begin
  fSafe.Lock;
  try
    result := fOutput;
    fOutput := '';
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDMockedSocket.MockException(NextActions: TDDDMockedSocketExceptions;
  const ExceptionMessage: string; ExceptionClass: ExceptClass);
begin
  fSafe.Lock;
  try
    fExceptionActions := NextActions;
    fExceptionMessage := ExceptionMessage;
    fExceptionClass := ExceptionClass;
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDMockedSocket.CheckRaiseException(Action: TDDDMockedSocketException);
begin
  if not (Action in fExceptionActions) then
    exit;
  fExceptionActions := [];
  if fExceptionMessage='' then
    fExceptionMessage := Format('Mocked Exception for %s',
      [GetEnumName(TypeInfo(TDDDMockedSocketExceptions),ord(Action))^]);
  if fExceptionClass=nil then
    fExceptionClass := EDDDMockedSocket;
  raise fExceptionClass.Create(fExceptionMessage);
end;

procedure TDDDMockedSocket.MockLatency(
  NextActions: TDDDMockedSocketLatencies; MilliSeconds: integer);
begin
  fSafe.Lock;
  try
    fLatencyActions := NextActions;
    fLatencyMS := MilliSeconds;
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDMockedSocket.CheckLatency(Action: TDDDMockedSocketLatency);
var waitMS: integer;
begin
  fSafe.Lock;
  try
    if Action in fLatencyActions then
      waitMS := fLatencyMS else
      waitMS := 0;
  finally
    fSafe.UnLock; // wait outside the instance lock
  end;
  while (waitMS>0) and not ((fOwner<>nil) and TSQLRestThread(fOwner).Terminated) do begin
    sleep(1); // do not use GetTickCount64 (poor resolution under Windows)
    dec(waitMS);
  end;
end;

function TDDDMockedSocket.GetPendingInBytes: integer;
begin
  fSafe.Lock;
  result := Length(fInput);
  fSafe.UnLock;
end;

function TDDDMockedSocket.GetPendingOutBytes: integer;
begin
  fSafe.Lock;
  result := Length(fOutput);
  fSafe.UnLock;
end;



{ ----- Implements ORM/SOA REST Client access }

{ TDDDRestClientSettings }

function TDDDRestClientSettings.NewRestClientInstance(
  aRootSettings: TDDDAppSettingsAbstract; aModel: TSQLModel;
  aOptions: TDDDNewRestInstanceOptions): TSQLRestClientURI;
var pass: RawUTF8;
begin
  if aModel=nil then
    if riCreateVoidModelIfNone in aOptions then begin
      aModel := TSQLModel.Create([],'');
      include(aOptions,riOwnModel);
    end else
       raise EDDDRestClient.CreateUTF8('%.NewRestClientInstance(aModel=nil)',[self]);
  if fClient.Root='' then // supplied TSQLModel.Root is the default root URI
    fClient.Root := aModel.Root else
    aModel.Root := fClient.Root;
  if fORM.Kind='' then
    fORM.Kind := 'TSQLHttpClientWebsockets'; // assume we need HTTP + callbacks
  if fORM.ServerName='' then
    fORM.ServerName := 'localhost';
  result := nil;
  try
    try
      result := TSQLRest.CreateTryFrom(aModel,ORM, // will call SetUser()
        riHandleAuthentication in aOptions) as TSQLRestClientURI;
      if result=nil then
        exit; // no match or wrong parameters
      pass := fClient.PasswordPlain;
      if pass<>'' then
        (result as TSQLHttpClientWebsockets).WebSocketsConnect(pass) else
        if not result.ServerTimeStampSynchronize then
          raise EDDDRestClient.CreateUTF8('%.Create: HTTP access failure on %/%',
            [self,ORM.ServerName,aModel.Root]);
      result.OnAuthentificationFailed := OnAuthentificationFailed;
    except
      FreeAndNil(result);
    end;
  finally
    if riOwnModel in aOptions then
      if result=nil then // avoid memory leak
        aModel.Free else
        aModel.Owner := result;
    if (result=nil) and (riRaiseExceptionIfNoRest in aOptions) then
      raise EDDDRestClient.CreateUTF8('Impossible to initialize % on %/%',
        [fORM.Kind,fORM.ServerName,fClient.Root]);
  end;
end;

function TDDDRestClientSettings.OnAuthentificationFailed(Retry: integer;
  var aUserName, aPassword: string; out aPasswordHashed: boolean): boolean;
begin
  if (Retry=1) and (fORM.User<>'') then begin
    aUserName := UTF8ToString(fORM.User);
    aPassword := UTF8ToString(fORM.PasswordPlain);
    aPasswordHashed := true;
    result := true;
  end else
    result := false;
end;

procedure TDDDRestClientSettings.SetDefaults(const Root,Port,WebSocketPassword,
  UserPassword,User: RawUTF8);
begin
  if fClient.Root='' then
    fClient.Root := Root;
  if fORM.Kind='' then
    if WebSocketPassword<>'' then
      fORM.Kind := 'TSQLHttpClientWebsockets' else
      fORM.Kind := TSQLHttpClient.ClassName;
  if (Port<>'') and (fORM.ServerName='') then begin
    fORM.ServerName := 'http://localhost:'+Port;
    if fClient.WebSocketsPassword='' then
      fClient.WebSocketsPassword := WebSocketPassword;
    if UserPassword<>'' then begin
      fORM.User := User;
      fORM.PasswordPlain := UserPassword;
    end;
  end;
end;

function AdministratedDaemonClient(Definition: TDDDRestClientSettings;
  Model: TSQLModel): TSQLHttpClientWebsockets;
begin
  result := Definition.NewRestClientInstance(nil,Model) as TSQLHttpClientWebsockets;
  try
    result.ServiceDefine(IAdministratedDaemon,sicShared);
  except
    result.Free;
    raise;
  end;
end;

function AdministratedDaemonServer(Settings: TDDDAdministratedDaemonSettings;
  DaemonClass: TDDDAdministratedDaemonClass): TDDDAdministratedDaemon;
begin
  if DaemonClass=nil then
    raise EDDDInfraException.Create('AdministratedDaemonServer(DaemonClass=nil)');
  if Settings=nil then
    raise EDDDInfraException.Create('AdministratedDaemonServer(Settings=nil)');
  with Settings.RemoteAdmin do
    result := DaemonClass.Create(AuthUserName,AuthHashedPassword,AuthRootURI,AuthNamedPipeName);
  result.InternalSettings := Settings;
  result.Log.SynLog.Log(sllTrace,'%.Create(%)',[DaemonClass,Settings],result);
  with Settings.RemoteAdmin do
    if AuthHttp.BindPort<>'' then
      result.AdministrationHTTPServer := TSQLHttpServer.Create(
        result.AdministrationServer,AuthHttp);
end;

initialization
  {$ifdef EnableMemoryLeakReporting}
  {$ifdef HASFASTMM4} // FastMM4 integrated in Delphi 2006 (and up)
  ReportMemoryLeaksOnShutdown := True;
  {$endif}
  {$endif}
end.
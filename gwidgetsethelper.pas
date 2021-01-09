unit gwidgetsethelper;

{$mode objfpc}{$H+}
//only for linux/unix

interface

uses
  BaseUnix, Unix, glib2,
  Classes, Process,
  // LazUtils
  FileUtil, UTF8Process,
  // LCL
  InterfaceBase, LCLIntf;

type
  PPWaitHandleEventHandler = ^PWaitHandleEventHandler;
  PWaitHandleEventHandler = ^TWaitHandleEventHandler;
  TWaitHandleEventHandler = record
    Handle: THandle;
    GIOChannel: pgiochannel;
    GSourceID: guint;
    UserData: PtrInt;
    OnEvent: TWaitHandleEvent;
    PrevHandler: PWaitHandleEventHandler;
    NextHandler: PWaitHandleEventHandler;
  end;

  PPChildSignalEventHandler = ^PChildSignalEventHandler;
  PChildSignalEventHandler = ^TChildSignalEventHandler;
  TChildSignalEventHandler = record
    PID: TPid;
    UserData: PtrInt;
    OnEvent: TChildExitEvent;
    PrevHandler: PChildSignalEventHandler;
    NextHandler: PChildSignalEventHandler;
  end;

  { TGWidgetSetHelper }

  TGWidgetSetHelper = class
  private
    FWaitHandles: PWaitHandleEventHandler;
    FChildSignalHandlers: PChildSignalEventHandler;
  public
    constructor Create;

    procedure HandlePipeEvent(AData: PtrInt; AFlags: dword);

    function AddProcessEventHandler(AHandle: THandle;
      AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
    function  AddPipeEventHandler(AHandle: THandle; AEventHandler: TPipeEvent;
      AData: PtrInt): PPipeEventHandler;
    function AddEventHandler(AHandle: THandle; AFlags: dword;
             AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;

    procedure RemovePipeEventHandler(var AHandler: PPipeEventHandler);
    procedure RemoveEventHandler(var AHandler: PEventHandler);
    procedure RemoveProcessEventHandler(var AHandler: PProcessEventHandler);

    procedure PrepareSynchronize({%H-}AObject: TObject);
    procedure ProcessChildSignal;
    procedure InitSynchronizeSupport;

    procedure ProcessMessages;
  end;

type

  { TAsyncProcess }

  TGAsyncProcess = class(TProcessUTF8)
  private
    FPipeHandler: PPipeEventHandler;
    FProcessHandler: PProcessEventHandler;
    FOnReadData: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
  protected
    function GetNumBytesAvailable: dword;
    procedure HandlePipeInput({%H-}AData: PtrInt; AReasons: TPipeReasons);
    procedure HandleProcessTermination({%H-}AData: PtrInt; {%H-}AReason: TChildExitReason; {%H-}AInfo: dword);
    procedure UnhookPipeHandle;
    procedure UnhookProcessHandle;
  public
    procedure Execute; override;
    destructor Destroy; override;
    property NumBytesAvailable: dword read GetNumBytesAvailable;
  published
    property OnReadData: TNotifyEvent read FOnReadData write FOnReadData;// You must read all the data in this event. Otherwise it is called again.
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

var GWidgetHelper : TGWidgetSetHelper;

implementation

  // TThread.Synchronize support
  var
    threadsync_pipein, threadsync_pipeout: cint;
    threadsync_giochannel: pgiochannel;
    childsig_pending: boolean;

  procedure ChildEventHandler({%H-}sig: longint; {%H-}siginfo: psiginfo;
    {%H-}sigcontext: psigcontext); cdecl;
  begin
    childsig_pending := true;
    WakeMainThread(nil);
  end;

  procedure InstallSignalHandler;
  var
    child_action: sigactionrec;
  begin
    child_action.sa_handler := @ChildEventHandler;
    fpsigemptyset(child_action.sa_mask);
    child_action.sa_flags := 0;
    fpsigaction(SIGCHLD, @child_action, nil);
  end;

function waithandle_iocallback({%H-}source: PGIOChannel; condition: TGIOCondition;
  data: gpointer): gboolean; cdecl;
var
  lEventHandler: PWaitHandleEventHandler absolute data;
begin
  //debugln('waithandle_iocallback lEventHandler=',HexStr(Cardinal(lEventHandler),8));
  lEventHandler^.OnEvent(lEventHandler^.UserData, condition);
  Result := true;
end;

function TGWidgetSetHelper.AddEventHandler(AHandle: THandle; AFlags: dword;
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
var
  giochannel: pgiochannel;
  lEventHandler: PWaitHandleEventHandler;
begin
  if AEventHandler = nil then exit;
  New(lEventHandler);
  giochannel := g_io_channel_unix_new(AHandle);
  lEventHandler^.Handle := AHandle;
  lEventHandler^.UserData := AData;
  lEventHandler^.GIOChannel := giochannel;
  lEventHandler^.OnEvent := AEventHandler;
  lEventHandler^.GSourceID := g_io_add_watch(giochannel,
    AFlags, @waithandle_iocallback, lEventHandler);
  //debugln('TGtk2WidgetSet.AddEventHandler lEventHandler=',HexStr(Cardinal(lEventHandler),8),' AHandle=',dbgs(lEventHandler^.Handle));
  lEventHandler^.PrevHandler := nil;
  lEventHandler^.NextHandler := FWaitHandles;
  if FWaitHandles <> nil then
    FWaitHandles^.PrevHandler := lEventHandler;
  FWaitHandles := lEventHandler;
  Result := lEventHandler;
end;

type
  PPipeEventInfo = ^TPipeEventInfo;
  TPipeEventInfo = record
    Handler: PEventHandler;
    UserData: PtrInt;
    OnEvent: TPipeEvent;
  end;

function TGWidgetSetHelper.AddPipeEventHandler(AHandle: THandle;
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
var
  lPipeEventInfo: PPipeEventInfo;
begin
  if AEventHandler = nil then exit;
  New(lPipeEventInfo);
  lPipeEventInfo^.UserData := AData;
  lPipeEventInfo^.OnEvent := AEventHandler;
  lPipeEventInfo^.Handler := AddEventHandler(AHandle, G_IO_IN or G_IO_HUP or G_IO_OUT,
    @HandlePipeEvent, {%H-}PtrUInt(lPipeEventInfo));
  Result := lPipeEventInfo;
end;

constructor TGWidgetSetHelper.Create;
begin
  IsMultiThread := true;
  InitSynchronizeSupport;
  InstallSignalHandler;
end;

procedure TGWidgetSetHelper.HandlePipeEvent(AData: PtrInt; AFlags: dword);
var
  lPipeEventInfo: PPipeEventInfo absolute AData;
  lReasons: TPipeReasons;
begin
  lReasons := [];
  if AFlags and G_IO_IN = G_IO_IN then
    Include(lReasons, prDataAvailable);
  if AFlags and G_IO_OUT = G_IO_OUT then
    Include(lReasons, prCanWrite);
  if AFlags and G_IO_HUP = G_IO_HUP then
    Include(lReasons, prBroken);

  lPipeEventInfo^.OnEvent(lPipeEventInfo^.UserData, lReasons);
end;

procedure TGWidgetSetHelper.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
var
  lPipeEventInfo: PPipeEventInfo absolute AHandler;
begin
  if AHandler = nil then exit;
  RemoveEventHandler(lPipeEventInfo^.Handler);
  Dispose(lPipeEventInfo);
  AHandler := nil;
end;

function TGWidgetSetHelper.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
var
  lHandler: PChildSignalEventHandler;
begin
  if AEventHandler = nil then exit(nil);
  New(lHandler);
  lHandler^.PID := TPid(AHandle);
  lHandler^.UserData := AData;
  lHandler^.OnEvent := AEventHandler;
  lHandler^.PrevHandler := nil;
  lHandler^.NextHandler := FChildSignalHandlers;
  if FChildSignalHandlers <> nil then
    FChildSignalHandlers^.PrevHandler := lHandler;
  FChildSignalHandlers := lHandler;
  Result := lHandler;
end;

procedure TGWidgetSetHelper.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
var
  lHandler: PChildSignalEventHandler absolute AHandler;
begin
  if AHandler = nil then exit;
  if lHandler^.PrevHandler = nil then
    FChildSignalHandlers := lHandler^.NextHandler
  else
    lHandler^.PrevHandler^.NextHandler := lHandler^.NextHandler;
  if lHandler^.NextHandler <> nil then
    lHandler^.NextHandler^.PrevHandler := lHandler^.PrevHandler;
  Dispose(lHandler);
  AHandler := nil;
end;

procedure TGWidgetSetHelper.RemoveEventHandler(var AHandler: PEventHandler);
var
  lEventHandler: PWaitHandleEventHandler absolute AHandler;
begin
  if AHandler = nil then exit;
  g_source_remove(lEventHandler^.GSourceID);
  { channel will be freed with ref count drops to 0 }
  g_io_channel_unref(lEventHandler^.GIOChannel);
  if lEventHandler^.PrevHandler = nil then
    FWaitHandles := lEventHandler^.NextHandler
  else
    lEventHandler^.PrevHandler^.NextHandler := lEventHandler^.NextHandler;
  if lEventHandler^.NextHandler <> nil then
    lEventHandler^.NextHandler^.PrevHandler := lEventHandler^.PrevHandler;
  //debugln('TGtk2WidgetSet.RemoveEventHandler lEventHandler=',HexStr(Cardinal(lEventHandler),8),' AHandle=',dbgs(lEventHandler^.Handle));
  Dispose(lEventHandler);
  AHandler := nil;
end;

procedure TGWidgetSetHelper.PrepareSynchronize({%H-}AObject: TObject);
{ This method is the WakeMainThread of the unit classes.
  It is called in TThread.Synchronize to wake up the main thread = LCL GUI thread.
  see: TGtk2WidgetSet.InitSynchronizeSupport
}
var
  thrash: char;
begin
  // ToDo: TGtk2WidgetSet.PrepareSynchronize what is AObject?

  // wake up GUI thread by sending a byte through the threadsync pipe
  thrash:='l';
  fpwrite(threadsync_pipeout, thrash, 1);
end;

procedure TGWidgetSetHelper.ProcessChildSignal;
var
  pid: tpid;
  reason: TChildExitReason;
  status: integer;
  info: dword;
  handler: PChildSignalEventHandler;
begin
  repeat
    status:=0;
    pid := fpwaitpid(-1, status, WNOHANG);
    if pid <= 0 then break;
    if wifexited(status) then
    begin
      reason := cerExit;
      info := wexitstatus(status);
    end else
    if wifsignaled(status) then
    begin
      reason := cerSignal;
      info := wtermsig(status);
    end else
      continue;

    handler := FChildSignalHandlers;
    while handler <> nil do
    begin
      if handler^.pid = pid then
      begin
        handler^.OnEvent(handler^.UserData, reason, info);
        break;
      end;
      handler := handler^.NextHandler;
    end;
  until false;
end;

function threadsync_iocallback({%H-}source: PGIOChannel; {%H-}condition: TGIOCondition;
  data: gpointer): gboolean; cdecl;
var
  thrashspace: array[1..1024] of byte;
begin
  // read the sent bytes
  fpread(threadsync_pipein, {%H-}thrashspace[1], 1);

  Result := true;
  // one of children signaled ?
  if childsig_pending then
  begin
    childsig_pending := false;
    TGWidgetSetHelper(data).ProcessChildSignal;
  end;
  // execute the to-be synchronized method
  if IsMultiThread then
    CheckSynchronize;
end;

procedure TGWidgetSetHelper.InitSynchronizeSupport;
{ When a thread calls its Synchronize, it calls
  WakeMainThread (defined in the unit classes).
  Set
}
begin
  { TThread.Synchronize ``glue'' }
  WakeMainThread := @PrepareSynchronize;
  assignpipe(threadsync_pipein, threadsync_pipeout);
  threadsync_giochannel := g_io_channel_unix_new(threadsync_pipein);
  g_io_add_watch(threadsync_giochannel, G_IO_IN, @threadsync_iocallback, Self);
end;

procedure TGWidgetSetHelper.ProcessMessages;
var i : integer;
begin
  i:=100;

  while g_main_context_pending(g_main_context_default) and (i>0) do
  begin
      if not g_main_context_iteration(g_main_context_default, False) then
        break;
      dec(i);
  end;
end;

function TGAsyncProcess.GetNumBytesAvailable: dword;
begin
  if not (poUsePipes in Options) then
    Result := 0
  else
    Result := Output.NumBytesAvailable;
end;

destructor TGAsyncProcess.Destroy;
begin
  UnhookProcessHandle;
  UnhookPipeHandle;
  inherited;
end;

procedure TGAsyncProcess.UnhookProcessHandle;
begin
  if FProcessHandler <> nil then
    GWidgetHelper.RemoveProcessEventHandler(FProcessHandler);
end;

procedure TGAsyncProcess.UnhookPipeHandle;
begin
  if FPipeHandler <> nil then
    GWidgetHelper.RemovePipeEventHandler(FPipeHandler);
end;

procedure TGAsyncProcess.HandlePipeInput({%H-}AData: PtrInt; AReasons: TPipeReasons);
begin
  if prBroken in AReasons then
    UnhookPipeHandle;
  if prDataAvailable in AReasons then
    if FOnReadData <> nil then
      FOnReadData(Self);
end;

procedure TGAsyncProcess.HandleProcessTermination({%H-}AData: PtrInt; {%H-}AReason: TChildExitReason; {%H-}AInfo: dword);
begin
  UnhookProcessHandle;
  UnhookPipeHandle;
  if FOnTerminate <> nil then
    FOnTerminate(Self);
end;

procedure TGAsyncProcess.Execute;
begin
  inherited Execute;

  if poUsePipes in Options then
    FPipeHandler := GWidgetHelper.AddPipeEventHandler(Output.Handle, @HandlePipeInput, 0);
  FProcessHandler := GWidgetHelper.AddProcessEventHandler(ProcessHandle, @HandleProcessTermination, 0);
end;

end.


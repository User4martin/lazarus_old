{ $Id$}
{
 *****************************************************************************
 *                               lclclasses.pp                               *
 *                               -------------                               *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Defines the base class for all LCL TComponents including controls.
}
unit LCLClasses;

{$mode objfpc}{$H+}

{.$DEFINE RDTSCBenchmarking}

interface

uses
  Classes, WSLCLClasses, WSReferences, LCLType, LCLProc;

type

  // SysUtils.LongRec has unsigned Word for Lo and Hi,
  //  we need a similar record with signed SmallInt
  LazLongRec = packed record
{$ifdef FPC_LITTLE_ENDIAN}
    Lo,Hi : SmallInt;
{$else FPC_LITTLE_ENDIAN}
    Hi,Lo : SmallInt;
{$endif FPC_LITTLE_ENDIAN}
  end;

  { TLCLComponent }

  TLCLComponent = class(TComponent)
  private
    FWidgetSetClass: TWSLCLComponentClass;
    FLCLRefCount: integer;
  protected
    class procedure WSRegisterClass; virtual;
    class function GetWSComponentClass(ASelf: TLCLComponent): TWSLCLComponentClass; virtual;
  public
    {$IFDEF DebugLCLComponents}
    constructor Create(TheOwner: TComponent); override;
    {$ENDIF}
    destructor Destroy; override;
    class function NewInstance: TObject; override;
    procedure RemoveAllHandlersOfObject(AnObject: TObject); virtual;
    procedure IncLCLRefCount;
    procedure DecLCLRefCount;
    property LCLRefCount: integer read FLCLRefCount;
    property WidgetSetClass: TWSLCLComponentClass read FWidgetSetClass;
  end;

  { TLCLReferenceComponent }

  // A base class for all components having a handle
  TLCLReferenceComponent = class(TLCLComponent)
  private
    FReferencePtr: PWSReference;
    FCreating: Boolean; // Set if we are creating the handle
    function  GetHandle: THandle;
    function  GetReferenceAllocated: Boolean;
  protected
    procedure CreateParams(var AParams: TCreateParams); virtual;
    procedure DestroyReference;
    function  GetReferenceHandle: THandle; virtual; abstract;
    procedure ReferenceCreated; virtual;    // gets called after the Handle is created
    procedure ReferenceDestroying; virtual; // gets called before the Handle is destroyed
    procedure ReferenceNeeded;
    function  WSCreateReference(AParams: TCreateParams): PWSReference; virtual;
    procedure WSDestroyReference; virtual;
  protected
  public
    destructor Destroy; override;
    property HandleAllocated: Boolean read GetReferenceAllocated;
    property ReferenceAllocated: Boolean read GetReferenceAllocated;
  end;

implementation

{$IFDEF RDTSCBenchmarking}
uses
  SysUtils,
  uRDTSC in 'D:\fpc-laz\Lazarus\bk_test\LazForum\urdtsc.pas';
const
  BenchmarkTotalTicks : QWord = 0;

procedure OnWSLCLFinalize;
begin
  WriteLn;
  Writeln(FormatFloat('CPU speed #0.000', 1 / (1000000 * CpuClockPeriod)) : 6,' GHz ');
  Writeln('Total time in NewInstance=',RdtscElapsed(0, BenchmarkTotalTicks));
  Write('Press enter to quit > '); ReadLn;
end;
{$ENDIF}

function WSRegisterLCLComponent: boolean;
begin
  RegisterWSComponent(TLCLComponent, TWSLCLComponent);
  Result := True;
end;

const
  cLCLComponentRegistered : boolean = false;

class procedure TLCLComponent.WSRegisterClass;
begin
  if cLCLComponentRegistered then
    Exit;
  WSDoInitialization(@TLCLComponent.WSRegisterClass);
  {$IFDEF RDTSCBenchmarking}
  CheckCpuSpeed;
  WSLCLClasses.OnFinalize := @OnWSLCLFinalize;
  {$ENDIF}
  WSRegisterLCLComponent;
  cLCLComponentRegistered := True;
end;

{ This method allows descendents to override the FWidgetSetClass, handles
  registration of the component in WSLVLClasses list of components. It is only
  called if there wasn't a direct or parent hit at the beginining of NewInstance. }
class function TLCLComponent.GetWSComponentClass(ASelf: TLCLComponent): TWSLCLComponentClass;
begin
  Result := FindWSRegistered(Self);
end;

{$IFDEF DebugLCLComponents}
constructor TLCLComponent.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //DebugLn('TLCLComponent.Create ',DbgSName(Self));
  DebugLCLComponents.MarkCreated(Self,DbgSName(Self));
end;
{$ENDIF}

destructor TLCLComponent.Destroy;
begin
  {$IFNDEF DisableChecks}
  if FLCLRefCount>0 then begin
    DebugLn(['WARNING: ' + ClassName + '.Destroy with LCLRefCount>0. Hint: Maybe the component is processing an event?']);
    {$IFDEF DebugTLCLComponentDestroy}
    DumpStack;
    {$ENDIF}
  end;
  {$ENDIF}
  {$IFDEF DebugLCLComponents}
  //DebugLn('TLCLComponent.Destroy ',DbgSName(Self));
  DebugLCLComponents.MarkDestroyed(Self);
  {$ENDIF}
  inherited Destroy;
end;

class function TLCLComponent.NewInstance: TObject;
{$IFDEF RDTSCBenchmarking}
var
  RDTSCStart, RDTSCStop : QWord;
{$ENDIF}
begin
  Result := inherited NewInstance;
  if not cLCLComponentRegistered then
    TLCLComponent.WSRegisterClass; { Initialize WSLCLClasses }
  {$IFDEF RDTSCBenchmarking} RDTSCStart := CPUTickStamp; {$ENDIF}
  TLCLComponent(Result).FWidgetSetClass := {WSLCLClasses.}GetWidgetSet(Self);
  {$IFDEF RDTSCBenchmarking}
  RDTSCStop := CPUTickStamp;
  BenchmarkTotalTicks := (BenchmarkTotalTicks + RDTSCStop) - RDTSCStart
                          - CPUTickStampCost;
  {$ENDIF}
end;

procedure TLCLComponent.RemoveAllHandlersOfObject(AnObject: TObject);
begin
end;

procedure TLCLComponent.IncLCLRefCount;
begin
  inc(FLCLRefCount);
end;

procedure TLCLComponent.DecLCLRefCount;
begin
  dec(FLCLRefCount);
end;

{ TLCLReferenceComponent }

procedure TLCLReferenceComponent.CreateParams(var AParams: TCreateParams);
begin
end;

destructor TLCLReferenceComponent.Destroy;
begin
  DestroyReference;
  inherited Destroy;
end;

procedure TLCLReferenceComponent.DestroyReference;
begin
  if ReferenceAllocated then
  begin
    ReferenceDestroying;
    WSDestroyReference;
    FReferencePtr^._Clear;
    FReferencePtr := nil;
  end;
end;

function TLCLReferenceComponent.GetHandle: THandle;
begin
  ReferenceNeeded;
  Result := GetReferenceHandle;
end;

function TLCLReferenceComponent.GetReferenceAllocated: Boolean;
begin
  Result := (FReferencePtr <> nil) and FReferencePtr^.Allocated;
end;

procedure TLCLReferenceComponent.ReferenceCreated;
begin
end;

procedure TLCLReferenceComponent.ReferenceDestroying;
begin
end;

procedure TLCLReferenceComponent.ReferenceNeeded;
var
  Params: TCreateParams;
begin
  if ReferenceAllocated then Exit;

  if FCreating
  then begin
    // raise some error ?
    {$IFNDEF DisableChecks}
    DebugLn('TLCLReferenceComponent: Circular reference creation');
    {$ENDIF}
    Exit;
  end;

  CreateParams(Params);
  FCreating := True;
  try
    FReferencePtr := WSCreateReference(Params);
    if not ReferenceAllocated
    then begin
      // raise some error ?
      {$IFNDEF DisableChecks}
      DebugLn('TLCLHandleComponent: Reference creation failed');
      {$ENDIF}
      Exit;
    end;
  finally
    FCreating := False;
  end;
  ReferenceCreated;
end;

function TLCLReferenceComponent.WSCreateReference(AParams: TCreateParams): PWSReference;
begin
  // this function should be overriden in derrived class
  Result := nil;
end;

procedure TLCLReferenceComponent.WSDestroyReference;
begin
  TWSLCLReferenceComponentClass(WidgetSetClass).DestroyReference(Self);
end;

end.


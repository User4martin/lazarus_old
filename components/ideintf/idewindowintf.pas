{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Provides general classes and methods to access and handle IDE dialogs and
    windows.
}
unit IDEWindowIntf;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, LazConfigStorage, LazUTF8, Forms, Controls,
  LCLIntf;

  //----------------------------------------------------------------------------
  // layout settings of modal forms (dialogs) in the IDE
type

  TIDEDialogLayoutList = class;

  { TIDEDialogLayout - for modal forms
    For non modal forms see TIDEWindowCreator below }

  TIDEDialogLayout = class
  private
    FList: TIDEDialogLayoutList;
    FModified: boolean;
    FName: string;
    FWidth: integer;
    FHeight: integer;
    procedure SetList(const AValue: TIDEDialogLayoutList);
    procedure SetModified(const AValue: boolean);
    procedure SetWidth(const AValue: integer);
    procedure SetHeight(const AValue: integer);
  public
    constructor Create(const TheName: string; TheList: TIDEDialogLayoutList);
    procedure Assign(Source: TIDEDialogLayout);
    function SizeValid: boolean;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property Name: string read FName;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
    property List: TIDEDialogLayoutList read FList write SetList;
    property Modified: boolean read FModified write SetModified;
  end;
  TIDEDialogLayoutClass = class of TIDEDialogLayout;

  { TIDEDialogLayoutList - for modal forms }

  TIDEDialogLayoutList = class
  private
    FItemClass: TIDEDialogLayoutClass;
    FItems: TList;
    FModified: boolean;
    function GetItems(Index: integer): TIDEDialogLayout;
  protected
    procedure SetModified(const AValue: boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TIDEDialogLayoutList);
    procedure ApplyLayout(ADialog: TControl;
                          DefaultWidth, DefaultHeight: integer;
                          UseAsMin: boolean = true);
    procedure ApplyLayout(ADialog: TControl);
    procedure SaveLayout(ADialog: TControl);
    procedure Clear;
    function Count: integer;
    function Find(const DialogName: string;
                  CreateIfNotExists: boolean): TIDEDialogLayout;
    function Find(ADialog: TObject;
                  CreateIfNotExists: boolean): TIDEDialogLayout;
    function IndexOf(const DialogName: string): integer;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
    property Items[Index: integer]: TIDEDialogLayout read GetItems;
    property Modified: boolean read FModified write SetModified;
    property ItemClass: TIDEDialogLayoutClass read FItemClass write FItemClass;
  end;
  
  { TIDEDialogLayoutStorage }

  TIDEDialogLayoutStorage = class(TComponent)
  protected
    procedure OnCreateForm(Sender: TObject);
    procedure OnCloseForm(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  IDEDialogLayoutList: TIDEDialogLayoutList = nil;// set by the IDE

type
  { TSimpleWindowLayout stores information about the position, min/maximized state
    and similar things for an IDE window or dialog, like the source editor,
    the object inspector, the main bar or the message view.
  }
  TIDEWindowPlacement = (
    iwpUseWindowManagerSetting, // leave window position, where window manager
                                //   creates the window
    iwpDefault,                 // set window to the default position
    iwpRestoreWindowGeometry,   // save window geometry at end and restore it
                                //   at start
    iwpCustomPosition,          // set window to custom position
    iwpRestoreWindowSize        // save window size at end and restore it
                                //   at start
    );
  TIDEWindowPlacements = set of TIDEWindowPlacement;
  TIDEWindowState = (iwsNormal, iwsMaximized, iwsMinimized, iwsHidden);
  TIDEWindowStates = set of TIDEWindowState;

  TSimpleWindowLayoutDividerPosPlacement = (
    iwpdDefault,      // set column/row/splitter to the default size
    iwpdCustomSize,   // set column/row/splitter to the custom size
    iwpdRestore,      // save column/row/splitter size on exit, and restore
    iwpdUseWindowSetting
  );

  TSimpleWindowLayoutDividerPosSizeGetter =
    function(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;

  TSimpleWindowLayoutDividerPosSizeSetter =
    procedure(AForm: TCustomForm; AColId: Integer; ASize: Integer);

  { TSimpleWindowLayoutDividerPos }

  TSimpleWindowLayoutDividerPos = class
  private
    FDefaultSize: integer;
    FDisplayName: PString;
    FId: Integer;
    FIdString: String;
    FPlacement: TSimpleWindowLayoutDividerPosPlacement;
    FSize: integer;
    function GetDisplayName: String;
  protected
    procedure SetDisplayName(ADisplayName: PString);
    procedure SetId(AnId: Integer);
  public
    constructor Create(AnIdString: String);
    constructor Create(AnIdString: String; AnId: Integer; ADisplayName: PString);
    procedure Assign(ADividerPos: TSimpleWindowLayoutDividerPos); reintroduce;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    function  SaveToConfig(Config: TConfigStorage; const Path: string) : Boolean;
    procedure Clear;
    property IdString: String read FIdString;
    property Id: Integer read FId;
    property DisplayName: String read GetDisplayName;
    property Placement: TSimpleWindowLayoutDividerPosPlacement read FPlacement write FPlacement;
    property Size: integer read FSize write FSize;
    property DefaultSize: integer read FDefaultSize write FDefaultSize;
  end;

  { TSimpleWindowLayoutDividerPosList }

  TSimpleWindowLayoutDividerPosList = class
  private
    FList: TList;
    function GetItems(Index: Integer): TSimpleWindowLayoutDividerPos;
    function GetNamedItems(Index: Integer): TSimpleWindowLayoutDividerPos;
  protected
    procedure Merge(AnItem: TSimpleWindowLayoutDividerPos);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ADividerPosList: TSimpleWindowLayoutDividerPosList); reintroduce;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
    procedure Clear;
    procedure ClearItems;
    function Add(AnIdString: String; AnId: Integer; ADisplayName: PString): TSimpleWindowLayoutDividerPos;
    function Add(AnIdString: String): TSimpleWindowLayoutDividerPos;
    function Count: Integer;
    function NamedCount: Integer;
    function Find(AnId: Integer): TSimpleWindowLayoutDividerPos;
    function Find(AnIdString: String): TSimpleWindowLayoutDividerPos;
    function IndexOf(AnId: Integer): Integer;
    function IndexOf(AnIdString: String): Integer;
    function IndexOf(AnItem: TSimpleWindowLayoutDividerPos): Integer;
    property Items[Index: Integer]: TSimpleWindowLayoutDividerPos read GetItems; default;
    property NamedItems[Index: Integer]: TSimpleWindowLayoutDividerPos read GetNamedItems;
  end;

  { TSimpleWindowLayout }

  TSimpleWindowLayout = class(TComponent)
  private
    FApplied: boolean;
    FFormCaption: string;
    FVisible: boolean;
    FWindowPlacement: TIDEWindowPlacement;
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FDefaultLeft: integer;
    FDefaultTop: integer;
    FDefaultWidth: integer;
    FDefaultHeight: integer;
    FWindowState: TIDEWindowState;
    FForm: TCustomForm;
    FFormID: string;
    FDefaultWindowPlacement: TIDEWindowPlacement;
    FDividers: TSimpleWindowLayoutDividerPosList;
    function GetFormCaption: string;
    function GetFormID: string;
    procedure SetForm(const AValue: TCustomForm);
    procedure OnFormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AFormID: string); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure GetCurrentPosition;
    function  Apply(const aForce: Boolean = False): Boolean;
    procedure ApplyDivider(AForce: Boolean = False);
    procedure Assign(Layout: TSimpleWindowLayout); reintroduce;
    procedure ReadCurrentDividers(AForce: Boolean = False);
    procedure ReadCurrentCoordinates;
    procedure ReadCurrentState;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
    function CustomCoordinatesAreValid: boolean;
    procedure CloseForm;
    function ValidateAndSetCoordinates(const aForce: Boolean = False): Boolean;
    procedure SetDefaultPosition(const AForm: TCustomForm);
  public
    property FormID: string read GetFormID write FFormID;
    function FormBaseID(out SubIndex: Integer): String; // split FormID into name+number
    property FormCaption: string read GetFormCaption;
    property WindowPlacement: TIDEWindowPlacement read fWindowPlacement write FWindowPlacement;
    property DefaultWindowPlacement: TIDEWindowPlacement
      read FDefaultWindowPlacement write FDefaultWindowPlacement;
    property Left: integer read FLeft write FLeft;
    property Top: integer read FTop write FTop;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property DefaultLeft: integer read FDefaultLeft write FDefaultLeft;
    property DefaultTop: integer read FDefaultTop write FDefaultTop;
    property DefaultWidth: integer read FDefaultWidth write FDefaultWidth;
    property DefaultHeight: integer read FDefaultHeight write FDefaultHeight;
    property WindowState: TIDEWindowState read FWindowState write FWindowState;
    property Form: TCustomForm read FForm write SetForm;
    property Visible: boolean read FVisible write FVisible;
    property Applied: boolean read FApplied write FApplied;
    property Dividers: TSimpleWindowLayoutDividerPosList read FDividers;
  end;

  { TSimpleWindowLayoutList }
  TLayoutMoveToVisbleMode = (
    vmAlwaysMoveToVisible,
    vmNeverMoveToVisible,
    vmOnlyMoveOffScreenToVisible   // Only make visible, if fully offscreen (no part of the windov is on screen)
  );

  TSimpleWindowLayoutList = class
  private
    fItems: TFPList;
    fRegisterEventHandlers: Boolean;
    function GetItems(Index: Integer): TSimpleWindowLayout;
    procedure OnScreenRemoveForm(Sender: TObject; AForm: TCustomForm);
  public
    constructor Create(ARegisterEventHandlers: Boolean);
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure ApplyAndShow(Sender: TObject; AForm: TCustomForm;
                           BringToFront: boolean;
                           AMoveToVisbleMode: TLayoutMoveToVisbleMode = vmAlwaysMoveToVisible);
    procedure StoreWindowPositions;
    procedure SetDefaultPosition(const AForm: TCustomForm);
    procedure Assign(SrcList: TSimpleWindowLayoutList; AssignOnlyNewlyCreated,
      DestroyNotAssigned, AssignOrder: Boolean);
    function Add(ALayout: TSimpleWindowLayout): integer;
    function CreateWindowLayout(const TheFormID: string): TSimpleWindowLayout;
    function CreateWindowLayout(const TheForm: TCustomForm): TSimpleWindowLayout;
    function IndexOf(const FormID: string): integer;
    function IndexOf(const AForm: TCustomForm): integer;
    function ItemByForm(AForm: TCustomForm): TSimpleWindowLayout;
    function ItemByFormID(const FormID: string): TSimpleWindowLayout;
    function ItemByFormCaption(const aFormCaption: string): TSimpleWindowLayout;
    procedure CloseForm(AForm: TCustomForm);
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
  public
    function Count: integer;
    property Items[Index: Integer]: TSimpleWindowLayout read GetItems; default;
  end;

const
  IDEWindowPlacementNames: array[TIDEWindowPlacement] of string = (
      'UseWindowManagerSetting',
      'Default',
      'RestoreWindowGeometry',
      'CustomPosition',
      'RestoreWindowSize'
    );
  IDEWindowStateNames: array[TIDEWindowState] of string = (
      'Normal', 'Maximized', 'Minimized', 'Hidden'
    );

function StrToIDEWindowPlacement(const s: string): TIDEWindowPlacement;
function StrToIDEWindowState(const s: string): TIDEWindowState;

type
  TIWCState = (
    iwcsHidden,
    iwcsIconified,
    iwcsNormal,
    iwcsDocked
    );
  TIWGetFormState = (
    iwgfDisabled, // create if not exist with disabled autosizing
    iwgfEnabled, // create if not exists
    iwgfShow, // create and show
    iwgfShowOnTop // create, show and bring to front
    );
  TIWGetFormStates = set of TIWGetFormState;

  TCreateIDEWindowMethod = procedure(Sender: TObject; aFormName: string;
                var AForm: TCustomForm; DoDisableAutoSizing: boolean) of object;
  TCreateIDEWindowProc = procedure(Sender: TObject; aFormName: string;
                var AForm: TCustomForm; DoDisableAutoSizing: boolean);
  TGetDefaultIDEWindowLayoutEvent = procedure(Sender: TObject; aFormName: string;
   out aBounds: TRect; out DockSibling: string; out DockAlign: TAlign) of object;
  TShowIDEWindowEvent = procedure(Sender: TObject; AForm: TCustomForm;
                                  BringToFront: boolean) of object;

  { TIDEWindowCreator
    Every dockable window of the IDE needs a TIDEWindowCreator. }

  TIDEWindowCreator = class
  private
    FCreateFormMethod: TCreateIDEWindowMethod;
    FCreateFormProc: TCreateIDEWindowProc;
    FDockAlign: TAlign;
    FDockSibling: string;
    FFormName: string;
    FBottom: string;
    FLeft: string;
    FMulti: boolean;
    FOnGetDividerSize: TSimpleWindowLayoutDividerPosSizeGetter;
    FOnGetLayout: TGetDefaultIDEWindowLayoutEvent;
    FOnSetDividerSize: TSimpleWindowLayoutDividerPosSizeSetter;
    FState: TIWCState;
    FTop: string;
    FRight: string;
    FDividerTemplate: TSimpleWindowLayoutDividerPosList;
    function GetDividerTemplate: TSimpleWindowLayoutDividerPosList;
    procedure SetBottom(const AValue: string);
    procedure SetLeft(const AValue: string);
    procedure SetTop(const AValue: string);
    procedure SetRight(const AValue: string);
    procedure InitSimpleLayout(ALayout: TSimpleWindowLayout);
  public
    constructor Create(aFormName: string); overload;
    constructor Create(aFormName: string;
                       CreateFormProc: TCreateIDEWindowProc;
                       CreateFormMethod: TCreateIDEWindowMethod;
                       aLeft, aTop, aRight, aBottom: string;
                       aDockSibling : string = '';
                       aDockAlign: TAlign = alNone;
                       aMulti: boolean = false;
               GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent = nil); overload;
    destructor Destroy; override;
    property FormName: string read FFormName; // prefix for all forms
    property Multi: boolean read FMulti; // there can be more than one of this form, e.g. the source editors and the package editors
    property OnCreateFormMethod: TCreateIDEWindowMethod read FCreateFormMethod write FCreateFormMethod;
    property OnCreateFormProc: TCreateIDEWindowProc read FCreateFormProc write FCreateFormProc;
    function NameFits(const AName: string): boolean;

    // hints for the layout system / dock master. It may ignore them completely.
    property State: TIWCState read FState write FState;
    property Left: string read FLeft write SetLeft; // '12' for 12 pixel, '10%' for 10 percent of screen.width
    property Top: string read FTop write SetTop; // '12' for 12 pixel, '10%' for 10 percent of screen.height
    property Right: string read FRight write SetRight; // '12' for 12 pixel, '+12' for a Width of 12 pixel, '-12' for Screen.Width-12, '10%' for 10 percent of screen.width
    property Bottom: string read FBottom write SetBottom; // '12' for 12 pixel, '+12' for a Height of 12 pixel, '10%' for 10 percent of screen.height
    property DockSibling: string read FDockSibling write FDockSibling; // another form name
    property DockAlign: TAlign read FDockAlign write FDockAlign;
    property OnGetLayout: TGetDefaultIDEWindowLayoutEvent read FOnGetLayout
                                                          write FOnGetLayout;
    procedure CheckBoundValue(s: string);
    procedure GetDefaultBounds(AForm: TCustomForm; out DefBounds: TRect);

    function CreateSimpleLayout: TSimpleWindowLayout;
    // TODO: Need a WindowCreator factory, by class of TForm
    // then this data can be stored per window class
    property  DividerTemplate: TSimpleWindowLayoutDividerPosList read GetDividerTemplate;
    property  OnGetDividerSize: TSimpleWindowLayoutDividerPosSizeGetter read FOnGetDividerSize write FOnGetDividerSize;
    property  OnSetDividerSize: TSimpleWindowLayoutDividerPosSizeSetter read FOnSetDividerSize write FOnSetDividerSize;
  end;

  { TIDEWindowCreatorList }

  TIDEWindowCreatorList = class
  private
    fItems: TFPList; // list of TIDEWindowCreator
    FScreenMaxSizeForDefaults: TPoint;
    FSimpleLayoutStorage: TSimpleWindowLayoutList;
    FOnLayoutChanged: TMethodList;
    procedure LayoutChanged;
    function GetItems(Index: integer): TIDEWindowCreator;
    procedure ErrorIfFormExists(FormName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    property Items[Index: integer]: TIDEWindowCreator read GetItems; default;
    function Add(aLayout: TIDEWindowCreator): integer; overload;
    function Add(aFormName: string): TIDEWindowCreator; overload;
    function Add(aFormName: string;
                 CreateFormProc: TCreateIDEWindowProc;
                 CreateFormMethod: TCreateIDEWindowMethod;
                 // you can pass some hints to the layout system how to place the form by default
                 // Note: the IDE dockmaster may completely ignore these values
                 aLeft, aTop, aRight, aBottom: string;
                 aDockSibling : string = '';
                 aDockAlign: TAlign = alNone;
                 aMulti: boolean = false; // if true, then there can be more than one instance, the aFormName is the shared prefix
                 GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent = nil
                 ): TIDEWindowCreator; overload;
    procedure Delete(Index: integer);
    function IndexOfName(FormName: string): integer;
    function FindWithName(FormName: string): TIDEWindowCreator;
    function GetForm(aFormName: string; AutoCreate: boolean;
                               DisableAutoSizing: boolean = false): TCustomForm;
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean;
                       AMoveToVisbleMode: TLayoutMoveToVisbleMode = vmAlwaysMoveToVisible); overload;
    function ShowForm(AFormName: string; BringToFront: boolean): TCustomForm; overload;
    procedure CreateForm(var AForm; AFormClass: TCustomFormClass;
                         DoDisableAutoSizing: boolean; TheOwner: TComponent); // utility function to create a form with delayed autosizing

    property SimpleLayoutStorage: TSimpleWindowLayoutList read FSimpleLayoutStorage;
    procedure RestoreSimpleLayout;
    procedure AddLayoutChangedHandler(const aEvent: TNotifyEvent);
    procedure RemoveLayoutChangedHandler(const aEvent: TNotifyEvent);

    property ScreenMaxSizeForDefaults: TPoint read FScreenMaxSizeForDefaults
                                              write FScreenMaxSizeForDefaults; // on big screens: do not span the whole screen
    function GetScreenrectForDefaults: TRect;
  end;

var
  IDEWindowCreators: TIDEWindowCreatorList = nil; // set by the IDE

type

  TDockSides = set of TAlign;

  { TIDEDockMaster }

  TIDEDockMaster = class
  protected
    FHideSimpleLayoutOptions: boolean;
  public
    procedure MakeIDEWindowDockable(AControl: TWinControl); virtual; abstract; // make AControl dockable, it can be docked and other dockable windows can be docked to it, this does not make it visible
    procedure MakeIDEWindowDockSite(AForm: TCustomForm; ASides: TDockSides = [alBottom]); virtual; abstract; // make AForm a dock site, AForm can not be docked, its Parent must be kept nil, this does not make it visible
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); virtual; abstract; // make a form visible, set BringToFront=true if form should be shown on active screen and on front of other windows, normally this focus the form
    function AddableInWindowMenu({%H-}AForm: TCustomForm): boolean; virtual;
    procedure AdjustMainIDEWindowHeight(const {%H-}AIDEWindow: TCustomForm;
      const {%H-}AAdjustHeight: Boolean; const {%H-}ANewHeight: Integer); virtual;
    procedure CloseAll; virtual; // close all forms, called after IDE has saved all and shuts down
    property HideSimpleLayoutOptions: boolean read FHideSimpleLayoutOptions;
  end;

var
  IDEDockMaster: TIDEDockMaster = nil; // can be set by a package

procedure MakeIDEWindowDockable(AControl: TWinControl);
procedure MakeIDEWindowDockSite(AForm: TCustomForm);
procedure CloseAllForms;

procedure Register;

implementation

uses
  LazIDEIntf;

function StrToIDEWindowPlacement(const s: string): TIDEWindowPlacement;
begin
  for Result:=Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
    if UTF8CompareText(s,IDEWindowPlacementNames[Result])=0 then exit;
  Result:=iwpDefault;
end;

function StrToIDEWindowState(const s: string): TIDEWindowState;
begin
  for Result:=Low(TIDEWindowState) to High(TIDEWindowState) do
    if UTF8CompareText(s,IDEWindowStateNames[Result])=0 then exit;
  Result:=iwsNormal;
end;

procedure MakeIDEWindowDockable(AControl: TWinControl);
begin
  if Assigned(IDEDockMaster) then
    IDEDockMaster.MakeIDEWindowDockable(AControl);
end;

procedure MakeIDEWindowDockSite(AForm: TCustomForm);
begin
  if Assigned(IDEDockMaster) then
    IDEDockMaster.MakeIDEWindowDockSite(AForm);
end;

procedure CloseAllForms;
var
  i: Integer;
  AForm: TCustomForm;
  AControl: TWinControl;
begin
  // hide all forms
  i:=Screen.CustomFormCount-1;
  while i>=0 do begin
    AForm:=GetParentForm(Screen.CustomForms[i]);
    AForm.Hide;
    dec(i);
    if i>=Screen.CustomFormCount then i:=Screen.CustomFormCount-1;
  end;

  // close all forms except the MainForm
  i:=Screen.CustomFormCount-1;
  while i>=0 do begin
    AForm:=Screen.CustomForms[i];
    if (AForm<>Application.MainForm) and not AForm.IsParentOf(Application.MainForm)
    then begin
      AControl:=AForm;
      while (AControl.Parent<>nil)
      and (AControl.Parent<>Application.MainForm) do begin
        AControl:=AControl.Parent;
        if AControl is TCustomForm then AForm:=TCustomForm(AControl);
      end;
      AForm.Close;
    end;
    dec(i);
    if i>=Screen.CustomFormCount then i:=Screen.CustomFormCount-1;
  end;
end;

procedure Register;
begin
  RegisterComponents('Misc',[TIDEDialogLayoutStorage]);
end;

{ TSimpleWindowLayoutDividerPosList }

function TSimpleWindowLayoutDividerPosList.GetItems(Index: Integer): TSimpleWindowLayoutDividerPos;
begin
  Result := TSimpleWindowLayoutDividerPos(FList[Index]);
end;

function TSimpleWindowLayoutDividerPosList.GetNamedItems(Index: Integer): TSimpleWindowLayoutDividerPos;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < Count do begin
    if Items[i].DisplayName <> '' then begin
      if Index = 0 then begin
        Result := Items[i];
        exit;
      end;
      dec(Index);
    end;
    inc(i);
  end;
end;

procedure TSimpleWindowLayoutDividerPosList.Merge(AnItem: TSimpleWindowLayoutDividerPos);
var
  i: Integer;
  old: TSimpleWindowLayoutDividerPos;
begin
  i := IndexOf(AnItem.IdString);
  FList.Add(AnItem);
  if i < 0 then
    exit;

  old := Items[i];
  if AnItem.Id < 0 then
    AnItem.SetId(old.Id);
  if AnItem.DisplayName = '' then
    AnItem.SetDisplayName(old.FDisplayName);
  if AnItem.DefaultSize < 0 then
    AnItem.DefaultSize := old.DefaultSize;

  FList.Remove(old);
  old.Free;
end;

constructor TSimpleWindowLayoutDividerPosList.Create;
begin
  FList := TList.Create;
end;

destructor TSimpleWindowLayoutDividerPosList.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FList);
end;

procedure TSimpleWindowLayoutDividerPosList.Assign(ADividerPosList: TSimpleWindowLayoutDividerPosList);
var
  i: Integer;
  tmp: TSimpleWindowLayoutDividerPos;
begin
  Clear;
  for i := 0 to ADividerPosList.Count - 1 do begin
    tmp := Add('');
    tmp.Assign(ADividerPosList[i]);
  end;
end;

procedure TSimpleWindowLayoutDividerPosList.LoadFromConfig(Config: TConfigStorage;
  const Path: string);
var
  tmp: TSimpleWindowLayoutDividerPos;
  c, i: Integer;
begin
  ClearItems;
  c := Config.GetValue(Path+'Count', 0);
  for i := 0 to c - 1 do begin
    tmp := TSimpleWindowLayoutDividerPos.Create('');
    tmp.LoadFromConfig(Config, Path + 'Item' + IntToStr(i) + '/');
    Merge(tmp);
  end;
end;

procedure TSimpleWindowLayoutDividerPosList.SaveToConfig(Config: TConfigStorage;
  const Path: string);
var
  i, c: Integer;
begin
  c := 0;
  for i := 0 to Count - 1 do
    if Items[i].SaveToConfig(Config, Path + 'Item' + IntToStr(c) + '/') then
      inc(c);
  for i := c to Count - 1 do
    Config.DeletePath(Path + 'Item' + IntToStr(i) + '/');
  Config.SetDeleteValue(Path+'Count', c, 0);
end;

procedure TSimpleWindowLayoutDividerPosList.Clear;
begin
  while FList.Count > 0 do begin
    TObject(FList[0]).Free;
    FList.Delete(0);
  end;
end;

procedure TSimpleWindowLayoutDividerPosList.ClearItems;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Clear;
end;

function TSimpleWindowLayoutDividerPosList.Add(AnIdString: String;
  AnId: Integer; ADisplayName: PString): TSimpleWindowLayoutDividerPos;
var
  i: Integer;
begin
  i := IndexOf(AnId);
  if i < 0
  then begin
    Result := TSimpleWindowLayoutDividerPos.Create(AnIdString, AnId, ADisplayName);
    FList.Add(Result);
  end
  else begin
    Result := Items[i];
    if ADisplayName = nil then
      Result.SetDisplayName(ADisplayName);
  end
end;

function TSimpleWindowLayoutDividerPosList.Add(AnIdString: String): TSimpleWindowLayoutDividerPos;
begin
  Result := Add(AnIdString, -1, nil);
end;

function TSimpleWindowLayoutDividerPosList.Count: Integer;
begin
  Result := FList.Count;
end;

function TSimpleWindowLayoutDividerPosList.NamedCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  i := Count - 1;
  while i >= 0 do begin
    if Items[i].DisplayName <> '' then inc(Result);
    dec(i);
  end;
end;

function TSimpleWindowLayoutDividerPosList.Find(AnId: Integer): TSimpleWindowLayoutDividerPos;
var
  i: Integer;
begin
  Result := nil;
  i := IndexOf(AnId);
  if i >= 0 then
    Result := Items[i];
end;

function TSimpleWindowLayoutDividerPosList.Find(AnIdString: String): TSimpleWindowLayoutDividerPos;
var
  i: Integer;
begin
  Result := nil;
  i := IndexOf(AnIdString);
  if i >= 0 then
    Result := Items[i];
end;

function TSimpleWindowLayoutDividerPosList.IndexOf(AnId: Integer): Integer;
begin
  Result := Flist.Count-1;
  while Result >= 0 do begin
    if Items[Result].Id = AnId then
      exit;
    dec(Result);
  end;
end;

function TSimpleWindowLayoutDividerPosList.IndexOf(AnIdString: String): Integer;
begin
  Result := Flist.Count-1;
  while Result >= 0 do begin
    if Items[Result].IdString = AnIdString then
      exit;
    dec(Result);
  end;
end;

function TSimpleWindowLayoutDividerPosList.IndexOf(AnItem: TSimpleWindowLayoutDividerPos): Integer;
begin
  Result := FList.IndexOf(AnItem);
end;

{ TSimpleWindowLayoutDividerPos }

function TSimpleWindowLayoutDividerPos.GetDisplayName: String;
begin
  if FDisplayName = nil then
    Result := ''
  else
    Result := FDisplayName^;
end;

procedure TSimpleWindowLayoutDividerPos.SetDisplayName(ADisplayName: PString);
begin
  FDisplayName := ADisplayName;
end;

procedure TSimpleWindowLayoutDividerPos.SetId(AnId: Integer);
begin
  FId := AnId;
end;

constructor TSimpleWindowLayoutDividerPos.Create(AnIdString: String);
begin
  Create(AnIdString, -1, nil);
end;

constructor TSimpleWindowLayoutDividerPos.Create(AnIdString: String; AnId: Integer;
  ADisplayName: PString);
begin
  FDefaultSize := -1;
  Clear;
  FId := AnId;
  FIdString := AnIdString;
  FDisplayName := ADisplayName;
end;

procedure TSimpleWindowLayoutDividerPos.Assign(ADividerPos: TSimpleWindowLayoutDividerPos);
begin
  FDefaultSize := ADividerPos.FDefaultSize;
  FDisplayName := ADividerPos.FDisplayName;
  FId          := ADividerPos.FId;
  FIdString    := ADividerPos.FIdString;
  FPlacement   := ADividerPos.FPlacement;
  FSize        := ADividerPos.FSize;
end;

procedure TSimpleWindowLayoutDividerPos.LoadFromConfig(Config: TConfigStorage;
  const Path: string);
var
  s: String;
begin
  Clear;
  FIdString := Config.GetValue(Path+'ID', '');
  FSize := Config.GetValue(Path+'Size', -1);
  s := Config.GetValue(Path+'Placement', 'iwpdUseWindowSetting');
  try
    ReadStr(s, FPlacement);
  except
    FPlacement := iwpdUseWindowSetting;
  end;
end;

function TSimpleWindowLayoutDividerPos.SaveToConfig(Config: TConfigStorage;
  const Path: string): Boolean;
var
  s: String;
begin
  Result := (FSize <> -1) or (FPlacement <> iwpdUseWindowSetting);
  if not Result then
    exit;
  WriteStr(s, FPlacement);
  Config.SetDeleteValue(Path+'ID', FIdString, '');
  Config.SetDeleteValue(Path+'Size', FSize, -1);
  Config.SetDeleteValue(Path+'Placement', s, 'iwpdUseWindowSetting');
end;

procedure TSimpleWindowLayoutDividerPos.Clear;
begin
  FSize := FDefaultSize;
  FPlacement := iwpdUseWindowSetting;
end;

{ TIDEDialogLayout }

procedure TIDEDialogLayout.SetList(const AValue: TIDEDialogLayoutList);
begin
  if FList=AValue then exit;
  FList:=AValue;
  if (List<>nil) and Modified then List.Modified:=true;
end;

procedure TIDEDialogLayout.SetModified(const AValue: boolean);
begin
  FModified:=AValue;
  if FModified and (FList<>nil) then FList.Modified:=true;
end;

procedure TIDEDialogLayout.SetWidth(const AValue: integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
  Modified:=true;
end;

procedure TIDEDialogLayout.SetHeight(const AValue: integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
  Modified:=true;
end;

constructor TIDEDialogLayout.Create(const TheName: string;
  TheList: TIDEDialogLayoutList);
begin
  FName:=TheName;
  FList:=TheList;
end;

procedure TIDEDialogLayout.Assign(Source: TIDEDialogLayout);
begin
  FName := Source.FName;
  FWidth := Source.FWidth;
  FHeight := Source.FHeight;
  FModified := True;
end;

function TIDEDialogLayout.SizeValid: boolean;
begin
  Result:=(Width>10) and (Height>10);
end;

procedure TIDEDialogLayout.LoadFromConfig(Config: TConfigStorage; const Path: string);
begin
  FName:=Config.GetValue(Path+'Name/Value','');
  FWidth:=Config.GetValue(Path+'Size/Width',0);
  FHeight:=Config.GetValue(Path+'Size/Height',0);
  Modified:=false;
end;

procedure TIDEDialogLayout.SaveToConfig(Config: TConfigStorage; const Path: string);
begin
  Config.SetValue(Path+'Name/Value',Name);
  Config.SetValue(Path+'Size/Width',Width);
  Config.SetValue(Path+'Size/Height',Height);
  Modified:=false;
end;

{ TIDEDialogLayoutList }

function TIDEDialogLayoutList.GetItems(Index: integer): TIDEDialogLayout;
begin
  Result:=TIDEDialogLayout(FItems[Index]);
end;

procedure TIDEDialogLayoutList.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
end;

constructor TIDEDialogLayoutList.Create;
begin
  inherited Create;
  FItems:=TList.Create;
  FItemClass:=TIDEDialogLayout;
end;

destructor TIDEDialogLayoutList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TIDEDialogLayoutList.Assign(Source: TIDEDialogLayoutList);
var
  i: Integer;
  Layout: TIDEDialogLayout;
begin
  FItemClass := Source.FItemClass;
  Clear;
  for i:=0 to Source.FItems.Count-1 do begin
    Layout := TIDEDialogLayout.Create(Source.Items[i].Name, Self);
    Layout.Assign(Source.Items[i]);
    FItems.Add(Layout);
  end;
  FModified := True;
end;

procedure TIDEDialogLayoutList.ApplyLayout(ADialog: TControl;
  DefaultWidth, DefaultHeight: integer; UseAsMin: boolean);
var
  ALayout: TIDEDialogLayout;
  NewWidth, NewHeight: integer;
begin
  if (ADialog=nil) or (Self=nil) then exit;
  ALayout:=Find(ADialog,true);
  //debugln(['TIDEDialogLayoutList.ApplyLayout ',ALayout.Name,' ',ALayout.SizeValid,' ',ALayout.Width,',',ALayout.Height]);
  if ALayout.SizeValid then begin
    NewWidth:=ALayout.Width;
    NewHeight:=ALayout.Height;
  end else begin
    NewWidth:=DefaultWidth;
    NewHeight:=DefaultHeight;
  end;
  if UseAsMin then begin
    if NewWidth<DefaultWidth then NewWidth:=DefaultWidth;
    if NewHeight<DefaultHeight then NewHeight:=DefaultHeight;
  end;
  ADialog.SetBounds(ADialog.Left,ADialog.Top,NewWidth,NewHeight);
end;

procedure TIDEDialogLayoutList.ApplyLayout(ADialog: TControl);
begin
  ApplyLayout(ADialog,ADialog.Width,ADialog.Height);
end;

procedure TIDEDialogLayoutList.SaveLayout(ADialog: TControl);
var
  ALayout: TIDEDialogLayout;
begin
  if (ADialog=nil) or (Self=nil) then exit;
  ALayout:=Find(ADialog,true);
  ALayout.Width:=ADialog.Width;
  ALayout.Height:=ADialog.Height;
end;

procedure TIDEDialogLayoutList.Clear;
var i: integer;
begin
  for i:=0 to FItems.Count-1 do
    Items[i].Free;
  FItems.Clear;
end;

function TIDEDialogLayoutList.Count: integer;
begin
  Result:=FItems.Count;
end;

function TIDEDialogLayoutList.Find(const DialogName: string;
  CreateIfNotExists: boolean): TIDEDialogLayout;
var i: integer;
begin
  i:=IndexOf(DialogName);
  if (i<0) then begin
    if CreateIfNotExists then begin
      Result:=FItemClass.Create(DialogName,Self);
      FItems.Add(Result);
    end else begin
      Result:=nil;
    end;
  end else begin
    Result:=Items[i];
  end;
end;

function TIDEDialogLayoutList.Find(ADialog: TObject; CreateIfNotExists: boolean
  ): TIDEDialogLayout;
begin
  if ADialog<>nil then begin
    Result:=Find(ADialog.ClassName,CreateIfNotExists);
  end else begin
    Result:=nil;
  end;
end;

function TIDEDialogLayoutList.IndexOf(const DialogName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (CompareText(DialogName,Items[Result].Name)<>0) do
    dec(Result);
end;

procedure TIDEDialogLayoutList.LoadFromConfig(Config: TConfigStorage; const Path: string);
var
  NewCount, i: integer;
  NewDialogLayout: TIDEDialogLayout;
begin
  Clear;
  NewCount:=Config.GetValue(Path+'Count',0);
  for i:=0 to NewCount-1 do begin
    NewDialogLayout:=FItemClass.Create('',Self);
    FItems.Add(NewDialogLayout);
    NewDialogLayout.LoadFromConfig(Config,Path+'Dialog'+IntToStr(i+1)+'/');
  end;
  Modified:=false;
end;

procedure TIDEDialogLayoutList.SaveToConfig(Config: TConfigStorage; const Path: string);
var i: integer;
begin
  Config.SetDeleteValue(Path+'Count',Count,0);
  for i:=0 to Count-1 do
    Items[i].SaveToConfig(Config,Path+'Dialog'+IntToStr(i+1)+'/');
  Modified:=false;
end;

{ TIDEDialogLayoutStorage }

procedure TIDEDialogLayoutStorage.OnCreateForm(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Sender as TControl);
end;

procedure TIDEDialogLayoutStorage.OnCloseForm(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Sender as TControl);
end;

constructor TIDEDialogLayoutStorage.Create(TheOwner: TComponent);
var
  Form: TCustomForm;
begin
  inherited Create(TheOwner);
  if Owner is TCustomForm then
  begin
    Form:=TCustomForm(Owner);
    Form.AddHandlerCreate(@OnCreateForm);
    Form.AddHandlerClose(@OnCloseForm);
  end;
end;

destructor TIDEDialogLayoutStorage.Destroy;
var
  Form: TCustomForm;
begin
  if Owner is TCustomForm then
  begin
    Form:=TCustomForm(Owner);
    Form.RemoveAllHandlersOfObject(Self);
  end;
  inherited Destroy;
end;

{ TSimpleWindowLayout }

constructor TSimpleWindowLayout.Create(AFormID: string);
var
  Creator: TIDEWindowCreator;
begin
  inherited Create(nil);
  FDividers := TSimpleWindowLayoutDividerPosList.Create;
  FormID := AFormID;
  fDefaultWindowPlacement := iwpRestoreWindowGeometry;
  Clear;
  Creator := IDEWindowCreators.FindWithName(AFormID);
  if Creator <> nil then
    Creator.InitSimpleLayout(Self);
end;

destructor TSimpleWindowLayout.Destroy;
begin
  Form:=nil;
  inherited Destroy;
  FDividers.Free;
end;

procedure TSimpleWindowLayout.LoadFromConfig(Config: TConfigStorage; const Path: string);
var
  P: string;
begin
  // set all values to default
  Clear;
  // read settings
  // build path
  P:=FormID;
  if P='' then exit;
  P:=Path+P+'/';
  FFormCaption := Config.GetValue(P+'Caption/Value', fFormID);
  // placement
  fWindowPlacement:=StrToIDEWindowPlacement(Config.GetValue(
    P+'WindowPlacement/Value',IDEWindowPlacementNames[fWindowPlacement]));
  // custom position
  Left := Config.GetValue(P+'CustomPosition/Left', Left);
  Top := Config.GetValue(P+'CustomPosition/Top', Top);
  Width := Config.GetValue(P+'CustomPosition/Width', Width);
  Height := Config.GetValue(P+'CustomPosition/Height', Height);
  // state
  fWindowState:=StrToIDEWindowState(Config.GetValue(
    P+'WindowState/Value',IDEWindowStateNames[fWindowState]));
  FVisible:=Config.GetValue(P+'Visible/Value',false);
  //debugln(['TSimpleWindowLayout.LoadFromConfig ',FormID,' ',Left,',',Top,',',Width,',',Height]);
  FDividers.LoadFromConfig(Config, P + 'Divider/');
end;

procedure TSimpleWindowLayout.SaveToConfig(Config: TConfigStorage;
  const Path: string);
var
  P: string;
begin
  // build path
  P:=FormID;
  if P='' then exit;
  P:=Path+P+'/';
  Config.SetDeleteValue(P+'Caption/Value',FFormCaption,'');
  // placement
  Config.SetDeleteValue(P+'WindowPlacement/Value',
    IDEWindowPlacementNames[fWindowPlacement],
    IDEWindowPlacementNames[iwpRestoreWindowSize]);
  // custom position
  Config.SetDeleteValue(P+'CustomPosition/Left', Left, 0);
  Config.SetDeleteValue(P+'CustomPosition/Top', Top, 0);
  Config.SetDeleteValue(P+'CustomPosition/Width', Width, 0);
  Config.SetDeleteValue(P+'CustomPosition/Height', Height, 0);
  // state
  Config.SetValue(P+'WindowState/Value',IDEWindowStateNames[fWindowState]);
  Config.SetDeleteValue(P+'Visible/Value',FVisible,false);
  FDividers.SaveToConfig(Config, P + 'Divider/');
end;

procedure TSimpleWindowLayout.SetDefaultPosition(const AForm: TCustomForm);
begin
  FDefaultLeft := AForm.Left;
  FDefaultTop := AForm.Top;
  FDefaultWidth := AForm.Width;
  FDefaultHeight := AForm.Height;
end;

procedure TSimpleWindowLayout.OnFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  GetCurrentPosition;
end;

procedure TSimpleWindowLayout.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if fForm=AComponent then begin
      fForm:=nil;
      Applied:=false;
    end;
  end;
end;

function TSimpleWindowLayout.CustomCoordinatesAreValid: boolean;
begin
  Result:=(Width>0) and (Height>0); // and (Left>10-Width) and (Top>10-Height);
end;

procedure TSimpleWindowLayout.CloseForm;
begin
  GetCurrentPosition;
  Form:=nil;
end;

function TSimpleWindowLayout.ValidateAndSetCoordinates(const aForce: Boolean
  ): Boolean;
var
  i: Integer;
  NewBounds: TRect;
begin
  Result := False;
  if aForce or CustomCoordinatesAreValid then begin
    if not CustomCoordinatesAreValid then//default position
      NewBounds := Bounds(DefaultLeft, DefaultTop, DefaultWidth, DefaultHeight)
    else// explicit position
      NewBounds := Bounds(Left, Top, Width, Height);

    // set minimum size
    if NewBounds.Right - NewBounds.Left < 60 then
      NewBounds.Right := NewBounds.Left + 60;
    if NewBounds.Bottom - NewBounds.Top < 60 then
      NewBounds.Bottom := NewBounds.Top + 60;

    // Move to visible area :
    // window is out at left side of screen
    if NewBounds.Right < Screen.DesktopLeft + 60 then
      OffsetRect(NewBounds, Screen.DesktopLeft + 60 - NewBounds.Right, 0);

    // window is out above the screen
    if NewBounds.Bottom < Screen.DesktopTop+60 then
      OffsetRect(NewBounds, 0, Screen.DesktopTop + 60 - NewBounds.Bottom);

    // window is out at right side of screen, i = right edge of screen - 60
    i := Screen.DesktopWidth + Screen.DesktopLeft - 60;
    if NewBounds.Left > i then begin
      NewBounds.Left := i;
      NewBounds.Right := NewBounds.Right + i - NewBounds.Left;
    end;

    // window is out below the screen, i = bottom edge of screen - 60
    i := Screen.DesktopHeight + Screen.DesktopTop - 60;
    if NewBounds.Top > i then begin
      NewBounds.Top := i;
      NewBounds.Bottom := NewBounds.Bottom + i - NewBounds.Top;
    end;

    // set bounds (do not use SetRestoredBounds - that flickers with the current LCL implementation)
    FForm.SetBounds(NewBounds.Left, NewBounds.Top,
                    NewBounds.Right - NewBounds.Left,
                    NewBounds.Bottom - NewBounds.Top);
    Result := True;
  end;
end;

function TSimpleWindowLayout.FormBaseID(out SubIndex: Integer): String;
var
  i: Integer;
begin
  Result := FormID;
  SubIndex := -1;
  i := length(Result);
  while (i > 0) and (Result[i] in ['0'..'9']) do
    dec(i);
  if (i < 1) or (i = length(Result)) then
    exit;
  SubIndex := StrToInt(copy(Result, i+1, length(Result)));
  Result := copy(Result, 1, i);
end;

procedure TSimpleWindowLayout.SetForm(const AValue: TCustomForm);
begin
  if fForm=AValue then exit;
  if Assigned(Form) then
  begin
    RemoveFreeNotification(Form);
    Form.RemoveHandlerClose(@OnFormClose);
  end;
  fForm:=AValue;
  if Assigned(Form) then
  begin
    fFormID := Form.Name;
    FFormCaption := Form.Caption;
    FreeNotification(Form);
    Form.AddHandlerClose(@OnFormClose);
    Applied:=false;
  end;
end;

function TSimpleWindowLayout.GetFormID: string;
begin
  if Form=nil then
    Result:=fFormID
  else
    Result:=Form.Name;
end;

function TSimpleWindowLayout.GetFormCaption: string;
begin
  if Form<>nil then
    FFormCaption:=Form.Caption
  else if FFormCaption='' then
    FFormCaption:=FormID;
  Result:=FFormCaption;
end;

procedure TSimpleWindowLayout.Clear;
begin
  //debugln(['TSimpleWindowLayout.Clear ',FormID]);
  fApplied := False;
  fVisible := False;
  fWindowPlacement:=fDefaultWindowPlacement;
  fLeft:=0;
  fTop:=0;
  fWidth:=0;
  fHeight:=0;
  fWindowState:=iwsNormal;
  FDividers.ClearItems;
end;

procedure TSimpleWindowLayout.ReadCurrentCoordinates;
var
  p: TPoint;
begin
  if (Form<>nil) and (Form.WindowState=wsNormal) then begin
    if Form.Parent<>nil then
      p:=Form.ClientOrigin
    else
      p:=Point(0,0);
    Left:=Form.Left+p.X;
    Top:=Form.Top+p.Y;
    Width:=Form.Width;
    Height:=Form.Height;
  end;
end;

procedure TSimpleWindowLayout.ReadCurrentState;
begin
  Visible:=(Form<>nil) and Form.IsVisible;
  if Form<>nil then begin
    case Form.WindowState of
    wsMinimized: fWindowState:=iwsMinimized;
    wsMaximized: fWindowState:=iwsMaximized;
    else
      fWindowState:=iwsNormal;
    end;
  end;
end;

procedure TSimpleWindowLayout.Assign(Layout: TSimpleWindowLayout);
begin
  Clear;
  Assert(FFormID = Layout.FFormID);
  //IMPORTANT: do not assign FForm and FFormID!
  FVisible:=Layout.FVisible;
  FWindowPlacement:=Layout.FWindowPlacement;
  FLeft:=Layout.FLeft;
  FTop:=Layout.FTop;
  FWidth:=Layout.FWidth;
  FHeight:=Layout.FHeight;
  FWindowState:=Layout.FWindowState;
  FDefaultWindowPlacement:=Layout.FDefaultWindowPlacement;
  FDividers.Assign(Layout.FDividers);
end;

procedure TSimpleWindowLayout.ReadCurrentDividers(AForce: Boolean = False);
var
  i, j: Integer;
  f: Boolean;
  Creator: TIDEWindowCreator;
begin
  Creator:=IDEWindowCreators.FindWithName(FormID);
  if (Creator = nil) or (Creator.OnGetDividerSize = nil) then exit;
  if fForm = nil then exit;
  for i := 0 to FDividers.Count - 1 do begin
    if FDividers[i].FId < 0 then continue;
    f := AForce;
    case FDividers[i].Placement of
      iwpdRestore:
        f := true;
      iwpdUseWindowSetting:
        f := WindowPlacement in [iwpRestoreWindowGeometry, iwpRestoreWindowSize];
    end;
    if f then begin
      j:=-1;
      if Creator.OnGetDividerSize(fForm, FDividers[i].Id, j) then
        FDividers[i].Size := j
      else
        FDividers[i].Size := -1; // Default / Not Changed / Unavailable
    end;
  end;
end;

procedure TSimpleWindowLayout.GetCurrentPosition;
begin
  //debugln('TSimpleWindowLayout.GetCurrentPosition ',DbgSName(Self),' ',FormID,' ',IDEWindowPlacementNames[WindowPlacement]);
  case WindowPlacement of
  iwpRestoreWindowGeometry, iwpRestoreWindowSize:
    ReadCurrentCoordinates;
  end;
  ReadCurrentDividers;
  ReadCurrentState;
  //debugln('TSimpleWindowLayout.GetCurrentPosition ',DbgSName(Self),' ',FormID,' Width=',dbgs(Width));
end;

function TSimpleWindowLayout.Apply(const aForce: Boolean): Boolean;
begin
  Result := False;
  if fForm = nil then exit;
  Applied:=true;
  {$IFDEF VerboseIDEDocking}
  debugln(['TSimpleWindowLayoutList.ApplyAndShow restore ',
          FormID,' ',IDEWindowPlacementNames[WindowPlacement],
          ' ',Left,',',Top,',',Width,',',Height]);
  {$ENDIF}

  case WindowPlacement of
  iwpCustomPosition,iwpRestoreWindowGeometry:
    begin
      //DebugLn(['TMainIDE.OnApplyWindowLayout ',IDEWindowStateNames[WindowState]]);
      case WindowState of
        iwsMinimized: FForm.WindowState:=wsMinimized;
        iwsMaximized: FForm.WindowState:=wsMaximized;
      end;
      Result := ValidateAndSetCoordinates(aForce);     // Adjust bounds to screen area and apply them.
      if WindowState in [iwsMinimized, iwsMaximized] then
        Result := True;
    end;
  iwpUseWindowManagerSetting:
    Result := True;
  end;

  ApplyDivider(aForce);
end;

procedure TSimpleWindowLayout.ApplyDivider(AForce: Boolean = False);
var
  i: Integer;
  f: Boolean;
  Creator: TIDEWindowCreator;
begin
  Creator:=IDEWindowCreators.FindWithName(FormID);
  if (Creator <> nil) and (Creator.OnSetDividerSize <> nil) then begin
    for i := 0 to FDividers.Count - 1 do begin
      if (FDividers[i].FId < 0) or (FDividers[i].Size < 0) then continue;
      f := AForce;
      case FDividers[i].Placement of
        iwpdRestore, iwpdCustomSize:
          f := true;
        iwpdUseWindowSetting:
          f := WindowPlacement in [iwpRestoreWindowGeometry, iwpRestoreWindowSize];
      end;
      if f then
        Creator.OnSetDividerSize(fForm, FDividers[i].Id, FDividers[i].Size);
    end;
  end;
end;

{ TSimpleWindowLayoutList }

procedure TSimpleWindowLayoutList.Clear;
var
  i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  fItems.Clear;
end;

procedure TSimpleWindowLayoutList.Delete(Index: Integer);
begin
  Items[Index].Free;
  fItems.Delete(Index);
end;

function TSimpleWindowLayoutList.GetItems(Index: Integer): TSimpleWindowLayout;
begin
  Result:=TSimpleWindowLayout(fItems[Index]);
end;

procedure TSimpleWindowLayoutList.OnScreenRemoveForm(Sender: TObject;
  AForm: TCustomForm);
begin
  CloseForm(AForm);
end;

constructor TSimpleWindowLayoutList.Create(ARegisterEventHandlers: Boolean);
begin
  fItems:=TFPList.Create;
  fRegisterEventHandlers := ARegisterEventHandlers;
  if ARegisterEventHandlers then
    Screen.AddHandlerRemoveForm(@OnScreenRemoveForm);
end;

destructor TSimpleWindowLayoutList.Destroy;
begin
  Screen.RemoveHandlerRemoveForm(@OnScreenRemoveForm);
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

function TSimpleWindowLayoutList.IndexOf(const FormID: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (FormID<>Items[Result].GetFormID) do dec(Result);
end;

function TSimpleWindowLayoutList.IndexOf(const AForm: TCustomForm): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AForm<>Items[Result].Form) do dec(Result);
end;

procedure TSimpleWindowLayoutList.LoadFromConfig(Config: TConfigStorage; const Path: string);
// do not clear, just add/replace the values from the config
var
  i: integer;
  ID: String;
  xLayoutIndex: Integer;
begin
  // create new windows
  i := Config.GetValue(Path+'Desktop/FormIdCount', 0);
  //debugln(['TSimpleWindowLayoutList.LoadFromConfig ',i]);
  while i > 0 do begin
    ID := Config.GetValue(Path+'Desktop/FormIdList/a'+IntToStr(i), '');
    //debugln(['TSimpleWindowLayoutList.LoadFromConfig ',i,' ',ID]);
    if (ID <> '') and IsValidIdent(ID) then
    begin
      xLayoutIndex := IndexOf(ID);
      if (xLayoutIndex = -1) then
      begin
        CreateWindowLayout(ID);
        xLayoutIndex := Count-1;
      end;
      fItems.Move(xLayoutIndex, 0);
    end;
    dec(i);
  end;

  for i:=0 to Count-1 do
    Items[i].LoadFromConfig(Config,Path);
end;

procedure TSimpleWindowLayoutList.SaveToConfig(Config: TConfigStorage; const Path: string);
var
  i: integer;
begin
  Config.SetDeleteValue(Path+'Desktop/FormIdCount',Count,0);
  //debugln(['TSimpleWindowLayoutList.SaveToConfig ',Count]);
  for i:=0 to Count-1 do begin
    Config.SetDeleteValue(Path+'Desktop/FormIdList/a'+IntToStr(i+1),Items[i].FormID,'');
    Items[i].SaveToConfig(Config,Path);
  end;
end;

procedure TSimpleWindowLayoutList.SetDefaultPosition(const AForm: TCustomForm);
var
  xLayout: TSimpleWindowLayout;
begin
  if AForm.Name <> '' then
  begin
    xLayout:=ItemByFormID(AForm.Name);
    if xLayout<>nil then
      xLayout.SetDefaultPosition(AForm);
  end;
end;

function TSimpleWindowLayoutList.Count: integer;
begin
  Result:=fItems.Count;
end;

function TSimpleWindowLayoutList.ItemByForm(AForm: TCustomForm): TSimpleWindowLayout;
var
  i: integer;
begin
  i:=IndexOf(AForm);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TSimpleWindowLayoutList.ItemByFormID(const FormID: string): TSimpleWindowLayout;
var
  i: integer;
begin
  i:=IndexOf(FormID);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TSimpleWindowLayoutList.ItemByFormCaption(const aFormCaption: string): TSimpleWindowLayout;
var
  i: integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    Result := Items[i];
    if Result.FormCaption = aFormCaption then
      exit;
    dec(i);
  end;
  Result:=nil;
end;

procedure TSimpleWindowLayoutList.CloseForm(AForm: TCustomForm);
var
  ALayout: TSimpleWindowLayout;
begin
  ALayout:=ItemByForm(AForm);
  if ALayout<>nil then
    ALayout.CloseForm;
end;

procedure TSimpleWindowLayoutList.ApplyAndShow(Sender: TObject; AForm: TCustomForm;
  BringToFront: boolean; AMoveToVisbleMode: TLayoutMoveToVisbleMode);

  function IsAnyCornerVisible(AForm: TCustomForm): Boolean;
  var
    Overlap: TRect;
  begin
    Overlap:=Rect(0,0,0,0);
    IntersectRect(Overlap, AForm.BoundsRect, AForm.Monitor.WorkareaRect);
    Result := (Overlap.Bottom > Overlap.Top + 1) and
              (Overlap.Right > Overlap.Left + 1);
  end;

var
  ALayout: TSimpleWindowLayout;
  NewBounds: TRect;
  Creator: TIDEWindowCreator;
  DockSiblingName: string;
  DockAlign: TAlign;
  DockSibling: TCustomForm;
  DockSiblingBounds: TRect;
  Offset: TPoint;
  ARestoreVisible: Boolean;
begin
  {$IFDEF VerboseIDEDocking}
  debugln(['TSimpleWindowLayoutList.ApplyAndShow Form=',DbgSName(AForm),' ',BringToFront,
           '  DesktopWidth=', Screen.DesktopWidth, '  DesktopHeight=', Screen.DesktopHeight]);
  {$ENDIF}
  try
    ALayout:=ItemByFormID(AForm.Name);
    if ALayout<>nil then
    begin
      ALayout.Form:=AForm;
      if ALayout.Applied then exit;
      if ALayout.Apply then exit;
    end;

    {$IFDEF VerboseIDEDocking}
    debugln(['TSimpleWindowLayoutList.ApplyAndShow no stored layout found, layout registered=',
            ALayout<>nil,' AForm=',DbgSName(AForm)]);
    {$ENDIF}

    // no layout found => use default
    Creator:=IDEWindowCreators.FindWithName(AForm.Name);
    if Creator<>nil then
    begin
      if Creator.OnGetLayout<>nil then
        Creator.OnGetLayout(Self,AForm.Name,NewBounds,DockSiblingName,DockAlign)
      else begin
        Creator.GetDefaultBounds(AForm,NewBounds);
        DockSiblingName:=Creator.DockSibling;
        DockAlign:=Creator.DockAlign;
      end;
      {$IFDEF VerboseIDEDocking}
      debugln(['TSimpleWindowLayoutList.ApplyAndShow creator found for ',DbgSName(AForm),': Left=',Creator.Left,' Top=',Creator.Top,' Right=',Creator.Right,' Bottom=',Creator.Bottom,' Creator.DockSibling=',Creator.DockSibling,' Creator.DockAlign=',dbgs(Creator.DockAlign),' NewBounds=',dbgs(NewBounds),' DockSibling=',DockSiblingName,' DockAlign=',dbgs(DockAlign)]);
      {$ENDIF}
      if DockSiblingName<>'' then
      begin
        DockSibling:=Screen.FindForm(DockSiblingName);
        if DockSibling<>nil then
        begin
          DockSiblingBounds:=DockSibling.BoundsRect;
          if DockSibling.Parent<>nil then
          begin
            Offset:=DockSibling.ClientToScreen(Point(0,0));
            OffsetRect(DockSiblingBounds,Offset.X,Offset.Y);
          end;
          case DockAlign of
          alLeft:
            begin
              NewBounds.Top:=DockSiblingBounds.Top;
              NewBounds.Bottom:=DockSiblingBounds.Bottom;
              OffsetRect(NewBounds,DockSiblingBounds.Left-6-NewBounds.Right,0);
            end;
          alRight:
            begin
              NewBounds.Top:=DockSiblingBounds.Top;
              NewBounds.Bottom:=DockSiblingBounds.Bottom;
              OffsetRect(NewBounds,DockSiblingBounds.Right+6-NewBounds.Left,0);
            end;
          alTop:
            begin
              NewBounds.Left:=DockSiblingBounds.Left;
              NewBounds.Right:=DockSiblingBounds.Right;
              OffsetRect(NewBounds,0,DockSiblingBounds.Top-25-NewBounds.Bottom);
            end;
          alBottom:
            begin
              NewBounds.Left:=DockSiblingBounds.Left;
              NewBounds.Right:=DockSiblingBounds.Right;
              OffsetRect(NewBounds,0,DockSiblingBounds.Bottom+25-NewBounds.Top);
            end;
          alClient:
            NewBounds:=DockSibling.BoundsRect;
          end;
        end;
      end;
      {$IFDEF VerboseIDEDocking}
      debugln(['TSimpleWindowLayoutList.ApplyAndShow ',DbgSName(AForm),' NewBounds=',dbgs(NewBounds)]);
      {$ENDIF}
      NewBounds.Left:=Min(10000,Max(-10000,NewBounds.Left));
      NewBounds.Top:=Min(10000,Max(-10000,NewBounds.Top));
      NewBounds.Right:=Max(NewBounds.Left+100,NewBounds.Right);
      NewBounds.Bottom:=Max(NewBounds.Top+100,NewBounds.Bottom);
      AForm.BoundsRect:=NewBounds;
      AMoveToVisbleMode := vmAlwaysMoveToVisible;
    end;
  finally
    if (AForm.WindowState in [wsNormal,wsMaximized]) and BringToFront then
    begin
      // do not change Visible property while designing form. issue #20602
      // we save it into ARestoreVisible before any changes from here.
      if (csDesigning in AForm.ComponentState) and (AForm.Designer <> nil) then
        ARestoreVisible := AForm.Visible;

      if (AMoveToVisbleMode = vmAlwaysMoveToVisible) or
         ( (AMoveToVisbleMode = vmOnlyMoveOffScreenToVisible) and
           (not IsAnyCornerVisible(AForm)) )
      then
        AForm.EnsureVisible(true)
      else
        AForm.ShowOnTop;

      if (csDesigning in AForm.ComponentState) and (AForm.Designer <> nil) then
        AForm.Visible := ARestoreVisible;
    end else
    begin
      if (csDesigning in AForm.ComponentState) and (AForm.Designer <> nil) then
        ARestoreVisible := AForm.Visible;

      AForm.Visible := True;
      if (AForm.WindowState in [wsMinimized]) then
      begin
        if BringToFront then
        begin
          // issue #19769
          if AForm.Visible and not (fsModal in AForm.FormState) then
            AForm.Visible := False;
          if (AMoveToVisbleMode = vmAlwaysMoveToVisible) or
             ( (AMoveToVisbleMode = vmOnlyMoveOffScreenToVisible) and
               (not IsAnyCornerVisible(AForm)) )
          then
            AForm.EnsureVisible(true)
          else
            AForm.ShowOnTop;
        end;
      end;

      if (csDesigning in AForm.ComponentState) and (AForm.Designer <> nil) then
        AForm.Visible := ARestoreVisible;
    end;
  end;
end;

procedure TSimpleWindowLayoutList.StoreWindowPositions;
var
  i: integer;
  xOldLayoutIndex: Integer;
begin
  //store positions
  for i:=0 to Count-1 do
    Items[i].GetCurrentPosition;

  //store z-order
  for i:=Screen.CustomFormCount-1 downto 0 do
  begin
    xOldLayoutIndex := IndexOf(Screen.CustomForms[i]);
    if xOldLayoutIndex > 0 then
      fItems.Move(xOldLayoutIndex, 0);
  end;
end;

procedure TSimpleWindowLayoutList.Assign(SrcList: TSimpleWindowLayoutList;
  AssignOnlyNewlyCreated, DestroyNotAssigned, AssignOrder: Boolean);
var
  i: integer;
  xItemIndex: Integer;
  xNewlyCreated: Boolean;
begin
  if SrcList=nil then exit;
  for i:=0 to SrcList.Count-1 do begin
    xNewlyCreated := False;
    if (i >= Self.Count) or (Self.Items[i].FormID <> SrcList[i].FormID) then
    begin//the target item has not the same index, find it!
      xItemIndex := IndexOf(SrcList[i].FormID);
      if xItemIndex < 0 then
      begin
        xItemIndex := Self.Add(TSimpleWindowLayout.Create(SrcList[i].FormID));//not found, create
        xNewlyCreated := True;
      end;
      if (xItemIndex<>i) and (AssignOrder) then
      begin
        Self.fItems.Move(xItemIndex, i);//move it to right position
        xItemIndex := i;
      end;
    end else
      xItemIndex := i;
    if not AssignOnlyNewlyCreated or xNewlyCreated then
      Self.Items[xItemIndex].Assign(SrcList[i])
  end;

  for i := Count-1 downto 0 do
  if SrcList.IndexOf(Items[i].FormID) = -1 then
  begin
    if DestroyNotAssigned then
      Delete(i)
    else
      Items[i].Clear;
  end;
end;

function TSimpleWindowLayoutList.Add(ALayout: TSimpleWindowLayout): integer;
begin
  Result:=fItems.Add(ALayout);
end;

function TSimpleWindowLayoutList.CreateWindowLayout(const TheFormID: string
  ): TSimpleWindowLayout;
begin
  if TheFormID='' then
    raise Exception.Create('TEnvironmentOptions.CreateWindowLayout TheFormID empty');
  if not IsValidIdent(TheFormID) then
    raise Exception.Create('TEnvironmentOptions.CreateWindowLayout TheFormID "'+TheFormID+'" invalid identifier');
  if ItemByFormID(TheFormID)<>nil then
    raise Exception.Create('TEnvironmentOptions.CreateWindowLayout TheFormID "'+TheFormID+'" already exists');
  Result:=TSimpleWindowLayout.Create(TheFormID);
  Add(Result);
end;

function TSimpleWindowLayoutList.CreateWindowLayout(const TheForm: TCustomForm
  ): TSimpleWindowLayout;
begin
  Result:=CreateWindowLayout(TheForm.Name);
  Result.Form:=TheForm;
end;

{ TIDEWindowCreator }

procedure TIDEWindowCreator.SetBottom(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FBottom=AValue then exit;
  FBottom:=AValue;
end;

function TIDEWindowCreator.GetDividerTemplate: TSimpleWindowLayoutDividerPosList;
begin
  If FDividerTemplate = nil then
    FDividerTemplate := TSimpleWindowLayoutDividerPosList.Create;
  Result := FDividerTemplate;
end;

procedure TIDEWindowCreator.SetLeft(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FLeft=AValue then exit;
  FLeft:=AValue;
end;

procedure TIDEWindowCreator.SetTop(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FTop=AValue then exit;
  FTop:=AValue;
end;

procedure TIDEWindowCreator.SetRight(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FRight=AValue then exit;
  FRight:=AValue;
end;

procedure TIDEWindowCreator.CheckBoundValue(s: string);
var
  p: Integer;
begin
  if s='' then exit;
  p:=1;
  if (p<=length(s)) and (s[p] in ['+','-']) then inc(p);
  while (p<=length(s)) and (s[p] in ['0'..'9']) do inc(p);
  if p<=1 then
    raise Exception.Create('TIDEWindowDefaultLayout.CheckBoundValue: expected number, but '+s+' found');
  // check for percent
  if (p<=length(s)) and (s[p]='%') then inc(p);
  if p<=length(s) then
    raise Exception.Create('TIDEWindowDefaultLayout.CheckBoundValue: expected number, but '+s+' found');
end;

procedure TIDEWindowCreator.GetDefaultBounds(AForm: TCustomForm; out
  DefBounds: TRect);
var
  aRight: LongInt;
  aBottom: LongInt;
  ScreenR: TRect;
  ScreenW: Integer;
  ScreenH: Integer;
begin
  ScreenR:=IDEWindowCreators.GetScreenrectForDefaults;
  ScreenW:=ScreenR.Right-ScreenR.Left;
  ScreenH:=ScreenR.Bottom-ScreenR.Top;

  // left
  if Left='' then
    DefBounds.Left:=AForm.Left
  else if Left[length(Left)]='%' then
    DefBounds.Left:=ScreenR.Left+ScreenW*StrToIntDef(copy(Left,1,length(Left)-1),0) div 100
  else
    DefBounds.Left:=ScreenR.Left+StrToIntDef(Left,0);
  // top
  if Top='' then
    DefBounds.Top:=AForm.Top
  else if Top[length(Top)]='%' then
    DefBounds.Top:=ScreenR.Top+ScreenH*StrToIntDef(copy(Top,1,length(Top)-1),0) div 100
  else
    DefBounds.Top:=ScreenR.Top+StrToIntDef(Top,0);
  // right
  if Right='' then
    aRight:=DefBounds.Left+AForm.Width
  else begin
    // 300 = fixed at 300,
    // +300 = Left+300
    // 30% = fixed at 30% on screen
    // +30% = Left+30% of screen
    // -300 = fixed 300 from right border of screen
    // -30% = fixed 30% from right border of screen
    if Right[length(Right)]='%' then
      aRight:=ScreenW*StrToIntDef(copy(Right,1,length(Right)-1),0) div 100
    else
      aRight:=StrToIntDef(Right,0);
    if aRight<0 then
      aRight:=ScreenR.Right+aRight // relative to right of screen
    else if (Right<>'') and (Right[1]='+') then
      inc(aRight,DefBounds.Left) // relative to Left
    else
      inc(aRight,ScreenR.Left); // relative to left of screen
  end;
  DefBounds.Right:=aRight;
  // bottom
  if Bottom='' then
    aBottom:=DefBounds.Top+AForm.Height
  else begin
    // 300 = fixed at 300,
    // +300 = Top+300
    // 30% = fixed at 30% on screen
    // +30% = Top+30% of screen
    // -300 = fixed 300 from bottom border of screen
    // -30% = fixed 30% from bottom border of screen
    if Bottom[length(Bottom)]='%' then
      aBottom:=ScreenH*StrToIntDef(copy(Bottom,1,length(Bottom)-1),0) div 100
    else
      aBottom:=StrToIntDef(Bottom,0);
    if aBottom<0 then
      aBottom:=ScreenR.Bottom+aBottom // relative to bottom of screen
    else if (Bottom<>'') and (Bottom[1]='+') then
      inc(aBottom,DefBounds.Top) // relative to Top
    else
      inc(aBottom,ScreenR.Top); // relative to top of screen
  end;
  DefBounds.Bottom:=aBottom;
end;

function TIDEWindowCreator.CreateSimpleLayout: TSimpleWindowLayout;
begin
  Result:=IDEWindowCreators.SimpleLayoutStorage.ItemByFormID(FormName);
  if not Assigned(Result) then
  begin
    Result := IDEWindowCreators.SimpleLayoutStorage.CreateWindowLayout(FormName);
    InitSimpleLayout(Result);
  end;
end;

procedure TIDEWindowCreator.InitSimpleLayout(ALayout: TSimpleWindowLayout);
begin
  if FDividerTemplate <> nil then
    ALayout.Dividers.Assign(FDividerTemplate);
end;

constructor TIDEWindowCreator.Create(aFormName: string);
begin
  FFormName:=aFormName;
  FDividerTemplate := nil;
end;

constructor TIDEWindowCreator.Create(aFormName: string;
  CreateFormProc: TCreateIDEWindowProc;
  CreateFormMethod: TCreateIDEWindowMethod;
  aLeft, aTop, aRight, aBottom: string; aDockSibling: string; aDockAlign: TAlign; aMulti: boolean;
  GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent);
begin
  Create(aFormName);
  FMulti:=aMulti;
  Left:=aLeft;
  Top:=aTop;
  Right:=aRight;
  Bottom:=aBottom;
  DockSibling:=aDockSibling;
  DockAlign:=aDockAlign;
  OnCreateFormMethod:=CreateFormMethod;
  OnCreateFormProc:=CreateFormProc;
  OnGetLayout:=GetLayoutEvent;
end;

destructor TIDEWindowCreator.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDividerTemplate);
end;

function TIDEWindowCreator.NameFits(const AName: string): boolean;
begin
  Result:=CompareText(copy(AName,1,Length(FormName)),FormName)=0;
end;

{ TIDEWindowCreatorList }

function TIDEWindowCreatorList.GetItems(Index: integer
  ): TIDEWindowCreator;
begin
  Result:=TIDEWindowCreator(fItems[Index]);
end;

procedure TIDEWindowCreatorList.ErrorIfFormExists(FormName: string);
begin
  if IndexOfName(FormName)>=0 then
    raise Exception.Create('TIDEWindowDefaultLayoutList.Add: form name '+FormName+' already exists');
end;

constructor TIDEWindowCreatorList.Create;
begin
  fItems:=TFPList.Create;
  FSimpleLayoutStorage:=TSimpleWindowLayoutList.Create(True);
  FScreenMaxSizeForDefaults:=Point(1200,900);
  FOnLayoutChanged:=TMethodList.Create;
end;

destructor TIDEWindowCreatorList.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  FreeAndNil(FSimpleLayoutStorage);
  FOnLayoutChanged.Free;
  inherited Destroy;
end;

procedure TIDEWindowCreatorList.Clear;
var
  i: Integer;
begin
  for i:=0 to fItems.Count-1 do
    TObject(fItems[i]).Free;
end;

function TIDEWindowCreatorList.Count: integer;
begin
  Result:=fItems.Count;
end;

function TIDEWindowCreatorList.Add(aLayout: TIDEWindowCreator
  ): integer;
begin
  ErrorIfFormExists(aLayout.FormName);
  Result:=fItems.Add(aLayout);
end;

procedure TIDEWindowCreatorList.AddLayoutChangedHandler(
  const aEvent: TNotifyEvent);
begin
  FOnLayoutChanged.Add(TMethod(aEvent));
end;

function TIDEWindowCreatorList.Add(aFormName: string
  ): TIDEWindowCreator;
begin
  ErrorIfFormExists(aFormName);
  Result:=TIDEWindowCreator.Create(aFormName);
  Add(Result);
end;

function TIDEWindowCreatorList.Add(aFormName: string;
  CreateFormProc: TCreateIDEWindowProc;
  CreateFormMethod: TCreateIDEWindowMethod;
  aLeft, aTop, aRight, aBottom: string;
  aDockSibling: string; aDockAlign: TAlign; aMulti: boolean;
  GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent): TIDEWindowCreator;
begin
  ErrorIfFormExists(aFormName);
  Result:=TIDEWindowCreator.Create(aFormName,CreateFormProc,CreateFormMethod,
       aLeft,aTop,aRight,aBottom,aDockSibling,aDockAlign,aMulti,GetLayoutEvent);
  Add(Result);
end;

procedure TIDEWindowCreatorList.Delete(Index: integer);
begin
  TObject(fItems[Index]).Free;
  fItems.Delete(Index);
end;

function TIDEWindowCreatorList.IndexOfName(FormName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and not Items[Result].NameFits(FormName) do
    dec(Result);
end;

procedure TIDEWindowCreatorList.LayoutChanged;
var
  I: Integer;
  xEvent: TNotifyEvent;
begin
  for I := 0 to FOnLayoutChanged.Count-1 do
  begin
    xEvent := TNotifyEvent(FOnLayoutChanged[I]);
    xEvent(Self);
  end;
end;

procedure TIDEWindowCreatorList.RemoveLayoutChangedHandler(
  const aEvent: TNotifyEvent);
begin
  FOnLayoutChanged.Remove(TMethod(aEvent));
end;

function TIDEWindowCreatorList.FindWithName(FormName: string): TIDEWindowCreator;
var
  i: LongInt;
begin
  i:=IndexOfName(FormName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDEWindowCreatorList.GetForm(aFormName: string; AutoCreate: boolean;
  DisableAutoSizing: boolean): TCustomForm;
var
  Item: TIDEWindowCreator;
begin
  Result:=Screen.FindForm(aFormName);
  if Result<>nil then begin
    if DisableAutoSizing then
      Result.DisableAutoSizing;
    exit;
  end;
  if AutoCreate then begin
    Item:=FindWithName(aFormName);
    if Item=nil then begin
      debugln(['TIDEWindowCreatorList.GetForm no creator for ',aFormName]);
      exit;
    end;
    if Assigned(Item.OnCreateFormProc) then begin
      Item.OnCreateFormProc(Self,aFormName,Result,DisableAutoSizing);
    end else if Assigned(Item.OnCreateFormMethod) then begin
      Item.OnCreateFormMethod(Self,aFormName,Result,DisableAutoSizing);
    end else begin
      debugln(['TIDEWindowCreatorList.GetForm no OnCreateForm for ',aFormName]);
      exit;
    end;
    if Result=nil then begin
      debugln(['TIDEWindowCreatorList.GetForm create failed for ',aFormName]);
      exit;
    end;
    SimpleLayoutStorage.SetDefaultPosition(Result);
  end;
end;

procedure TIDEWindowCreatorList.ShowForm(AForm: TCustomForm; BringToFront: boolean;
  AMoveToVisbleMode: TLayoutMoveToVisbleMode);
var
  Layout: TSimpleWindowLayout;
begin
  if (AForm.Name='') or (not IsValidIdent(AForm.Name)) then
    raise Exception.Create('TIDEWindowCreatorList.ShowForm invalid form name '+AForm.Name);

  // auto create a layout storage for every shown form
  Layout:=SimpleLayoutStorage.ItemByFormID(AForm.Name);
  if Layout=nil then begin
    if not (csDesigning in AForm.ComponentState) then
      SimpleLayoutStorage.CreateWindowLayout(AForm);
  end
  else
    Layout.Form:=AForm;

  if (IDEDockMaster<>nil) and (not (csDesigning in AForm.ComponentState))
  and (FindWithName(AForm.Name)<>nil) then
    // show dockable if it has a creator and is not a designer form
    IDEDockMaster.ShowForm(AForm,BringToFront)
  else
    if (IDETabMaster <> nil) and (csDesigning in AForm.ComponentState) then
      IDETabMaster.ShowForm(AForm)
    else
      SimpleLayoutStorage.ApplyAndShow(Self,AForm,BringToFront,AMoveToVisbleMode);
end;

function TIDEWindowCreatorList.ShowForm(AFormName: string; BringToFront: boolean): TCustomForm;
begin
  Result:=GetForm(AFormName,true,false);
  if Result<>nil then
    ShowForm(Result,BringToFront);
end;

procedure TIDEWindowCreatorList.CreateForm(var AForm;
  AFormClass: TCustomFormClass; DoDisableAutoSizing: boolean;
  TheOwner: TComponent);
begin
  if TCustomForm(AForm)=nil then begin
    TCustomForm(AForm):=TCustomForm(AFormClass.NewInstance);
    TCustomForm(AForm).DisableAutoSizing;
    TCustomForm(AForm).Create(TheOwner);
    if not DoDisableAutoSizing then
      TCustomForm(AForm).EnableAutoSizing;
    SimpleLayoutStorage.SetDefaultPosition(TCustomForm(AForm));
  end else if DoDisableAutoSizing then
    TCustomForm(AForm).DisableAutoSizing;
end;

procedure TIDEWindowCreatorList.RestoreSimpleLayout;
var
  i: Integer;
  ALayout: TSimpleWindowLayout;
  AForm: TCustomForm;
begin
  if IDEDockMaster=nil then
  begin
    for i:=SimpleLayoutStorage.Count-1 downto 0 do//loop down in order to keep z-order of the forms
    begin
      ALayout:=SimpleLayoutStorage[i];
      AForm:=GetForm(ALayout.FormID,ALayout.Visible);
      if AForm=nil then Continue;
      ALayout.Apply(True);
      if ALayout.Visible or (AForm=Application.MainForm) then
        ShowForm(AForm,true)
      else if AForm.Visible then
        AForm.Close;
    end;
  end;
  LayoutChanged;
end;

function TIDEWindowCreatorList.GetScreenrectForDefaults: TRect;
begin
  Result:=Screen.WorkAreaRect;
  if (Result.Right-Result.Left<10)
  or (Result.Bottom-Result.Top<10) then begin
    // screen not recognized
    Result:=Rect(0,0,Max(Screen.Width,600),Max(Screen.Height,400));
  end;
  Result.Right:=Min(Result.Right,Result.Left+IDEWindowCreators.ScreenMaxSizeForDefaults.X);
  Result.Bottom:=Min(Result.Bottom,Result.Top+IDEWindowCreators.ScreenMaxSizeForDefaults.Y);
end;

{ TIDEDockMaster }

function TIDEDockMaster.AddableInWindowMenu(AForm: TCustomForm): boolean;
begin
  Result:=true;
end;

procedure TIDEDockMaster.AdjustMainIDEWindowHeight(
  const AIDEWindow: TCustomForm; const AAdjustHeight: Boolean;
  const ANewHeight: Integer);
begin

end;

procedure TIDEDockMaster.CloseAll;
begin
  CloseAllForms;
end;

initialization
  IDEWindowCreators:=TIDEWindowCreatorList.Create;
  IDEDialogLayoutList:=TIDEDialogLayoutList.Create;
finalization
  FreeAndNil(IDEWindowCreators);
  FreeAndNil(IDEDialogLayoutList);

end.


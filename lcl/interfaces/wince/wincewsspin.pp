{
 *****************************************************************************
 *                              Win32WSSpin.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WinCEWSSpin;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Spin, Controls, StdCtrls, LCLType, commctrl,
////////////////////////////////////////////////////
  WSSpin, WSLCLClasses, Windows, WinCEInt, WinCEProc,
  WinCEWSStdCtrls, WinCEWSControls;
  
type

  { TWinCEWSCustomFloatSpinEdit }

  TWinCEWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  published
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;

    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
  end;

// Prototypes for helper routines
function GetBuddyWindow(AHandle: HWND): HWND;
function SpinWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; cdecl;
function SpinBuddyWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; cdecl;
procedure UpdateFloatSpinEditControl(const Handle: HWND;
  const AFloatSpinEdit: TCustomFloatSpinEdit);
procedure UpdateFloatSpinEditText(const ASpinHandle: HWND; const ANewValue: Double;
  const ADecimalPlaces: integer);

implementation

uses
  SysUtils;

{ TWinCEWSCustomFloatSpinEdit }

function GetBuddyWindow(AHandle: HWND): HWND;
begin
  Result := SendMessage(AHandle, UDM_GETBUDDY, 0, 0)
end;

function SpinWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; cdecl;
var
  BuddyWindow: HWND;
begin
  Result := WindowProc(Window, Msg, WParam, LParam);
  if Msg = WM_SETFOCUS then
  begin
    BuddyWindow := GetBuddyWindow(Window);
    Windows.SetFocus(BuddyWindow);
    // don't select text in edit, if user clicked on the up down and the edit
    // was already focused
    if HWND(WPARAM)<>BuddyWindow then ;
      // for LCL controls this is done in win32callback.inc
      Windows.SendMessage(BuddyWindow, EM_SETSEL, 0, -1);
  end;
end;

function SpinBuddyWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; cdecl;
var
  AWindowInfo: PWindowInfo;
begin
  Result := WindowProc(Window, Msg, WParam, LParam);
  if Msg = WM_KILLFOCUS then
  begin
    AWindowInfo := GetWindowInfo(Window);
    if AWindowInfo^.AWinControl is TCustomFloatSpinEdit then
      UpdateFloatSpinEditControl(AWindowInfo^.AWinControl.Handle,
        TCustomFloatSpinEdit(AWindowInfo^.AWinControl));
  end;
end;

procedure UpdateFloatSpinEditControl(const Handle: HWND;
  const AFloatSpinEdit: TCustomFloatSpinEdit);
var
  lWindowInfo: PWindowInfo;
begin
  lWindowInfo := GetWindowInfo(Handle);
  if lWindowInfo <> @DefaultWindowInfo then
  begin
    lWindowInfo^.spinValue := AFloatSpinEdit.Value;
    UpdateFloatSpinEditText(Handle, AFloatSpinEdit.Value, AFloatSpinEdit.DecimalPlaces);
  end;
end;

procedure UpdateFloatSpinEditText(const ASpinHandle: HWND; const ANewValue: Double;
  const ADecimalPlaces: integer);
var
  editHandle: HWND;
  newValueText: string;
begin
  editHandle := GetBuddyWindow(ASpinHandle);
  newValueText := FloatToStrF(ANewValue, ffFixed, 20, ADecimalPlaces);
  Windows.SendMessageW(editHandle, WM_SETTEXT, 0, Windows.LPARAM(PWideChar(UTF8Decode(newValueText))));
end;
  
class function TWinCEWSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  Params.SubClassWndProc := @SpinWindowProc;
  with Params do
  begin
    Buddy := CreateWindowExW(WS_EX_CLIENTEDGE, 'EDIT',
     PWideChar(UTF8Decode(StrCaption)),
     Flags Or ES_AUTOHSCROLL,
     Left, Top, Width, Height,
     Parent, HMENU(Nil), HInstance, Nil);
    Window := CreateUpDownControl(Flags or DWORD(WS_BORDER or UDS_ALIGNRIGHT or UDS_ARROWKEYS),
      0, 0,       // pos -  ignored for buddy
      0, 0,       // size - ignored for buddy
      Parent, 0, HInstance, Buddy,
      1000, 0, 500);
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, true);
  UpdateFloatSpinEditControl(Params.Window, TCustomFloatSpinEdit(AWinControl));
  // init buddy
  Params.SubClassWndProc := @SpinBuddyWindowProc;
  WindowCreateInitBuddy(AWinControl, Params);
  Params.BuddyWindowInfo^.isChildEdit := true;
  Result := Params.Window;
end;

class procedure TWinCEWSCustomFloatSpinEdit.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle, BuddyHandle: HWND;
  R: TRect;
  WindowWidth: Integer;
begin
  // Felipe: With this code, the edit part gets invisible
  // Is the same as win32 code, need to check why

{  WinHandle := AWinControl.Handle;
  BuddyHandle := Windows.SendMessage(WinHandle, UDM_GETBUDDY, 0, 0);

  GetWindowRect(WinHandle, @R);
  WindowWidth := R.Right - R.Left;

  MoveWindow(BuddyHandle, Left, Top, Width - WindowWidth + 2, Height, True);
  MoveWindow(WinHandle, Left + Width - WindowWidth, Top, WindowWidth, Height, True);

  SuppressMove := True;}
end;

class function  TWinCEWSCustomFloatSpinEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelStart(GetBuddyWindow(ACustomEdit.Handle));
end;

class function  TWinCEWSCustomFloatSpinEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelLength(GetBuddyWindow(ACustomEdit.Handle));
end;

class function  TWinCEWSCustomFloatSpinEdit.GetText(const AWinControl: TWinControl;
  var AText: string): boolean;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    exit;
  AText := GetControlText(GetBuddyWindow(AWinControl.Handle));
end;

class function  TWinCEWSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
begin
  Result := GetWindowInfo(ACustomFloatSpinEdit.Handle)^.spinValue;
end;

class procedure TWinCEWSCustomFloatSpinEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  EditSetSelStart(GetBuddyWindow(ACustomEdit.Handle), NewStart);
end;

class procedure TWinCEWSCustomFloatSpinEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  EditSetSelLength(GetBuddyWindow(ACustomEdit.Handle), NewLength);
end;

class procedure TWinCEWSCustomFloatSpinEdit.ShowHide(const AWinControl: TWinControl);
var
  Buddy: HWND;
begin
  // call inherited
  TWinCEWSWinControl.ShowHide(AWinControl);
  Buddy := GetBuddyWindow(AWinControl.Handle);
  if AWinControl.HandleObjectShouldBeVisible then
    ShowWindow(Buddy, SW_SHOW)
  else
    ShowWindow(Buddy, SW_HIDE);
end;

class procedure TWinCEWSCustomFloatSpinEdit.UpdateControl(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
  UpdateFloatSpinEditControl(ACustomFloatSpinEdit.Handle, ACustomFloatSpinEdit);
end;

end.

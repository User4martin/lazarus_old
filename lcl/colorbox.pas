{
  TColorBox is component that displays colors in a combobox
  TColorListBox is component that displays colors in a listbox

  Copyright (C) 2005 Darius Blaszijk

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit ColorBox;

{$mode objfpc}
{$H+}

interface

uses
  LResources, SysUtils, LCLProc, LCLType, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TColorPalette = (cpDefault, cpFull);

  { TCustomColorBox }

  TCustomColorBox = class(TCustomComboBox)
  private
    FPalette: TColorPalette;
    function GetColor(Index : Integer): TColor;
    function GetSelected: TColor;
    procedure SetSelected(Value: TColor);
    procedure SetPalette(Value: TColorPalette);
  protected
    procedure SetStyle(Value: TComboBoxStyle); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetColorList;

    property Colors[Index : Integer] : TColor Read GetColor;
    property Palette: TColorPalette read FPalette write SetPalette;
    property Selected: TColor read GetSelected write SetSelected;
  end;

  { TColorBox }

  TColorBox = class(TCustomColorBox)
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth;
    property MaxLength;
    property Palette;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Selected;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
  end;

  { TCustomColorListBox }

  TCustomColorListBox = class(TCustomListBox)
  private
    FPalette: TColorPalette;
    function GetColor(Index : Integer): TColor;
    function GetSelected: TColor;
    procedure SetSelected(Value: TColor);
    procedure SetPalette(Value: TColorPalette);
  protected
    procedure SetStyle(Value: TListBoxStyle); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetColorList;

    property Colors[Index : Integer] : TColor Read GetColor;
    property Selected: TColor read GetSelected write SetSelected;
    property Palette: TColorPalette read FPalette write SetPalette;
  end;

  { TColorListBox }

  TColorListBox = class(TCustomColorListBox)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property ClickOnSelChange;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property Palette;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectionChange;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

// The following colors match the predefined Delphi Colors
// as defined in Graphics.pp
const
  ColorDefault: array[0..20] of Integer =
  ( clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clLtGray,
    clDkGray, clWhite, clCream, clNone, clDefault);

{------------------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('Additional', [TColorBox, TColorListBox]);
end;

{------------------------------------------------------------------------------
  Method:   TCustomColorBox.Create
  Params:   AOwner
  Returns:  Nothing

  Use Create to create an instance of TCustomColorBox and initialize all properties
  and variables.

 ------------------------------------------------------------------------------}
constructor TCustomColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPalette := cpDefault;

  SetColorList;

  Style := csOwnerDrawFixed;
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorBox.GetSelected
  Params:   None
  Returns:  TColor

  Use GetSelected to convert the item selected into a system color.

 ------------------------------------------------------------------------------}
function TCustomColorBox.GetSelected: TColor;
begin
  Result := 0;
  if ItemIndex >= 0 then
    Result := StringToColor(Items[ItemIndex]);
end;

{------------------------------------------------------------------------------
  Method:   TCustomColorBox.GetColor
  Params:   Index
  Returns:  Color at position Index

  Used as read procedure from Colors property.

 ------------------------------------------------------------------------------}

function TCustomColorBox.GetColor(Index : Integer): TColor;
begin
  if not IdentToColor(Items[Index], Result) then
    Result := clNone;
end;

{------------------------------------------------------------------------------
  Method:   TCustomColorBox.SetSelected
  Params:   Value
  Returns:  Nothing

  Use SetSelected to set the item in the ColorBox when appointed a color
  from code.

 ------------------------------------------------------------------------------}
procedure TCustomColorBox.SetSelected(Value: TColor);
var
  c: integer;
  selColor: TColor;
begin
  ItemIndex := -1;
  for c := 0 to Pred(Items.Count) do
  begin
    selColor := StringToColor(Items[c]);
    if selColor = Value then
      ItemIndex := c;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCustomColorBox.SetPalette
  Params:   Value
  Returns:  Nothing

  Use SetPalette to determine wether to reset the colorlist in the ColorBox
  based on the type of palette.

 ------------------------------------------------------------------------------}
procedure TCustomColorBox.SetPalette(Value: TColorPalette);
begin
  if Value <> FPalette then
  begin
    FPalette := Value;
    SetColorList;
  end;
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorBox.SetStyle
  Params:   Value
  Returns:  Nothing

  Use SetStyle to prevent the style to be changed to anything else than
  csOwnerDrawFixed.

 ------------------------------------------------------------------------------}
procedure TCustomColorBox.SetStyle(Value: TComboBoxStyle);
begin
  inherited SetStyle(csOwnerDrawFixed);
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorBox.DrawItem
  Params:   Index, Rect, State
  Returns:  Nothing

  Use DrawItem to customdraw an item in the ColorBox. A color preview is drawn
  and the item rectangle is made smaller and given to the inherited method to
  draw the corresponding text. The Brush color and Pen color where changed and
  reset to their original values.

 ------------------------------------------------------------------------------}
procedure TCustomColorBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  BrushColor: TColor;
  PenColor: TColor;
begin
  if Index<0 then
    exit;
  r.top := Rect.top + 3;
  r.bottom := Rect.bottom - 3;
  r.left := Rect.left + 3;
  r.right := r.left + 14;
  Exclude(State,odPainted);
  with Canvas do begin
    FillRect(Rect);

    BrushColor := Brush.Color;
    PenColor := Pen.Color;

    Brush.Color := StringToColor(Items[Index]);
      
    Pen.Color := clBlack;

    Rectangle(r);

    Brush.Color := BrushColor;
    Pen.Color := PenColor;
  end;
  r := Rect;
  r.left := r.left + 20;
  
  //DebugLn('TCustomColorBox.DrawItem ',dbgs(Index),' ',dbgs(r),' ',dbgs(odPainted in State),' ',dbgs(Assigned(OndrawItem)));
  inherited DrawItem(Index, r, State);
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorBox.SetColorList
  Params:   None
  Returns:  Nothing

  Use SetColorList to fill the itemlist in the ColorBox with the right color
  entries. Based on the value of the Palette property.

 ------------------------------------------------------------------------------}
procedure TCustomColorBox.SetColorList;
var
  c: Longint;
  s: AnsiString;
  m: TIdentMapEntry;
begin
  with Items do
  begin
    Clear;

    //add palettes as desired
    case Palette of
      cpFull :
        begin
          c := 0;
          while IdentEntry(c, m) do
          begin
            Add(m.Name);
            Inc(c);
          end;
        end;
      else
      begin
        for c := 0 to High(ColorDefault) do
          if ColorToIdent(ColorDefault[c], s) then Add(s);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------
  Method:   TCustomColorListBox.Create
  Params:   AOwner
  Returns:  Nothing

  Use Create to create an instance of TCustomColorListBox and initialize all properties
  and variables.

 ------------------------------------------------------------------------------}
constructor TCustomColorListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPalette := cpDefault;

  SetColorList;

  Style := lbOwnerDrawFixed;
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorListBox.GetSelected
  Params:   None
  Returns:  TColor

  Use GetSelected to convert the item selected into a system color.

 ------------------------------------------------------------------------------}
function TCustomColorListBox.GetSelected: TColor;
begin
  Result := 0;
  if ItemIndex >= 0 then
    if not IdentToColor(Items[ItemIndex], LongInt(Result)) then
      Result := 0;
end;

{------------------------------------------------------------------------------
  Method:   TCustomColorListBox.GetColor
  Params:   Index
  Returns:  Color at position Index

  Used as read procedure from Colors property.

 ------------------------------------------------------------------------------}
function TCustomColorListBox.GetColor(Index : Integer): TColor;
begin
  if not IdentToColor(Items[Index],Result) then
    Result:=clNone;
end;

{------------------------------------------------------------------------------
  Method:   TCustomColorListBox.SetSelected
  Params:   Value
  Returns:  Nothing

  Use SetSelected to set the item in the ColorListBox when appointed a color
  from code.

 ------------------------------------------------------------------------------}
procedure TCustomColorListBox.SetSelected(Value: TColor);
var
  c: integer;
  i: Longint;
begin
  ItemIndex := -1;

  for c := 0 to Pred(Items.Count) do
    if IdentToColor(Items[c], i) then
      if i = Value then
        ItemIndex := c;
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorListBox.SetPalette
  Params:   Value
  Returns:  Nothing

  Use SetPalette to determine wether to reset the colorlist in the ColorListBox
  based on the type of palette.

 ------------------------------------------------------------------------------}
procedure TCustomColorListBox.SetPalette(Value: TColorPalette);
begin
  if Value <> FPalette then
  begin
    FPalette := Value;
    SetColorList;
  end;
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorListBox.SetStyle
  Params:   Value
  Returns:  Nothing

  Use SetStyle to prevent the style to be changed to anything else than
  lbOwnerDrawFixed.

 ------------------------------------------------------------------------------}
procedure TCustomColorListBox.SetStyle(Value: TListBoxStyle);
begin
  inherited SetStyle(lbOwnerDrawFixed);
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorListBox.DrawItem
  Params:   Index, Rect, State
  Returns:  Nothing

  Use DrawItem to customdraw an item in the ColorListBox. A color preview is drawn
  and the item rectangle is made smaller and given to the inherited method to
  draw the corresponding text. The Brush color and Pen color where changed and
  reset to their original values.

 ------------------------------------------------------------------------------}
procedure TCustomColorListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  ItemColor: TColor;
  BrushColor: TColor;
  PenColor: TColor;
begin
  if Index<0 then
    exit;
  r.top := Rect.top + 3;
  r.bottom := Rect.bottom - 3;
  r.left := Rect.left + 3;
  r.right := r.left + 14;
  Exclude(State,odPainted);
  with Canvas do begin
    FillRect(Rect);

    BrushColor := Brush.Color;
    PenColor := Pen.Color;

    if IdentToColor(Items[Index], LongInt(ItemColor)) then
      Brush.Color := ItemColor;
      
    Pen.Color := clBlack;

    Rectangle(r);

    Brush.Color := BrushColor;
    Pen.Color := PenColor;
  end;
  r := Rect;
  r.left := r.left + 20;
  
  //DebugLn('TCustomColorListBox.DrawItem ',dbgs(Index),' ',dbgs(r),' ',dbgs(odPainted in State),' ',dbgs(Assigned(OndrawItem)));
  inherited DrawItem(Index, r, State);
end;
{------------------------------------------------------------------------------
  Method:   TCustomColorListBox.SetColorList
  Params:   None
  Returns:  Nothing

  Use SetColorList to fill the itemlist in the ColorListBox with the right color
  entries. Based on the value of the Palette property.

 ------------------------------------------------------------------------------}
procedure TCustomColorListBox.SetColorList;
var
  c: Longint;
  s: AnsiString;
  m: TIdentMapEntry;
begin
  with Items do
  begin
    Clear;

    //add palettes as desired
    case Palette of
      cpFull :
        begin
          c := 0;
          while IdentEntry(c, m) do
          begin
            Add(m.Name);
            Inc(c);
          end;
        end;
      else
      begin
        for c := 0 to High(ColorDefault) do
          if ColorToIdent(ColorDefault[c], s) then Add(s);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
end.

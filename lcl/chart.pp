{  $Id$  }
{
 /***************************************************************************
                               chart.pp
                               --------
                 Component Library Extended Controls

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Michael Van Canneyt
}
unit Chart;

{$MODE ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, LCLProc, Controls, ExtCtrls, Graphics, Dialogs;

type
  { TCustomBarChart }

  TBar = class(TCollectionItem)
  private
    FSName: String;
    FValue: integer;
    FColor: TColor;
  public
  end;

  TCustomBarChart = class(TPanel)
  private
    FUpdateCount: Integer;
    FBars: TCollection;
    FDepth: byte;
    function NormalizeScaleUnits(OldScale: Integer): Integer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function AddBar(const SName: string; Value: integer; AColor: TColor): TBar;
    function GetBar(SId: integer): TBar;
    function BarCount: Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    property Depth: byte read FDepth write FDepth;
  end;
  
  
  { TBarChart }
  
  TBarChart = class(TCustomBarChart)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property Depth: byte;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc',[TBarChart]);
end;

constructor TCustomBarChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBars:=TCollection.Create(TBar);
  FDepth:=5;
  SetInitialBounds(0,0,150,120);
end;

destructor TCustomBarChart.Destroy;
begin
  FBars.Destroy;
  inherited Destroy;
end;

function TCustomBarChart.AddBar(const SName: string; Value: Integer;
  AColor: TColor): TBar;
begin
  BeginUpdate;
  Try
    result:=TBar(FBars.Add);
    result.FsName:=SName;
    result.FValue:=Value;
    result.FColor:=AColor;
  finally
    EndUpdate;
  end;
end;

function TCustomBarChart.GetBar(SId: integer): TBar;
begin
  result:=TBar(FBars.FindItemID(SId));
end;

Function TCustomBarChart.NormalizeScaleUnits(OldScale: Integer): Integer;

Var
  T: Integer;

begin
  Result:=OldScale;
  if Result<2 then
    Result:=2
  else if Result<=5 then
    Result:=5
  else if Result<=10 then
    Result:=10
  else
    begin
    T:=StrToInt(IntToStr(Result)[1])+1;
    repeat
      Result:=Result div 10;
      T:=T*10;
    until Result<10;
    Result:=T;
    end;
end;

procedure TCustomBarChart.Paint;

var
  i,k,j,h,w,h1,HMax,VMax: integer;

  NScaleLines : Integer;
  ScaleUnits  : Integer;
  PixelPerUnit: Double;
  BC          : Double;
  RBC         : Integer;
  BL   : Integer;
  m,z: integer;
  ts : TBar;
  s  : string;
  rc : TRect;

  procedure ScaleLine(dk: integer; const s: string);

  begin
    Canvas.MoveTo(hmax+dk+FDepth,h1);
    Canvas.LineTo(hmax+dk+FDepth,h1+h);
    Canvas.LineTo(hmax+dk,h1+FDepth+h);
    Canvas.LineTo(hmax+dk,h1+FDepth+h+2);
    Canvas.TextOut(HMax+dk-j,m,s);
  end;

begin
  inherited Paint;
  hmax:=10;
  vmax:=0;
  for i:=0 to FBars.Count-1 do
    begin
    ts:=TBar(FBars.Items[i]);
    k:=Canvas.TextWidth(ts.FsName);
    if k>hmax then
      Hmax:=k;
    if ts.FValue>vmax then
      vmax:=ts.FValue;
    end;
  HMax:=HMax+10;
  h1:=RoundToInt(1.5*Canvas.TextHeight('W'));
  h:=Height-2*h1-Fdepth;
  w:=Width-hmax-2*FDepth;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Width:=1;
  Canvas.Pen.Style:=psSolid;
  Canvas.Brush.Color:=clYellow;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Polygon([Point(HMax,h1+FDepth),Point(HMax,h1+FDepth+h),Point(HMax+FDepth,h1+h),Point(HMax+FDepth,h1)]);
  Canvas.Brush.Color:=clWhite;
  Canvas.Polygon([Point(HMax,h1+FDepth+h),Point(HMax+w,h1+FDepth+h),Point(HMax+w+FDepth,h1+h),Point(HMax+FDepth,h1+h)]);
  Canvas.Brush.Color:=Color;
  Canvas.Rectangle(hmax+Fdepth,h1,hmax+w+FDepth,h1+h+1);
  Canvas.Pen.Width:=3;
  Canvas.MoveTo(hmax,h1+FDepth);
  Canvas.LineTo(hmax,h1+FDepth+h);
  Canvas.LineTo(hmax+w,h1+FDepth+h);
  Canvas.TextOut(0,0,Caption);
  j:=Canvas.TextWidth(IntTostr(VMax));
  if VMax=0 then
     begin
     PixelPerUnit:=1;
     NscaleLines:=1;
     end
   else
     begin
     PixelPerUnit:=double(w-j-6) / VMax;
     NScaleLines:=(w-j-6) div (2*j);
     end;
  ScaleUnits:=(Vmax div NScaleLines) +1;
  ScaleUnits:=NormalizeScaleUnits(ScaleUnits);
  if ScaleUnits=0 then
    NScaleLines:=1
  else
    NScaleLines:=VMax div ScaleUnits;
  Canvas.Pen.Color:=clGray;
  Canvas.Pen.Style:=psDot;
  Canvas.Pen.Width:=1;
  j:=j div 2;
  m:=h1+FDepth+h+2;
  if VMax=0 then
    begin
    k:=w div 2;
    ScaleLine(k,'0');
    end
  else
    Canvas.TextOut(HMax-j,m,'0');
  for k:=1 to NScaleLines do
    ScaleLine(RoundToInt(ScaleUnits*PixelPerUnit*k),IntToStr(k*ScaleUnits));
  If FBars.Count=0 then
    BC:=0
  else
    BC:=double(h) / (2*(FBars.Count+1));
  RBC:=RoundToInt(BC);
  z:=h1+FDepth+h;
  Canvas.Pen.Style:=psSolid;
  for i:=0 to FBars.Count-1 do
    begin
    ts:=TBar(FBars.Items[i]);
    z:=h1+FDepth+h-Round(2*(I+1)*BC);
    Canvas.Brush.Color:=ts.FColor;
    m:=ts.FValue;
    BL:=RoundToInt(m*PixelPerUnit);
    Canvas.Rectangle(hmax+1,z-1,hmax+BL+1,z+RBC-1);
    Canvas.Polygon([Point(hmax,z),Point(hmax+BL,z),Point(hmax+BL+FDepth,z-FDepth),Point(hmax+FDepth,z-FDepth)]);
    Canvas.Polygon([Point(hmax+BL,z),Point(hmax+BL,z+RBC-1),Point(hmax+BL+FDepth,z+RBC-1-FDepth),Point(hmax+BL+FDepth,z-FDepth)]);
    s:=IntToStr(m);
    w:=z+(RBC-FDepth) div 2;
    Canvas.MoveTo(Hmax+BL+Fdepth div 2,w);
    Canvas.LineTo(Hmax+BL+Fdepth+5,w);
    Canvas.Brush.Color:=clYellow;
    with rc do
      begin
      left:=hmax+BL+FDepth+5;
      right:=left+Canvas.TextWidth(s)+6;
      top:=Canvas.TextHeight(s) div 2+3;
      bottom:=w+top;
      top:=bottom-2*top;
      end;
    Canvas.Rectangle(rc);
    //debugln('TCustomBarChart.Paint A ',dbgs(rc),' s="',s,'"');
    Canvas.TextRect(rc,rc.Left,rc.Top,s);
    Canvas.Font.Color:=Font.Color;
    Canvas.TextRect(Rect(0,z,hmax-5,z+RBC),0,z,ts.FSName);
    end;
  Canvas.Pen.Style:=psSolid;
end;

procedure TCustomBarChart.Clear;
begin
  FBars.Clear;
end;

procedure TCustomBarChart.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomBarChart.EndUpdate;
begin
  if FUpdateCount=0 then
    raise Exception.Create('TCustomBarChart.EndUpdate');
  Dec(FUpdateCount);
  If FUpdateCount=0 then
    Repaint;
end;

function TCustomBarChart.BarCount: Integer;
begin
  Result:=FBars.Count;
end;


end.

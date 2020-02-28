unit AsmTestUnit;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLProc, Math,
  FpDbgClasses, FpDbgDisasX86, FpDbgDisasAvr;

type
  // Expose Mode so that it can be changed interactively
  TCustomDbgProcess = class(TDbgProcess)
  public
    property Mode: TFPDMode read FMode write FMode;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    chk64Bit: TCheckBox;
    DisassemblerGroup: TRadioGroup;
    Timer1: TTimer;
    txtOutput: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    txtCode: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure DisassemblerGroupClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    fProcess: TCustomDbgProcess;  // Only use the Mode property for debugging
    fDisassembler: TDbgAsmDecoder;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  LazStringUtils;

{$R asmtestunit.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
var
  idx, n: Integer;
  Line, S: String;
  Code: array[0..28] of Byte;
  CodeIdx, B: Byte;
  Value: Int64;
  e: Integer;
  p: Pointer;
begin
  n := txtCode.SelStart;
  if n < 0 then Exit;
  S := Copy(txtCode.Text, 1, n);
  idx := 0;
  for n := 1 to Length(S) do
  begin
    if S[n] = #10 then Inc(idx);
  end;
  Line := txtCode.Lines[idx];
  CodeIdx := 0;
  while (Line <> '') and (CodeIdx < 20) do
  begin
    S := GetPart([], [' ', #9], Line);
    Delete(Line, 1, 1); // strip end ' ' or #9
    if S = '' then Continue;
    B := Min(16, Length(S));
    Val('$' + S, Value, e);
    if e <> 0 then Continue;
    Move(Value, Code[CodeIdx], B div 2);
    Inc(CodeIdx, B div 2);
  end;
  if CodeIdx > 0
  then begin
    p := @Code;
    fDisassembler.Disassemble(p, S, Line);
    txtOutput.Text := S + ' '+ Line;
  end
//  else txtOutput.Text :='';
end;

procedure TForm1.DisassemblerGroupClick(Sender: TObject);
begin
  if DisassemblerGroup.ItemIndex in [0,1] then
  begin
    if not(fDisassembler is TX86AsmDecoder) then
    begin
      FreeAndNil(fDisassembler);
      // A bit of a hack...
      fDisassembler := TX86AsmDecoder.Create(TDbgProcess(fProcess));
    end;
    if DisassemblerGroup.ItemIndex = 0 then
      fProcess.Mode := dm64
    else
      fProcess.Mode := dm32;
  end
  else if DisassemblerGroup.ItemIndex = 2 then
  begin
    if not(fDisassembler is TAvrAsmDecoder) then
    begin
      FreeAndNil(fDisassembler);
      // A bit of a hack...
      fDisassembler := TAvrAsmDecoder.Create(TDbgProcess(fProcess));
    end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // fProcess only used to set 32/64 bit, ignore other functionality
  fProcess := TCustomDbgProcess.Create('', 0, 0, nil);
  fDisassembler := FpDbgDisasX86.TX86AsmDecoder.Create(TDbgProcess(fProcess));
  fProcess.Mode := dm64;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(fDisassembler) then
    FreeAndNil(fDisassembler);
  if Assigned(fProcess) then
    FreeAndNil(fProcess);
end;

end.


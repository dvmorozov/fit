unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  TAGraph, Buttons, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ColorDialog1: TColorDialog;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TAChart1: TTAChart;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Form1Close(Sender: TObject; var CloseAction: TCloseAction);
    procedure Form1Show(Sender: TObject);
    procedure TAChart1DrawReticule(Sender: TComponent; IndexSerie, Index, Xi,
      Yi: Integer; Xg, Yg: Double);
    procedure TAChart1DrawVertReticule(Sender: TComponent; IndexSerie, Index,
      Xi, Yi: Integer; Xg, Yg: Double);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button10Click(Sender: TObject);
begin
if ColorDialog1.Execute then TAChart1.Color:=ColorDialog1.Color;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
TAChart1.YGraphMin:=StrToInt(Edit1.Text);
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
TAChart1.YGraphMax:=StrToInt(Edit1.Text);
end;

procedure TForm1.Button13Click(Sender: TObject);
var
  Serie:TTASerie;
  i:Integer;
begin
Serie:=TTASerie.Create(TAChart1);
TAChart1.AddSerie(Serie);
Serie.ShowLines:=CheckBox3.Checked;
Serie.ShowPoints:=CheckBox2.Checked;
Serie.Title:='Sqr';
for i:=-5000 to 5000 do
   Serie.AddXY(i/10,Sqr(i/10)/1000,clRed);
Button13.Enabled:=False;
end;

procedure TForm1.Button14Click(Sender: TObject);
var
  Serie:TTASerie;
  i:Integer;
begin
Randomize;
Serie:=TTASerie.Create(TAChart1);
TAChart1.AddSerie(Serie);
Serie.ShowLines:=CheckBox3.Checked;
Serie.ShowPoints:=CheckBox2.Checked;
Serie.Title:='Line';
for i:=-500 to 500 do
   Serie.AddXY(i/10,i/10-5+Random(10),clBlue);
Button14.Enabled:=False;
end;

procedure TForm1.Button15Click(Sender: TObject);
var
   i:Integer;
   Serie:TComponent;
begin
while TAChart1.SeriesCount>0 do
   begin
   Serie:=TAChart1.GetSerie(0);
   Serie.Free;
   end;
Button8.Enabled:=True;
Button13.Enabled:=True;
Button14.Enabled:=True;
CheckBox1.Checked:=False;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
TAChart1.Refresh;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
   i:Integer;
   Serie:TComponent;
begin
if ColorDialog1.Execute then
   for i:=0 to TAChart1.SeriesCount-1 do
      begin
      Serie:=TAChart1.GetSerie(i);
      TTASerie(Serie).PenFit.Color:=ColorDialog1.Color;
      end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  TALine:TTALine;
begin
TALine:=TTALine.Create(TAChart1);
TAChart1.AddSerie(TALine);
TALine.LineStyle:=lsHorizontal;
TALine.Position:=StrToFloat(Edit5.Text);
TALine.Visible:=True;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
TAChart1.XGraphMin:=StrToInt(Edit1.Text);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  TALine:TTALine;
begin
TALine:=TTALine.Create(TAChart1);
TAChart1.AddSerie(TALine);
TALine.LineStyle:=lsVertical;
TALine.Position:=StrToFloat(Edit5.Text);
TALine.Visible:=True;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
TAChart1.XGraphMax:=StrToInt(Edit1.Text);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin

end;

procedure TForm1.Button8Click(Sender: TObject);
var
  Serie:TTASerie;
  i:Integer;
begin
Serie:=TTASerie.Create(TAChart1);
TAChart1.AddSerie(Serie);
Serie.ShowLines:=CheckBox3.Checked;
Serie.ShowPoints:=CheckBox2.Checked;
Serie.Title:='Sinus';
for i:=-500 to 499 do
   Serie.AddXY(i/10,Sin(i/10)*10,clBlack);
Button8.Enabled:=False;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
if ColorDialog1.Execute then TAChart1.GraphBrush.Color:=ColorDialog1.Color;
end;

procedure TForm1.CheckBox10Click(Sender: TObject);
begin
TAChart1.YAxisLabel:=Edit3.Text;
TAChart1.XAxisLabel:=Edit4.Text;
TAChart1.ShowAxisLabel:=CheckBox10.Checked;
end;

procedure TForm1.CheckBox11Click(Sender: TObject);
begin
TAChart1.ShowLegend:=CheckBox11.Checked;
end;

procedure TForm1.CheckBox12Click(Sender: TObject);
begin
TAChart1.ShowReticule:=CheckBox12.Checked;
if CheckBox12.Checked then CheckBox13.Checked:=False;
end;

procedure TForm1.CheckBox13Click(Sender: TObject);
begin
TAChart1.ShowVerticalReticule:=CheckBox13.Checked;
if CheckBox13.Checked then CheckBox12.Checked:=False;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
TAChart1.MirrorX:=CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
var
   i:Integer;
   Serie:TComponent;
begin
for i:=0 to TAChart1.SeriesCount-1 do
   begin
   Serie:=TAChart1.GetSerie(i);
   TTASerie(Serie).ShowPoints:=CheckBox2.Checked;
   end;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
var
   i:Integer;
   Serie:TComponent;
begin
for i:=0 to TAChart1.SeriesCount-1 do
   begin
   Serie:=TAChart1.GetSerie(i);
   TTASerie(Serie).ShowLines:=CheckBox3.Checked;
   end;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
TAChart1.AutoUpdateXMin:=CheckBox4.Checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
TAChart1.AutoUpdateXMax:=CheckBox5.Checked;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
TAChart1.AutoUpdateYMin:=CheckBox6.Checked;
end;

procedure TForm1.CheckBox7Click(Sender: TObject);
begin
TAChart1.AutoUpdateYMax:=CheckBox7.Checked;
end;

procedure TForm1.CheckBox8Click(Sender: TObject);
var
   Serie:TComponent;
   TASerie:TTASerie;
begin
CheckBox8.Enabled:=False;
if TAChart1.SeriesCount>0 then
   begin
   Serie:=TAChart1.GetSerie(0);
   TASerie:=TTASerie(Serie);
   //Serie.IndexMinFit:=0;
   //Serie.IndexMaxFit:=Serie.Count-1;
   TASerie.DisplayFit:=CheckBox8.Checked;
   if CheckBox8.Checked then
      begin
      while not(TASerie.FitReady) do Application.ProcessMessages;
      Label2.Caption:='A fit ='+Format('%6.2f',[TASerie.AFit]);
      Label3.Caption:='B fit ='+Format('%6.2f',[TASerie.BFit]);
      Label4.Caption:='Error fit ='+Format('%6.2f',[TASerie.ErrorFit]);
      end
   else
      begin
      Label2.Caption:='A fit =';
      Label3.Caption:='B fit =';
      Label4.Caption:='Error fit =';
      end;
   end;
CheckBox8.Enabled:=True;
end;

procedure TForm1.CheckBox9Click(Sender: TObject);
begin
TAChart1.Title:=Edit2.Text;
TAChart1.ShowTitle:=CheckBox9.Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  Serie:TComponent;
  i:Integer;
begin
for i:=0 to TAChart1.SeriesCount-1 do
   begin
   Serie:=TAChart1.GetSerie(i);
   case ComboBox1.ItemIndex of
      0:TTASerie(Serie).PointStyle:=TAGraph.psRectangle;
      1:TTASerie(Serie).PointStyle:=TAGraph.psCircle;
      2:TTASerie(Serie).PointStyle:=TAGraph.psCross;
      3:TTASerie(Serie).PointStyle:=TAGraph.psDiagCross;
      4:TTASerie(Serie).PointStyle:=TAGraph.psStar;
      end;
   end;
end;

procedure TForm1.Form1Close(Sender: TObject; var CloseAction: TCloseAction);
begin
TAChart1.Free;
CloseAction:=caFree;
end;

procedure TForm1.Form1Show(Sender: TObject);
begin
if TAChart1.MirrorX then CheckBox1.Checked:=True;
Label8.Caption:='';
Label9.Caption:='';
Label10.Caption:='';
end;

procedure TForm1.TAChart1DrawReticule(Sender: TComponent; IndexSerie, Index,
  Xi, Yi: Integer; Xg, Yg: Double);
var
   Str:string;
   Serie:TComponent;
begin
Serie:=TAChart1.GetSerie(IndexSerie);
Str:=TTASerie(Serie).Title;
if Str='Sinus' then Label8.Caption:=Str+' : ( Xi= '+IntToStr(Xi)+' / Yi= '+IntToStr(Yi)+' )'+
   ' ( Xg= '+Format('%6.2f',[Xg])+' / Yg= '+Format('%6.2f',[Yg])+' )';
if Str='Sqr' then Label9.Caption:=Str+' : ( Xi= '+IntToStr(Xi)+' / Yi= '+IntToStr(Yi)+' )'+
   ' ( Xg= '+Format('%6.2f',[Xg])+' / Yg= '+Format('%6.2f',[Yg])+' )';
if Str='Line' then Label10.Caption:=Str+' : ( Xi= '+IntToStr(Xi)+' / Yi= '+IntToStr(Yi)+' )'+
   ' ( Xg= '+Format('%6.2f',[Xg])+' / Yg= '+Format('%6.2f',[Yg])+' )';
end;

procedure TForm1.TAChart1DrawVertReticule(Sender: TComponent; IndexSerie,
  Index, Xi, Yi: Integer; Xg, Yg: Double);
var
   Str:string;
   Serie:TComponent;
begin
Serie:=TAChart1.GetSerie(IndexSerie);
Str:=TTASerie(Serie).Title;
if Str='Sinus' then Label8.Caption:=Str+' : ( Xi= '+IntToStr(Xi)+' / Yi= '+IntToStr(Yi)+' )'+
   ' ( Xg= '+Format('%6.2f',[Xg])+' / Yg= '+Format('%6.2f',[Yg])+' )';
if Str='Sqr' then Label9.Caption:=Str+' : ( Xi= '+IntToStr(Xi)+' / Yi= '+IntToStr(Yi)+' )'+
   ' ( Xg= '+Format('%6.2f',[Xg])+' / Yg= '+Format('%6.2f',[Yg])+' )';
if Str='Line' then Label10.Caption:=Str+' : ( Xi= '+IntToStr(Xi)+' / Yi= '+IntToStr(Yi)+' )'+
   ' ( Xg= '+Format('%6.2f',[Xg])+' / Yg= '+Format('%6.2f',[Yg])+' )';
end;

initialization
  {$I main.lrs}

end.


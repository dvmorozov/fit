(*!!! комментарии, кот. нужно будет удалить выделяются данным образом !!!*)
unit TAGraph;

{$IFDEF fpc}
{$MODE DELPHI}{$H+}
{$ENDIF}

{ Because the module is from another author, messages are suppressed. }
{$warnings off}
{$hints off}
{$notes off}

{ Copyright (C) 2005 by Philippe Martinole  <philippe.martinole@teleauto.org>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.
  
  Please contact the author if you'd like to use this component but the LGPL
  doesn't work with your project licensing.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.
}

{
********************************************************************************
*                              TAChart Graph Component                         *
********************************************************************************
*                                                                              *
* The component is TAChart                                                     *
* For both Delphi and Lazarus                                                  *
* The series are TASerie, they can be added to the TAChart component           *
* The properties and methods are the ones I need for the TeleAuto project.     *
* Many others can be added.                                                    *
*                                                                              *
********************************************************************************
Done :

For 1.2
- Faster SetXValue and SetYValue                                // Done 17/06/05
- Faster GetPointNextTo                                         // Done 17/06/05
- Faster GetYPointNextTo                                        // Done 17/06/05
- BeginUpdate and EndUpdate added to speed up SetXValue
  and SetYValue                                                 // Done 17/06/05
}

interface

uses
  {$IFDEF fpc}
  LResources,
  {$ELSE}
  Windows,
  {$ENDIF}
   SysUtils, Classes, Controls, Graphics, Dialogs, StdCtrls;

const
  MinDouble=-1.7e308;
  MaxDouble=1.7e308;
  MaxArray=2;
  MaxColor=15;
  Colors:array[1..MaxColor] of TColor=
     ( clRed,
       clGreen,
       clBlue,
       clBlack,
       clGray,
       clFuchsia,
       clTeal,
       clNavy,
       clMaroon,
       clLime,
       clOlive,
       clPurple,
       clSilver,
       clAqua,
       clCream
       );

type
  TPointStyle=(psRectangle,psCircle,psCross,psDiagCross,psStar,
  psVertLineBT,psVertLineTB                     // DM 29/01/08
  );
  TLineStyle=(lsVertical,lsHorizontal);  

  TADoubleArray=array [0..MaxArray,1..MaxArray] of Double;
  TADoubleArrayRow=array [0..MaxArray] of Double;
  TAIntegerArrayRow=array [0..MaxArray] of Integer;

  TTALigByte=array[0..999999] of Byte;
  PTALigByte=^TTALigByte;
  TTALigInteger=array[0..999999] of Integer;
  PTALigInteger=^TTALigInteger;
  TTALigDouble=array[0..999999] of Double;
  PTALigDouble=^TTALigDouble;

  TDrawVertReticule=procedure(Sender:TComponent;IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double) of object;
  TDrawReticule=procedure(Sender:TComponent;IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double) of object;
  TZoom=procedure(Sender:TComponent) of object;
  
  TTASerie = class(TComponent)
  private
    { Dйclarations privйes }
    Chart:TGraphicControl;
    FTitle:string;
    FStyle:TPointStyle;
    FPointBrushStyle:TBrushStyle;              // Brush style for point drawing (DM 02/10/07)
    FSeriesColor:TColor;                       // Color associated with the serie (DM 01/10/07)
    FImageSize:LongInt;                        // DM 01/10/07

// Image = coordinates in the component
// Graph = coordinates in the graph

    XGraphMin,YGraphMin:Double;                // Max Graph value of points
    XGraphMax,YGraphMax:Double;

    XImage,YImage:PTALigInteger;               // Image coordinates of points
    XGraph,YGraph:PTALigDouble;                // Graph coordinates of points

    ColorR,ColorG,ColorB:PTALigByte;           // Color of points
    XOfYGraphMin,XOfYGraphMax:Double;          // X max value of points
    NbPoints:Integer;                          // Number of points
    NbPointsMem:Integer;                       // Number of reserved points in heap
    FAFit,FBFit,FErrorFit:Double;              // Linear fitting
    FIndexMinFit,FIndexMaxFit:Integer;
    FDisplayFit:Boolean;
    FPenFit:TPen;
    FFitReady:Boolean;
    FShowPoints:Boolean;
    FShowLines:Boolean;
    FInitShowPoints:Boolean;                   // DM 13/02/08
    FInitShowLines:Boolean;
    
    UpdateInProgress:Boolean;

    procedure SetTitle(Value:string);
    procedure SetIndexMinFit(Value:Integer);
    procedure SetIndexMaxFit(Value:Integer);
    procedure SetDisplayFit(Value:Boolean);
    procedure SetPenFit(Value:TPen);
    procedure SetShowPoints(Value:Boolean);
    procedure SetShowLines(Value:Boolean);
    procedure SetStyle(Value:TPointStyle);
  protected
    { Dйclarations protйgйes }
  public
    { Dйclarations publiques }
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    procedure StyleChanged(Sender:TObject);
    procedure Clear;
    procedure Draw;
    function  Count:Integer;
    procedure AddXY(X,Y:Double;_Color:TColor);
    function  GetXValue(Index:Integer):Double;
    function  GetYValue(Index:Integer):Double;
    procedure SetXValue(Index:Integer;Value:Double);
    procedure SetYValue(Index:Integer;Value:Double);        
    function  GetXImgValue(Index:Integer):Integer;
    function  GetYImgValue(Index:Integer):Integer;
    procedure Delete(Index:Integer);
    procedure GetMin(var X,Y:Double);
    procedure GetMax(var X,Y:Double);
    function  GetXMin:Double;
    function  GetXMax:Double;
    function  GetYMin:Double;
    function  GetYMax:Double;
    procedure SetColor(Index:Integer;_Color:TColor);
    function  GetColor(Index:Integer):TColor;
    procedure LineFit;
    procedure DrawLineFit;

    property Title:string read FTitle write SetTitle;
    property PointStyle:TPointStyle read FStyle write SetStyle;
    property DisplayFit:Boolean read FDisplayFit write SetDisplayFit;
    property IndexMinFit:Integer read FIndexMinFit write SetIndexMinFit;
    property IndexMaxFit:Integer read FIndexMaxFit write SetIndexMaxFit;
    property FitReady:Boolean read FFitReady;
    property ShowPoints:Boolean read FShowPoints write SetShowPoints;
    property ShowLines:Boolean read FShowLines write SetShowLines default True;
    property InitShowPoints:Boolean read FInitShowPoints write FInitShowPoints;
    property InitShowLines:Boolean read FInitShowLines write FInitShowLines;
    property AFit:Double read FAFit;
    property BFit:Double read FBFit;
    property ErrorFit:Double read FErrorFit;

    procedure BeginUpdate;
    procedure EndUpdate;
  published
    { Dйclarations publiйes }
    property PenFit:TPen read FPenFit write SetPenFit;
    property SeriesColor:TColor read FSeriesColor write FSeriesColor;   //  DM 01/10/07
    property ImageSize:LongInt read FImageSize write FImageSize;        //  DM 01/10/07
    property PointBrushStyle:TBrushStyle read FPointBrushStyle          //  DM 02/10/07
        write FPointBrushStyle;
  end;

  TTALine = class(TComponent)
  private
    { Dйclarations privйes }
    Chart:TGraphicControl;

    FVisible:Boolean;
    FStyle:TLineStyle;

    PosImage:Integer;                     // Image coordinates of line
    PosGraph:Double;                      // Graph coordinates of line

    FPen:TPen;

    procedure SetVisible(Value:Boolean);
    procedure SetPos(Value:Double);
    procedure SetPen(Value:TPen);
    procedure SetStyle(Value:TLineStyle);
  protected
    { Dйclarations protйgйes }
  public
    { Dйclarations publiques }
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    procedure Draw;
    procedure StyleChanged(Sender:TObject);

    property  LineStyle:TLineStyle read FStyle write SetStyle;

  published
    { Dйclarations publiйes }
    property Visible:Boolean read FVisible write SetVisible;
    property Pen:TPen read FPen write SetPen;
    property Position:Double read PosGraph write SetPos;
  end;

  TTAChart = class(TGraphicControl)
  private
    { Dйclarations privйes }
    FTitle:string;
    FTitleFont:TFont;
    FShowTitle:Boolean;
    FXAxisLabel,FYAxisLabel:string;             // Axis Labels
    FShowAxisLabel:Boolean;
    SeriesList:TList;                           // List of series
    FMirrorX:Boolean;                           // From right to left ?
    FSeriecount:Integer;                        // Number of series
    YMarkWidth:Integer;                         // Depend on Y marks
    XImageMin,YImageMin:Integer;                // Image coordinates of limits
    XImageMax,YImageMax:Integer;
    FXGraphMin,FYGraphMin:Double;               // Graph coordinates of limits
    FXGraphMax,FYGraphMax:Double;
    FAutoUpdateXMin:Boolean;                    // Automatic calculation of XMin limit of graph ?
    FAutoUpdateXMax:Boolean;                    // Automatic calculation of XMax limit of graph ?
    FAutoUpdateYMin:Boolean;                    // Automatic calculation of YMin limit of graph ?
    FAutoUpdateYMax:Boolean;                    // Automatic calculation of YMax limit of graph ?
    FShowLegend:Boolean;

    FGraphBrush:TBrush;
    ax,bx,ay,by:Double;                         // Image<->Graphe conversion coefs

    Down:Boolean;
    Zoom:Boolean;
    Fixed:Boolean;
    XDown,YDown,XOld,YOld:Integer;              (* XDown - точка, где была нажата клавиша; XOld - точка, до которой мышь была протащена *)
    XMarkOld,YMarkOld:Integer;                  (* используются при рисовании штрихов *)
    ZoomRect:TRect;

    FShowReticule:Boolean;
    FShowVerticalReticule:Boolean;

    FDrawVertReticule:TDrawVertReticule;
    FDrawReticule:TDrawReticule;
    FZoom:TZoom;                                //  DM 26/11/07

    XReticule,YReticule:Integer;

    procedure SetAutoUpdateXMin(Value:Boolean);
    procedure SetAutoUpdateXMax(Value:Boolean);
    procedure SetAutoUpdateYMin(Value:Boolean);
    procedure SetAutoUpdateYMax(Value:Boolean);
    procedure SetXGraphMin(Value:Double);
    procedure SetYGraphMin(Value:Double);
    procedure SetXGraphMax(Value:Double);
    procedure SetYGraphMax(Value:Double);
    procedure SetMirrorX(Value:Boolean);
    procedure SetGraphBrush(Value:TBrush);
    procedure SetShowTitle(Value:Boolean);
    procedure SetShowLegend(Value:Boolean);
    procedure SetTitle(Value:string);
    procedure SetTitleFont(Value:TFont);
    procedure SetShowAxisLabel(Value:Boolean);    
    procedure SetXAxisLabel(Value:string);
    procedure SetYAxisLabel(Value:string);
    function  GetLegendWidth:Integer;
    procedure GetPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
    procedure GetXPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
    procedure GetYPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
    procedure DrawReticule(X,Y:Integer);
    procedure DrawVerticalReticule(X:Integer);
    procedure SetShowVerticalReticule(Value:Boolean);
    procedure SetShowReticule(Value:Boolean);    
    function GetCanvas: TCanvas;                //  DM  20/11/08
  protected
    { Dйclarations protйgйes }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoDrawVertReticule(IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double); virtual;
    procedure DoDrawReticule(IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double); virtual;
    procedure Resize; override;                 //  DM  20/11/08
    function GetWidth: LongInt;                 //  DM  21/04/09
    procedure SetWidth(Value: LongInt);
    function GetHeight: LongInt;                (* nuzhno dlya togo, choby ispol'zovat' real'nye razmery bitmapa *)
    procedure SetHeight(Value: LongInt);
  public
    { Dйclarations publiques }
    (* опубликовано для задания извне *)
    //  DM 20/11/07
    AxisColor:TColor;                           //  Axis color
    (* опубликовано для доступа к полям размера *)
    Bitmap: TBitmap;                            //  DM 20/11/07
    
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    procedure Paint; override;
    procedure Refresh;
    procedure Clean;
    procedure DrawTitle;
    procedure DrawAxis;
    procedure DrawLegend;
    procedure ZoomIn;                           //  DM 25/11/07
    procedure ZoomOut;                          //  DM 25/11/07

    procedure AddSerie(Serie:TComponent);
//    procedure DeleteSerie(Serie:TTASerie);
    procedure DeleteSerie(Serie:TComponent);
    function  GetSerie(i:Integer):TComponent;
    procedure SetAutoXMin(Auto:Boolean);
    procedure SetAutoXMax(Auto:Boolean);
    procedure SetAutoYMin(Auto:Boolean);
    procedure SetAutoYMax(Auto:Boolean);

    procedure XGraphToImage(Xin:Double;var XOut:Integer);
    procedure YGraphToImage(Yin:Double;var YOut:Integer);
    procedure GraphToImage(Xin,Yin:Double;var XOut,YOut:Integer);
    procedure XImageToGraph(XIn:Integer;var XOut:Double);
    procedure YImageToGraph(YIn:Integer;var YOut:Double);
    procedure ImageToGraph(XIn,YIn:Integer;var XOut,YOut:Double);
    procedure DisplaySeries;

    property SeriesCount:Integer read FSeriecount default 0;

    function GetNewColor:TColor;

  published
    { Dйclarations publiйes }
    procedure StyleChanged(Sender: TObject);
    property AutoUpdateXMin:Boolean read FAutoUpdateXMin write SetAutoUpdateXMin;
    property AutoUpdateXMax:Boolean read FAutoUpdateXMax write SetAutoUpdateXMax;
    property AutoUpdateYMin:Boolean read FAutoUpdateYMin write SetAutoUpdateYMin;
    property AutoUpdateYMax:Boolean read FAutoUpdateYMax write SetAutoUpdateYMax;
    property XGraphMin:Double read FXGraphMin write SetXGraphMin;
    property YGraphMin:Double read FYGraphMin write SetYGraphMin;
    property XGraphMax:Double read FXGraphMax write SetXGraphMax;
    property YGraphMax:Double read FYGraphMax write SetYGraphMax;
    property MirrorX:Boolean read FMirrorX write SetMirrorX;
    property GraphBrush:TBrush read FGraphBrush write SetGraphBrush;
    property ShowLegend:Boolean read FShowLegend write SetShowLegend;
    property ShowTitle:Boolean read FShowTitle write SetShowTitle;
    property Title:string read FTitle write SetTitle;
    property TitleFont:TFont read FTitleFont write SetTitleFont;
    property ShowAxisLabel:Boolean read FShowAxisLabel write SetShowAxisLabel;
    property XAxisLabel:string read FXAxisLabel write SetXAxisLabel;
    property YAxisLabel:string read FYAxisLabel write SetYAxisLabel;
    property ShowVerticalReticule:Boolean read FShowVerticalReticule write SetShowVerticalReticule;
    property ShowReticule:Boolean read FShowReticule write SetShowReticule;

    property OnDrawVertReticule:TDrawVertReticule read FDrawVertReticule write FDrawVertReticule;
    property OnDrawReticule:TDrawReticule read FDrawReticule write FDrawReticule;
    property OnZoom:TZoom read FZoom write FZoom;   //  DM 26/11/07

    property Canvas: TCanvas read GetCanvas;        //  DM 20/11/08
    
    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragKind;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnConstrainedResize;
    
    property Width: LongInt read GetWidth write SetWidth;       //  DM 21/04/09
    property Height: LongInt read GetHeight write SetHeight;    //  DM 21/04/09
  end;
  
const
  RightMargin:LongInt = 0;      //  DM 09/10/07
  BottomMargin:LongInt = 0;

procedure Register;

implementation

constructor TTALine.Create(AOwner:TComponent);
begin
inherited Create(AOwner);

FVisible:=False;

FPen:=TPen.Create;
FPen.OnChange:=StyleChanged;

LineStyle:=lsHorizontal;
end;

destructor TTALine.Destroy;
begin
inherited Destroy;

if Chart<>nil then (Chart as TTAChart).DeleteSerie(Self);

FPen.Free;
end;

procedure TTALine.StyleChanged(Sender:TObject);
begin
Chart.Invalidate;
end;

procedure TTALine.SetPen(Value:TPen);
begin
FPen.Assign(Value);
end;

procedure TTALine.SetStyle(Value:TLineStyle);
begin
if FStyle<>Value then
   begin
   FStyle:=Value;
   if Chart<>nil then Chart.Invalidate;
   end;
end;

procedure TTALine.SetVisible(Value:Boolean);
begin
FVisible:=Value;
Chart.Invalidate;
end;

procedure TTALine.SetPos(Value:Double);
begin
PosGraph:=Value;
Chart.Invalidate;
end;

procedure TTALine.Draw;
var
   i,j:Integer;
   Larg:Integer;
   xi1,yi1,xi2,yi2:Integer;
   xg1,yg1,xg2,yg2:Double;
   Min,Max,a,b:Double;
   Inside1,Inside2:Boolean;
   Chart1:TTAChart;
   YLeft,YRight,XBottom,XTop:Double;
   XLine,YLine:array[1..2] of Integer;
   BLeft,BRight,BBottom,BTop:Boolean;
   XLeftI,YLeftI,XRightI,YRightI,XBottomI,YBottomI,XTopI,YTopI:Integer;
   Temp:Double;
   dx,dy,dxy,qx,rx,qy,ry,u1,u2,u3,u4:Double;
   OK:Boolean;
   XMin,XMax,Ymin,Ymax,TempI:Integer;
   label Points;
begin
with Chart as TTAChart do
   begin
   XMin:=XImageMin;
   XMax:=XImageMax;
   YMin:=YImageMin;
   YMax:=YImageMax;
   end;

if XMin>XMax then
   begin
   TempI:=XMin;
   XMin:=XMax;
   XMax:=TempI;
   end;
if YMin>YMax then
   begin
   TempI:=YMin;
   YMin:=YMax;
   YMax:=TempI;
   end;

// Draw
with (Chart as TTAChart) do Canvas.Pen.Assign(FPen);

Min:=(Chart as TTAChart).XGraphMin;
Max:=(Chart as TTAChart).XGraphMax;

if LineStyle=lsHorizontal then
   begin
   (Chart as TTAChart).YGraphToImage(PosGraph,PosImage);

   (Chart as TTAChart).Canvas.MoveTo(XMin,PosImage);
   (Chart as TTAChart).Canvas.LineTo(XMax,PosImage);
   end;

if LineStyle=lsVertical then
   begin
   (Chart as TTAChart).XGraphToImage(PosGraph,PosImage);

   (Chart as TTAChart).Canvas.MoveTo(PosImage,YMin);
   (Chart as TTAChart).Canvas.LineTo(PosImage,YMax);
   end;

end;

// Unit Windows deleted for linux compatibility
// The three following function are copied from this unit

function RGB(R:Byte;G:Byte;B:Byte) : LongWord;
begin
 Result:=B Shl 16 + G Shl 8 + R;
end;

function GetRValue( C : Cardinal) : Byte;
begin
 result :=C and $0000FF;
end;

function GetGValue( C : Cardinal) : Byte;
begin
 result :=(C and $00FF00) shr 8;
end;

function GetBValue( C : Cardinal) : Byte;
begin
 result :=(C and $FF0000) Shr 16;
end;

// Fin des modifications

procedure CalculateIntervals(Mini,Maxi:Double;var Debut,Pas:Double);
var
   Etendue,EtendueTmp:Double;
   NbPas,Mult:array[1..3] of Double;

   Index:array[1..3] of Byte;
   Trouve:Boolean;
   DTmp:Double;
   BTmp:Byte;
   i,j:Integer;
begin
if Maxi>59 then
   Sleep(1);
Etendue:=Maxi-Mini;
if Etendue=0 then begin Debut:=Mini; Pas:=1; Exit; end;

Mult[1]:=1;
EtendueTmp:=Etendue;
NbPas[1]:=EtendueTmp;
if NbPas[1]>=10 then
   begin
   while NbPas[1]>10 do
      begin
      EtendueTmp:=EtendueTmp/10;
      Mult[1]:=Mult[1]/10;
      NbPas[1]:=EtendueTmp;
      end;
   end
else
   begin
   while EtendueTmp*10<=10 do
      begin
      EtendueTmp:=EtendueTmp*10;
      Mult[1]:=Mult[1]*10;
      NbPas[1]:=EtendueTmp;
      end;
   end;

Mult[2]:=1;
EtendueTmp:=Etendue;
NbPas[2]:=EtendueTmp/0.5;
if NbPas[2]>=10 then
   begin
   while NbPas[2]>10 do
      begin
      EtendueTmp:=EtendueTmp/10;
      Mult[2]:=Mult[2]/10;
      NbPas[2]:=EtendueTmp/0.5;
      end;
   end
else
   begin
   while EtendueTmp*10/0.5<=10 do
      begin
      EtendueTmp:=EtendueTmp*10;
      Mult[2]:=Mult[2]*10;
      NbPas[2]:=EtendueTmp/0.5;
      end;
   end;

Mult[3]:=1;
EtendueTmp:=Etendue;
NbPas[3]:=EtendueTmp/0.2;
if NbPas[3]>=10 then
   begin
   while NbPas[3]>10 do
      begin
      EtendueTmp:=EtendueTmp/10;
      Mult[3]:=Mult[3]/10;
      NbPas[3]:=EtendueTmp/0.2;
      end;
   end
else
   begin
   while EtendueTmp*10/0.2<=10 do
      begin
      EtendueTmp:=EtendueTmp*10;
      Mult[3]:=Mult[3]*10;
      NbPas[3]:=EtendueTmp/0.2;
      end;
   end;

for i:=1 to 3 do Index[i]:=i;

Trouve:=True;
while Trouve do
   begin
   Trouve:=False;
   for i:=1 to 2 do
      if NbPas[i]>NbPas[i+1] then
         begin
         Trouve:=True;
         DTmp:=NbPas[i];
         NbPas[i]:=NbPas[i+1];
         NbPas[i+1]:=DTmp;
         BTmp:=Index[i];
         Index[i]:=Index[i+1];
         Index[i+1]:=BTmp;
         end;
   end;

if NbPas[3]<=10 then j:=3
else if NbPas[2]<=10 then j:=2
else if NbPas[1]<=10 then j:=1
else
   begin
//   ShowMessage(lang('Erreur'));
   Exit;
   end;

if Index[j]=1 then Pas:=1;
if Index[j]=2 then Pas:=0.5;
if Index[j]=3 then Pas:=0.2;
Pas:=Pas/Mult[Index[j]];
// If 0 is in the interval, it is cool to have it as a mark !
if (Mini<0) and (Maxi>0) then
   begin
   Debut:=0;
   while Debut>Mini do Debut:=Debut-Pas;
   end
else
   begin
   // Don't work if mini is negative and > 1
//   if Abs(Mini)<1 then
      Debut:=Round((Mini-Pas)*Mult[Index[j]])/Mult[Index[j]]
//   else
//      Debut:=System.Int(Mini)-Pas; //null
   end;
end;

// Gauss Jordan resolution
function Gaussj(var a:TADoubleArray; n:Integer; var b:TADoubleArray; m:Integer):Boolean;
var
big,dum,pivinv:Double;
i,icol,irow,j,k,l,ll:Integer;
indxc,indxr,ipiv:TAIntegerArrayRow;
begin
Result:=True;
for j:=1 to n do ipiv[j]:=0;
for i:=1 to n do
   begin
   big:=0;
   for j:=1 to n do
      if ipiv[j]<>1 then
         for k:=1 to n do
            if ipiv[k]=0 then
               if abs(a[j,k])>=big then
                  begin
                  big:=abs(a[j,k]);
                  irow:=j;
                  icol:=k;
                  end
               else if ipiv[k]>1 then begin Result:=False; Exit; end;
   ipiv[icol]:=ipiv[icol]+1;
   if irow<>icol then
      begin
      for l:=1 to n do
         begin
         dum:=a[irow,l];
         a[irow,l]:=a[icol,l];
         a[icol,l]:=dum;
         end;
      for l:=1 to m do
         begin
         dum:=b[irow,l];
         b[irow,l]:=b[icol,l];
         b[icol,l]:=dum;
         end;
      end;
   indxr[i]:=irow;
   indxc[i]:=icol;
   if abs(a[icol,icol])<1e-18 then begin Result:=False; Exit; end;
   pivinv:=1/a[icol,icol];
   a[icol,icol]:=1;
   for l:=1 to n do a[icol,l]:=a[icol,l]*pivinv;
   for l:=1 to m do b[icol,l]:=b[icol,l]*pivinv;
   for ll:=1 to n do
      if ll<>icol then
         begin
         dum:=a[ll,icol];
         a[ll,icol]:=0;
         for l:=1 to n do a[ll,l]:=a[ll,l]-a[icol,l]*dum;
         for l:=1 to m do b[ll,l]:=b[ll,l]-b[icol,l]*dum;
         end;
   end;
for l:=n downto 1 do
   if indxr[l]<>indxc[l] then
      for k:=1 to n do
         begin
         dum:=a[k,indxr[l]];
         a[k,indxr[l]]:=a[k,indxc[l]];
         a[k,indxc[l]]:=dum;
         end;
Result:=True;
end;

// Linear least squares
procedure lfitLin(var x,y,sig:PTALigDouble;
                  ndata:Integer;
                  var a:TADoubleArrayRow;
                  var covar:TADoubleArray;
                  var chisq:Double);
var
k,j,i:Integer;
ym,wt,sum,sig2i:Double;
beta:TADoubleArray;
afunc:TADoubleArrayRow;
ma:integer;
begin
// Init the main matrix and second term with 0
for j:=1 to 2 do
   begin
   for k:=1 to 2 do covar[j,k]:=0;
   beta[j,1]:=0;
   end;

// Coefs calculation
for i:=1 to ndata do
   begin
   afunc[1]:=1;
   afunc[2]:=x^[i];
   ym:=y^[i];
   sig2i:=1/sqr(sig^[i]);
   for j:=1 to 2 do
      begin
      wt:=afunc[j]*sig2i ;
      for k:=1 to j do
         covar[j,k]:=covar[j,k]+wt*afunc[k];
      beta[j,1]:=beta[j,1]+ym*wt;
      end;
   end;
for j:=2 to 2 do
   for k:=1 to j-1 do covar[k,j]:=covar[j,k];

// Resolution
GaussJ(covar,2,beta,1);
for j:=1 to 2 do
   a[j]:=beta[j,1];

// Chi2 calculation
chisq:=0;
for i:=1 to ndata  do
   begin
   afunc[1]:=1;
   afunc[2]:=x^[i];
   sum:=0;
   for j:=1 to 2 do
      sum:=sum+a[j]*afunc[j];
   chisq:=chisq+sqr((y^[i]-sum)/sig^[i]);
   end;
end;

//*******************************************************************************
//*******************************************************************************
//*******************************************************************************

constructor TTASerie.Create(AOwner:TComponent);
begin
inherited Create(AOwner);

FPenFit:=TPen.Create;
FPenFit.OnChange:=StyleChanged;

PointStyle:=psCross;

NbPoints:=0;
NbPointsMem:=1000;
GetMem(XGraph,NbPointsMem*8);
GetMem(YGraph,NbPointsMem*8);
GetMem(XImage,NbPointsMem*4);
GetMem(YImage,NbPointsMem*4);
GetMem(ColorR,NbPointsMem);
GetMem(ColorG,NbPointsMem);
GetMem(ColorB,NbPointsMem);
FillChar(XGraph^,NbPointsMem*8,0);
FillChar(YGraph^,NbPointsMem*8,0);
FillChar(XImage^,NbPointsMem*4,0);
FillChar(YImage^,NbPointsMem*4,0);
FillChar(ColorR^,NbPointsMem,0);
FillChar(ColorG^,NbPointsMem,0);
FillChar(ColorB^,NbPointsMem,0);

XGraphMin:=MaxDouble;
YGraphMin:=MaxDouble;
XGraphMax:=MinDouble;
YGraphMax:=MinDouble;

DisplayFit:=False;
FFitReady:=False;
ShowPoints:=False;
ShowLines:=True;
FInitShowPoints:=ShowPoints;        //  DM 13/02/08
FInitShowLines:=ShowLines;

UpdateInProgress:=False;

FImageSize:=2;                      //  DM 01/10/07
FPointBrushStyle:=bsSolid;          //  DM 02/10/07
end;

destructor TTASerie.Destroy;
begin
inherited Destroy;

if Chart<>nil then (Chart as TTAChart).DeleteSerie(Self);

FPenFit.Free;

FreeMem(XGraph,NbPointsMem*8);
FreeMem(YGraph,NbPointsMem*8);
FreeMem(XImage,NbPointsMem*4);
FreeMem(YImage,NbPointsMem*4);
FreeMem(ColorR,NbPointsMem);
FreeMem(ColorG,NbPointsMem);
FreeMem(ColorB,NbPointsMem);
end;

procedure TTASerie.StyleChanged(Sender:TObject);
begin
Chart.Invalidate;
end;

procedure TTASerie.Clear;
begin
if NbPointsMem>1000 then
   begin
   FreeMem(XGraph,NbPointsMem*8);
   FreeMem(YGraph,NbPointsMem*8);
   FreeMem(XImage,NbPointsMem*4);
   FreeMem(YImage,NbPointsMem*4);
   FreeMem(ColorR,NbPointsMem);
   FreeMem(ColorG,NbPointsMem);
   FreeMem(ColorB,NbPointsMem);

   NbPointsMem:=1000;
   GetMem(XGraph,NbPointsMem*8);
   GetMem(YGraph,NbPointsMem*8);
   GetMem(XImage,NbPointsMem*4);
   GetMem(YImage,NbPointsMem*4);
   GetMem(ColorR,NbPointsMem);
   GetMem(ColorG,NbPointsMem);
   GetMem(ColorB,NbPointsMem);
   end;

NbPoints:=0;
NbPointsMem:=1000;

FillChar(XGraph^,NbPointsMem*8,0);
FillChar(YGraph^,NbPointsMem*8,0);
FillChar(XImage^,NbPointsMem*4,0);
FillChar(YImage^,NbPointsMem*4,0);
FillChar(ColorR^,NbPointsMem,0);
FillChar(ColorG^,NbPointsMem,0);
FillChar(ColorB^,NbPointsMem,0);

XGraphMin:=MaxDouble;
YGraphMin:=MaxDouble;
XGraphMax:=MinDouble;
YGraphMax:=MinDouble;

FFitReady:=False;

Chart.Invalidate;
end;

procedure TTASerie.SetStyle(Value:TPointStyle);
begin
if FStyle<>Value then
   begin
   FStyle:=Value;
   if Chart<>nil then Chart.Invalidate;
   end;
end;

procedure TTASerie.Draw;
var
   i,j:Integer;
   Larg:Integer;
   xi1,yi1,xi2,yi2:Integer;
   xg1,yg1,xg2,yg2:Double;
   Min,Max,a,b:Double;
   Inside1,Inside2:Boolean;
   Chart1:TTAChart;
   YLeft,YRight,XBottom,XTop:Double;
   XLine,YLine:array[1..2] of Integer;
   BLeft,BRight,BBottom,BTop:Boolean;
   XLeftI,YLeftI,XRightI,YRightI,XBottomI,YBottomI,XTopI,YTopI:Integer;
   Temp:Double;
   dx,dy,dxy,qx,rx,qy,ry,u1,u2,u3,u4:Double;
   OK:Boolean;
   XMin,XMax,Ymin,Ymax,TempI:Integer;
   SavedColor:TColor;                   //  DM 02/10/07
   (*SavedBrushStyle:TBrushStyle;*)
   StrokeX,StrokeY,StartX,StopX:LongInt;//  DM 01/02/08
   label Points;
begin
if NBPoints=0 then Exit;

with Chart as TTAChart do
   begin
   XMin:=XImageMin;
   XMax:=XImageMax;
   YMin:=YImageMin;
   YMax:=YImageMax;
   end;

if XMin>XMax then
   begin
   TempI:=XMin;
   XMin:=XMax;
   XMax:=TempI;
   end;
if YMin>YMax then
   begin
   TempI:=YMin;
   YMin:=YMax;
   YMax:=TempI;
   end;

// Calculate again all points
for i:=0 to NBPoints-1 do
   (Chart as TTAChart).GraphToImage(XGraph^[i],YGraph^[i],XImage^[i],YImage^[i]);

// Draw all points
with (Chart as TTAChart) do
   begin
   Canvas.Pen.Mode:=pmCopy;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Width:=1;
   end;

Min:=(Chart as TTAChart).XGraphMin;
Max:=(Chart as TTAChart).XGraphMax;

Larg:=FImageSize;   //5;    DM 01/10/07
//i:=0;
//while XGraph^[i]<Min do Inc(i);
//while (XGraph^[i]<Max) and (i<NbPoints-1) do
for i:=0 to NbPoints-2 do
   begin
   with (Chart as TTAChart) do
      begin
      xi1:=XImage^[i];
      yi1:=YImage^[i];
      xi2:=XImage^[i+1];
      yi2:=YImage^[i+1];
      xg1:=XGraph^[i];
      yg1:=YGraph^[i];
      xg2:=XGraph^[i+1];
      yg2:=YGraph^[i+1];

      Canvas.Pen.Color:=RGB(ColorR^[i],ColorG^[i],ColorB^[i]);

      if FShowLines then
         begin
         if (xg1>XGraphMin) and (xg2>XGraphMin) and (xg1<XGraphMax) and (xg2<XGraphMax) and
            (yg1>YGraphMin) and (yg2>YGraphMin) and (yg1<YGraphMax) and (yg2<YGraphMax) then
            begin
            Canvas.MoveTo(xi1,yi1);
            Canvas.LineTo(xi2,yi2);
            goto Points;
            end;

         if ((xg1<XGraphMin) and (xg2<XGraphMin)) or ((xg1>XGraphMax) and (xg2>XGraphMax)) or
            ((yg1<YGraphMin) and (yg2<YGraphMin)) or ((yg1>YGraphMax) and (yg2>YGraphMax)) then
            goto Points;

         if yg1>yg2 then
            begin
            Temp:=xg1; xg1:=xg2; xg2:=Temp;
            Temp:=yg1; yg1:=yg2; yg2:=Temp;
            end;

         if yg1=yg2 then
            begin
            if xg1>xg2 then
               begin
               Temp:=xg1; xg1:=xg2; xg2:=Temp;
               Temp:=yg1; yg1:=yg2; yg2:=Temp;
               end;
            if xg1<XGraphMin then xi1:=XImageMin;
            if xg2>XGraphMax then xi2:=XImageMax;
            Canvas.MoveTo(xi1,yi1);
            Canvas.LineTo(xi2,yi2);
            goto Points;
            end;

         if xg1=xg2 then
            begin
            if yg1<YGraphMin then yi1:=YImageMin;
            if yg2>YGraphMax then yi2:=YImageMax;
            Canvas.MoveTo(xi1,yi1);
            Canvas.LineTo(xi2,yi2);
            goto Points;
            end;

         dy:=yg1-yg2;
         dx:=xg1-xg2;
         dxy:=xg1*yg2-yg1*xg2;
         qx:=XGraphMin*dy;
         rx:=XGraphMax*dy;
         qy:=YGraphMin*dx;
         ry:=YGraphMax*dx;
         u1:=qx-qy+dxy;
         u2:=qx-ry+dxy;
         u3:=rx-ry+dxy;
         u4:=rx-qy+dxy;

         OK:=False;
         if u1*u2<0 then
            begin
            OK:=True;
            if xg1<XGraphMin then
               begin
               yg1:=(XGraphMin*dy+dxy)/dx;
               xg1:=XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
               end;
            if xg2<XGraphMin then
               begin
               yg2:=(XGraphMin*dy+dxy)/dx;
               xg2:=XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
               end;
            end;

         if u2*u3<0 then
            begin
            OK:=True;
            if yg2>YGraphMax then
               begin
               xg2:=(YGraphMax*dx-dxy)/dy;
               yg2:=YGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
               end;
            end;

         if u3*u4<0 then
            begin
            OK:=True;
            if xg1>XGraphMax then
               begin
               yg1:=(XGraphMax*dy+dxy)/dx;
               xg1:=XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
               end;
            if xg2>XGraphMax then
               begin
               yg2:=(XGraphMax*dy+dxy)/dx;
               xg2:=XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
               end;
            end;

         if u4*u1<0 then
            begin
            OK:=True;
            if yg1<YGraphMin then
               begin
               xg1:=(YGraphMin*dx-dxy)/dy;
               yg1:=YGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
               end;
            end;

         if OK then
            begin
            XGraphToImage(xg1,xi1);
            YGraphToImage(yg1,yi1);
            XGraphToImage(xg2,xi2);
            YGraphToImage(yg2,yi2);

            Canvas.MoveTo(xi1,yi1);
            Canvas.LineTo(xi2,yi2);
            end;

         end;

      Points:

      if FShowPoints then
      begin
         if (YImage^[i]>YMin) and (YImage^[i]<YMax) //  DM 06/02/08
            and (XImage^[i]>XMin) and (XImage^[i]<XMax) then
         begin
             SavedColor:=Canvas.Brush.Color;        //  DM 02/10/07
             Canvas.Brush.Style:=FPointBrushStyle;  //bsClear;  DM 02/10/07
             Canvas.Brush.Color:=Canvas.Pen.Color;  //  DM 02/10/07
             case PointStyle of
                psRectangle:
                   begin
                   Canvas.Rectangle(XImage^[i]-Larg,YImage^[i]-Larg,XImage^[i]+Larg+1,YImage^[i]+Larg+1);
                   end;
                psCross:
                   begin
                   Canvas.MoveTo(XImage^[i]-Larg,YImage^[i]);
                   Canvas.LineTo(XImage^[i]+Larg+1,YImage^[i]);
                   Canvas.MoveTo(XImage^[i],YImage^[i]-Larg);
                   Canvas.LineTo(XImage^[i],YImage^[i]+Larg+1);
                   end;
                psDiagCross:
                   begin
                   Canvas.MoveTo(XImage^[i]-Larg,YImage^[i]-Larg);
                   Canvas.LineTo(XImage^[i]+Larg+1,YImage^[i]+Larg+1);
                   Canvas.MoveTo(XImage^[i]-Larg,YImage^[i]+Larg+1);
                   Canvas.LineTo(XImage^[i]+Larg+1,YImage^[i]-Larg);
                   end;
                psStar:
                   begin
                   Canvas.MoveTo(XImage^[i]-Larg,YImage^[i]);
                   Canvas.LineTo(XImage^[i]+Larg+1,YImage^[i]);
                   Canvas.MoveTo(XImage^[i],YImage^[i]-Larg);
                   Canvas.LineTo(XImage^[i],YImage^[i]+Larg+1);

                   Canvas.MoveTo(XImage^[i]-Larg,YImage^[i]-Larg);
                   Canvas.LineTo(XImage^[i]+Larg+1,YImage^[i]+Larg+1);
                   Canvas.MoveTo(XImage^[i]-Larg,YImage^[i]+Larg+1);
                   Canvas.LineTo(XImage^[i]+Larg+1,YImage^[i]-Larg);
                   end;
                psCircle:
                   begin
                   Canvas.Ellipse(XImage^[i]-Larg,YImage^[i]-Larg,XImage^[i]+Larg+1,YImage^[i]+Larg+1);
                   end;
                psVertLineBT:
                   begin                                //  DM 29/01/08
                   (* Canvas.MoveTo(XImage^[i],YMin); *)
                   (* Canvas.LineTo(XImage^[i],YMax); *)
                   (* линия накладывается так, чтобы не затенять график *)
                   for StrokeY:=YMin to YMax-1 do
                        if Canvas.Pixels[XImage^[i],StrokeY]=
                            GraphBrush.Color then
                                Canvas.Pixels[XImage^[i],StrokeY]:=
                                    Canvas.Pen.Color;
                   if i mod 2 = 0 then
                   begin
                       (*   четная точка (индексирование начинается с 0) -
                            интервал находится справа *)
                       (*   в этой части программы эта точка точно не последняя *)
                       (*
                       SavedBrushStyle:=Canvas.Brush.Style;
                       Canvas.Brush.Style:=bsBDiagonal;
                            стили почему-то не работают, поэтому рисуем вручную
                       Canvas.FillRect(Rect(XImage[i],YMin,XImage[i+1],YMax));
                       Canvas.Brush.Style:=SavedBrushStyle;
                       *)
                       StartX:=XImage^[i];StopX:=XImage^[i+1]-1;
                       if StopX>XMax-1 then StopX:=XMax-1;
                       for StrokeX:=StartX to StopX do
                           for StrokeY:=YMin to YMax-1 do
                           begin
                               (* сетка накладывается так, чтобы не затенять график *)
                               if (StrokeX+StrokeY) mod 5=0 then
                                   if Canvas.Pixels[StrokeX,StrokeY]=
                                       GraphBrush.Color then
                                           Canvas.Pixels[StrokeX,StrokeY]:=
                                               Canvas.Pen.Color;
                           end;
                   end;  //  if i mod 2 = 0 then
                   end;  //  psVertLineBT
                psVertLineTB:
                   begin                                //  DM 29/01/08
                   (* линия накладывается так, чтобы не затенять график *)
                   for StrokeY:=YMin to YMax-1 do
                        if Canvas.Pixels[XImage^[i],StrokeY]=
                            GraphBrush.Color then
                                Canvas.Pixels[XImage^[i],StrokeY]:=
                                    Canvas.Pen.Color;
                   if i mod 2 = 0 then
                   begin
                       (*   четная точка (индексирование начинается с 0) -
                            интервал находится справа *)
                       (*   в этой части программы эта точка точно не последняя *)
                       StartX:=XImage^[i];StopX:=XImage^[i+1]-1;
                       if StopX>XMax-1 then StopX:=XMax-1;
                       for StrokeX:=StartX to StopX do
                           for StrokeY:=YMin to YMax-1 do
                           begin
                               (* сетка накладывается так, чтобы не затенять график *)
                               if (StrokeX-StrokeY) mod 5=0 then
                                   if Canvas.Pixels[StrokeX,StrokeY]=
                                       GraphBrush.Color then
                                           Canvas.Pixels[StrokeX,StrokeY]:=
                                               Canvas.Pen.Color;
                           end;
                   end;  //  if i mod 2 = 0 then
                   end;  //  psVertLineTB
                end;  //  case PointStyle of
             Canvas.Brush.Style:=bsClear;           //  restore original condition (DM 02/10/07)
             Canvas.Brush.Color:=SavedColor;        //  DM 02/10/07
         end
         else
         begin
             //  DM 06/02/08
             SavedColor:=Canvas.Brush.Color;        //  DM 02/10/07
             Canvas.Brush.Style:=FPointBrushStyle;  //bsClear;  DM 02/10/07
             Canvas.Brush.Color:=Canvas.Pen.Color;  //  DM 02/10/07
             case PointStyle of
                psVertLineBT: begin                 //  DM 29/01/08
                   (* Canvas.MoveTo(XImage^[i],YMin); *)
                   (* Canvas.LineTo(XImage^[i],YMax); *)
                   (* линия накладывается так, чтобы не затенять график *)
                   for StrokeY:=YMin to YMax-1 do
                        if Canvas.Pixels[XImage^[i],StrokeY]=
                            GraphBrush.Color then
                                Canvas.Pixels[XImage^[i],StrokeY]:=
                                    Canvas.Pen.Color;
                   if i mod 2 = 0 then
                   begin
                       (*   четная точка (индексирование начинается с 0) -
                            интервал находится справа *)
                       (*   в этой части программы эта точка точно не последняя *)
                       (*
                       SavedBrushStyle:=Canvas.Brush.Style;
                       Canvas.Brush.Style:=bsBDiagonal;
                            стили почему-то не работают, поэтому рисуем вручную
                       Canvas.FillRect(Rect(XImage[i],YMin,XImage[i+1],YMax));
                       Canvas.Brush.Style:=SavedBrushStyle;
                       *)
                       (* !!! комбинация условий обеспечивает, что если
                       выделенная область целиком выходит за границы окна,
                       то штрихи не рисуются !!! *)
                       StartX:=XImage^[i];StopX:=XImage^[i+1]-1;
                       if StartX<=XMin then
                       begin
                           StartX:=XMin;
                           if StopX>XMax-1 then StopX:=XMax-1;
                           for StrokeX:=StartX to StopX do
                               for StrokeY:=YMin to YMax-1 do
                               begin
                                   (* сетка накладывается так,
                                   чтобы не затенять график *)
                                   if (StrokeX+StrokeY) mod 5=0 then
                                       if Canvas.Pixels[StrokeX,StrokeY]=
                                           GraphBrush.Color then
                                               Canvas.Pixels[StrokeX,StrokeY]:=
                                                   Canvas.Pen.Color;
                               end;
                        end;
                   end; //  if i mod 2 = 0 then
                end;  //  psVertLineBT
                psVertLineTB: begin                 //  DM 08/02/08
                   (* линия накладывается так, чтобы не затенять график *)
                   for StrokeY:=YMin to YMax-1 do
                        if Canvas.Pixels[XImage^[i],StrokeY]=
                            GraphBrush.Color then
                                Canvas.Pixels[XImage^[i],StrokeY]:=
                                    Canvas.Pen.Color;
                   if i mod 2 = 0 then
                   begin
                       (*   четная точка (индексирование начинается с 0) -
                            интервал находится справа *)
                       (*   в этой части программы эта точка точно не последняя *)
                       (* !!! комбинация условий обеспечивает, что если
                       выделенная область целиком выходит за границы окна,
                       то штрихи не рисуются !!! *)
                       StartX:=XImage^[i];StopX:=XImage^[i+1]-1;
                       if StartX<=XMin then
                       begin
                           StartX:=XMin;
                           if StopX>XMax-1 then StopX:=XMax-1;
                           for StrokeX:=StartX to StopX do
                               for StrokeY:=YMin to YMax-1 do
                               begin
                                   (* сетка накладывается так,
                                   чтобы не затенять график *)
                                   if (StrokeX-StrokeY) mod 5=0 then
                                       if Canvas.Pixels[StrokeX,StrokeY]=
                                           GraphBrush.Color then
                                               Canvas.Pixels[StrokeX,StrokeY]:=
                                                   Canvas.Pen.Color;
                               end;
                        end;
                   end; //  if i mod 2 = 0 then
                end;  //  psVertLineTB
             end;  //  case PointStyle of
             Canvas.Brush.Style:=bsClear;               //  restore original condition (DM 02/10/07)
             Canvas.Brush.Color:=SavedColor;            //  DM 02/10/07
         end;  //  else
      end;  //  if FShowPoints then
      end;  //  with (Chart as TTAChart) do
   end;  //  for i:=0 to NbPoints-2 do

// Draw last point
if FShowPoints and (YImage^[NbPoints-1]>YMin) and (YImage^[NbPoints-1]<YMax)
   and (XImage^[NbPoints-1]>XMin) and (XImage^[NbPoints-1]<XMax) then
   with (Chart as TTAChart) do
      if (XGraph^[NbPoints-1]<Max) then
         begin
         Canvas.Pen.Color:=RGB(ColorR^[NbPoints-1],ColorG^[NbPoints-1],ColorB^[NbPoints-1]);
         SavedColor:=Canvas.Brush.Color;            //  DM 02/10/07
         Canvas.Brush.Style:=FPointBrushStyle;      //bsClear;  DM 02/10/07
         Canvas.Brush.Color:=Canvas.Pen.Color;      //  DM 02/10/07
         if PointStyle=psCross then
            begin
            Canvas.MoveTo(XImage^[NbPoints-1]-Larg,YImage^[NbPoints-1]);
            Canvas.LineTo(XImage^[NbPoints-1]+Larg+1,YImage^[NbPoints-1]);
            Canvas.MoveTo(XImage^[NbPoints-1],YImage^[NbPoints-1]-Larg);
            Canvas.LineTo(XImage^[NbPoints-1],YImage^[NbPoints-1]+Larg+1);
            end
         else
         if PointStyle=psCircle then
            begin
            Canvas.Ellipse(XImage^[NbPoints-1]-Larg,YImage^[NbPoints-1]-Larg,
               XImage^[NbPoints-1]+Larg+1,YImage^[NbPoints-1]+Larg+1);
            end
         // styles below was inserted by DM 02/10/07
         else
         if PointStyle=psRectangle then
            begin
            Canvas.Rectangle(
                XImage^[NbPoints-1]-Larg,YImage^[NbPoints-1]-Larg,
                XImage^[NbPoints-1]+Larg+1,YImage^[NbPoints-1]+Larg+1);
            end
         else
         if PointStyle=psDiagCross then
            begin
            Canvas.MoveTo(XImage^[NbPoints-1]-Larg,YImage^[NbPoints-1]-Larg);
            Canvas.LineTo(XImage^[NbPoints-1]+Larg+1,YImage^[NbPoints-1]+Larg+1);
            Canvas.MoveTo(XImage^[NbPoints-1]-Larg,YImage^[NbPoints-1]+Larg+1);
            Canvas.LineTo(XImage^[NbPoints-1]+Larg+1,YImage^[NbPoints-1]-Larg);
            end
         else
         if PointStyle=psStar then
            begin
            Canvas.MoveTo(XImage^[NbPoints-1]-Larg,YImage^[NbPoints-1]);
            Canvas.LineTo(XImage^[NbPoints-1]+Larg+1,YImage^[NbPoints-1]);
            Canvas.MoveTo(XImage^[NbPoints-1],YImage^[NbPoints-1]-Larg);
            Canvas.LineTo(XImage^[NbPoints-1],YImage^[NbPoints-1]+Larg+1);

            Canvas.MoveTo(XImage^[NbPoints-1]-Larg,YImage^[NbPoints-1]-Larg);
            Canvas.LineTo(XImage^[NbPoints-1]+Larg+1,YImage^[NbPoints-1]+Larg+1);
            Canvas.MoveTo(XImage^[NbPoints-1]-Larg,YImage^[NbPoints-1]+Larg+1);
            Canvas.LineTo(XImage^[NbPoints-1]+Larg+1,YImage^[NbPoints-1]-Larg);
            end
         else
         if PointStyle=psVertLineBT then
            begin                                   //  DM 29/01/08
            (* Canvas.MoveTo(XImage^[NbPoints-1],YMin); *)
            (* Canvas.LineTo(XImage^[NbPoints-1],YMax); *)
            (* линия накладывается так, чтобы не затенять график *)
            for StrokeY:=YMin to YMax-1 do
                with Chart as TTAChart do
                     if Canvas.Pixels[XImage^[NbPoints-1],StrokeY]=
                         GraphBrush.Color then
                             Canvas.Pixels[XImage^[NbPoints-1],StrokeY]:=
                                 Canvas.Pen.Color;
            end
         else
         if PointStyle=psVertLineTB then
            begin                                   //  DM 29/01/08
            (* линия накладывается так, чтобы не затенять график *)
            for StrokeY:=YMin to YMax-1 do
                with Chart as TTAChart do
                     if Canvas.Pixels[XImage^[NbPoints-1],StrokeY]=
                         GraphBrush.Color then
                             Canvas.Pixels[XImage^[NbPoints-1],StrokeY]:=
                                 Canvas.Pen.Color;
            end;
         Canvas.Brush.Style:=bsClear;               //  restore original condition (DM 02/10/07)
         Canvas.Brush.Color:=SavedColor;            //  DM 02/10/07
         end;

if FDisplayFit then
   begin
   LineFit;
   DrawLineFit;
   FFitReady:=True;
   end;
end;

function TTASerie.Count:Integer;
begin
Result:=NBPoints;
end;

procedure TTASerie.AddXY(X,Y:Double;_Color:TColor);
var
   XImageTemp,YImageTemp:PTALigInteger;
   XGraphTemp,YGraphTemp:PTALigDouble;
   ColorRTemp,ColorVTemp,ColorBTemp:PTALigByte;
begin
if NBPoints+1>NBPointsMem then
   begin
   NBPointsMem:=NBPointsMem+1000;
   GetMem(XGraphTemp,NbPointsMem*8);
   GetMem(YGraphTemp,NbPointsMem*8);
   GetMem(XImageTemp,NbPointsMem*4);
   GetMem(YImageTemp,NbPointsMem*4);
   GetMem(ColorRTemp,NbPointsMem);
   GetMem(ColorVTemp,NbPointsMem);
   GetMem(ColorBTemp,NbPointsMem);

   Move(XGraph^,XGraphTemp^,NbPoints*8);
   Move(YGraph^,YGraphTemp^,NbPoints*8);
   Move(XImage^,XImageTemp^,NbPoints*4);
   Move(YImage^,YImageTemp^,NbPoints*4);
   Move(ColorR^,ColorRTemp^,NbPoints);
   Move(ColorG^,ColorVTemp^,NbPoints);
   Move(ColorB^,ColorBTemp^,NbPoints);

   FreeMem(XGraph,NbPoints*8);
   FreeMem(YGraph,NbPoints*8);
   FreeMem(XImage,NbPoints*4);
   FreeMem(YImage,NbPoints*4);
   FreeMem(ColorR,NbPoints);
   FreeMem(ColorG,NbPoints);
   FreeMem(ColorB,NbPoints);

   XImage:=XImageTemp;
   YImage:=YImageTemp;
   XGraph:=XGraphTemp;
   YGraph:=YGraphTemp;
   ColorR:=ColorRTemp;
   ColorG:=ColorVTemp;
   ColorB:=ColorBTemp;
   end;

// Update max
if X>XGraphMax then XGraphMax:=X;
if X<XGraphMin then XGraphMin:=X;
if Y>YGraphMax then
   begin
   YGraphMax:=Y;
   XOfYGraphMax:=X;
   end;
if Y<YGraphMin then
   begin
   YGraphMin:=Y;
   XOfYGraphMin:=X;
   end;

// Add point
XGraph^[NbPoints]:=X;
YGraph^[NbPoints]:=Y;
ColorR^[NbPoints]:=GetRValue(_Color);
ColorG^[NbPoints]:=GetGValue(_Color);
ColorB^[NbPoints]:=GetBValue(_Color);
Inc(NBPoints);

FFitReady:=False;

Chart.Invalidate;
end;

function TTASerie.GetXValue(Index:Integer):Double;
begin
Result:=XGraph^[Index];
end;

function TTASerie.GetYValue(Index:Integer):Double;
begin
Result:=YGraph^[Index];
end;

procedure TTASerie.SetXValue(Index:Integer;Value:Double);
var
   i:Integer;
   Val:Double;
begin
if not(UpdateInProgress) then
   begin
   if Value<XGraphMin then XGraphMin:=Value
   else if Value>XGraphMax then XGraphMax:=Value
   else
      begin
      if XGraph^[Index]=XGraphMax then
         begin
         XGraph^[Index]:=Value;
         if Value<XGraphMax then
            begin
            XGraphMax:=MinDouble;
            for i:=0 to NbPoints-1 do
               begin
               Val:=XGraph^[i];
               if Val>XGraphMax then XGraphMax:=Val;
               end;
            end;
         end
      else if XGraph^[Index]=XGraphMin then
         begin
         XGraph^[Index]:=Value;
         if Value>XGraphMin then
            begin
            XGraphMin:=MaxDouble;
            for i:=0 to NbPoints-1 do
               begin
               Val:=XGraph^[i];
               if Val<XGraphMin then XGraphMin:=Val;
               end;
            end;
         end;
      end;
   end;

XGraph^[Index]:=Value;

{XGraph^[Index]:=Value;

XGraphMax:=MinDouble;
XGraphMin:=MaxDouble;
for i:=0 to NbPoints-1 do
   begin
   Val:=XGraph^[i];
   if Val>XGraphMax then XGraphMax:=Val;
   if Val<XGraphMin then XGraphMin:=Val;
   end;}

Chart.Invalidate;
end;

procedure TTASerie.SetYValue(Index:Integer;Value:Double);
var
   i:Integer;
   Val:Double;
begin
if not(UpdateInProgress) then
   begin
   if Value<YGraphMin then YGraphMin:=Value
   else if Value>YGraphMax then YGraphMax:=Value
   else
      begin
      if YGraph^[Index]=YGraphMax then
         begin
         YGraph^[Index]:=Value;
         if Value<YGraphMax then
            begin
            YGraphMax:=MinDouble;
            for i:=0 to NbPoints-1 do
               begin
               Val:=YGraph^[i];
               if Val>YGraphMax then YGraphMax:=Val;
               end;
            end;
         end
      else if YGraph^[Index]=YGraphMin then
         begin
         YGraph^[Index]:=Value;
         if Value>YGraphMin then
            begin
            YGraphMin:=MaxDouble;
            for i:=0 to NbPoints-1 do
               begin
               Val:=YGraph^[i];
               if Val<YGraphMin then YGraphMin:=Val;
               end;
            end;
         end;
      end;
   end;

YGraph^[Index]:=Value;

Chart.Invalidate;
end;

function TTASerie.GetXImgValue(Index:Integer):Integer;
begin
Result:=XImage^[Index];
end;

function TTASerie.GetYImgValue(Index:Integer):Integer;
begin
Result:=YImage^[Index];
end;

procedure TTASerie.Delete(Index:Integer);
var
   i:Integer;
begin
if Index<NBPoints-1 then
   begin
   for i:=Index+1 to NBPoints-1 do
      begin
      XGraph^[i-1]:=XGraph^[i];
      YGraph^[i-1]:=YGraph^[i];
      end;
   end;
Dec(NBPoints);
Chart.Invalidate;
end;

function TTASerie.GetXMin:Double;
begin
Result:=XGraphMin;
end;

function TTASerie.GetXMax:Double;
begin
Result:=XGraphMax;
end;

function TTASerie.GetYMin:Double;
begin
Result:=YGraphMin;
end;

function TTASerie.GetYMax:Double;
begin
Result:=YGraphMax;
end;

procedure TTASerie.GetMax(var X,Y:Double);
begin
X:=XOfYGraphMax;
Y:=YGraphMax;
end;

procedure TTASerie.GetMin(var X,Y:Double);
begin
X:=XOfYGraphMin;
Y:=YGraphMin;
end;

procedure TTASerie.SetColor(Index:Integer;_Color:TColor);
begin
ColorR^[Index]:=GetRValue(_Color);
ColorG^[Index]:=GetGValue(_Color);
ColorB^[Index]:=GetBValue(_Color);
end;

function TTASerie.GetColor(Index:Integer):TColor;
begin
Result:=RGB(ColorR^[Index],ColorG^[Index],ColorB^[Index]);
end;

procedure TTASerie.LineFit;
var
   x,y,Sig:PTALigDouble;
   i,NbData:Integer;
   var A:TADoubleArrayRow;
   var Covar:TADoubleArray;
   var ChiSQ:Double;
begin
if (FIndexMaxFit=0) and (FIndexMinFit=0) then
   begin
   FIndexMinFit:=0;
   FIndexMaxFit:=NbPoints-1;
   end;

NBData:=FIndexMaxFit-FIndexMinFit+1;
Getmem(x,(NBData+1)*8);
Getmem(y,(NBData+1)*8);
Getmem(Sig,(NBData+1)*8);

try

for i:=FIndexMinFit to FIndexMaxFit do
   begin
   x^[i+1]:=XGraph^[i];
   y^[i+1]:=YGraph^[i];
   Sig^[i+1]:=1;
   end;

// Linear least squares
LFitLin(x,y,Sig,NbData,A,Covar,ChiSQ);

// Coefs
FAFit:=A[2];
FBFit:=A[1];
if NbData>2 then
   FErrorFit:=Sqrt(ChiSQ/(NbData-2))
else
   FErrorFit:=0;

finally
Freemem(x,(NBData+1)*8);
Freemem(y,(NBData+1)*8);
Freemem(Sig,(NBData+1)*8);
end;
end;

procedure TTASerie.DrawLineFit;
var
   i:Integer;
   YLeft,YRight,XBottom,XTop:Double;
   XLineFit,YLineFit:array[1..2] of Integer;
begin
with (Chart as TTAChart) do
   Canvas.Pen.Assign(FPenFit);

i:=0;
with (Chart as TTAChart) do
   begin
   // Intersections
   // Y=AFit*X+BFit
   YLeft:=FAFit*XGraphMin+FBFit;
   YRight:=FAFit*XGraphMax+FBFit;
   // X=(Y-BFit)/AFit
   XBottom:=(YGraphMin-FBFit)/FAFit;
   XTop:=(YGraphMax-FBFit)/FAFit;

   if (YLeft<YGraphMax) and (YLeft>YGraphMin) then
      begin
      Inc(i);
      XLineFit[i]:=XImageMin;
      YGraphToImage(YLeft,YLineFit[i]);
      end;
   if (YRight<YGraphMax) and (YRight>YGraphMin) then
      begin
      Inc(i);
      XLineFit[i]:=XImageMax;
      YGraphToImage(YRight,YLineFit[i]);
      end;
   if (XBottom<XGraphMax) and (XBottom>XGraphMin) then
      begin
      Inc(i);
      XGraphToImage(XBottom,XLineFit[i]);
      YLineFit[i]:=YImageMin;
      end;
   if (XTop<XGraphMax) and (XTop>XGraphMin) then
      begin
      Inc(i);
      XGraphToImage(XTop,XLineFit[i]);
      YLineFit[i]:=YImageMax;
      end;
   Canvas.MoveTo(XLineFit[1],YLineFit[1]);
   Canvas.LineTo(XLineFit[2],YLineFit[2]);
   end;
end;

procedure TTASerie.SetDisplayFit(Value:Boolean);
begin
FDisplayFit:=Value;
if Chart<>nil then Chart.Invalidate;
end;

procedure TTASerie.SetTitle(Value:string);
begin
FTitle:=Value;
if Chart<>nil then Chart.Invalidate;
end;

procedure TTASerie.SetIndexMinFit(Value:Integer);
begin
FIndexMinFit:=Value;
FFitReady:=False;
if Chart<>nil then Chart.Invalidate;
end;

procedure TTASerie.SetIndexMaxFit(Value:Integer);
begin
FIndexMaxFit:=Value;
FFitReady:=False;
if Chart<>nil then Chart.Invalidate;
end;

procedure TTASerie.SetPenFit(Value:TPen);
begin
FPenFit.Assign(Value);
end;

procedure TTASerie.SetShowPoints(Value:Boolean);
begin
FShowPoints:=Value;
if Chart<>nil then Chart.Invalidate;
end;

procedure TTASerie.SetShowLines(Value:Boolean);
begin
FShowLines:=Value;
if Chart<>nil then Chart.Invalidate;
end;

procedure TTASerie.BeginUpdate;
begin
UpdateInProgress:=True;
end;

procedure TTASerie.EndUpdate;
var
   i:Integer;
   Val:Double;   
begin
UpdateInProgress:=False;

XGraphMax:=MinDouble;
XGraphMin:=MaxDouble;
for i:=0 to NbPoints-1 do
   begin
   Val:=XGraph^[i];
   if Val>XGraphMax then XGraphMax:=Val;
   if Val<XGraphMin then XGraphMin:=Val;
   end;

YGraphMax:=MinDouble;
YGraphMin:=MaxDouble;
for i:=0 to NbPoints-1 do
   begin
   Val:=YGraph^[i];
   if Val>YGraphMax then YGraphMax:=Val;
   if Val<YGraphMin then YGraphMin:=Val;
   end;

Chart.Invalidate;
end;

//*******************************************************************************
//*******************************************************************************
//*******************************************************************************

constructor TTAChart.Create(AOwner:TComponent);
begin
inherited Create(AOwner);

Bitmap:=TBitmap.Create;                     //  DM 20/11/08
Bitmap.Width:=600;                          // initialization
Bitmap.Height:=450;

Width:=600;
Height:=450;

XMarkOld:=-1;
YMarkOld:=-1;

SeriesList:=TList.Create;

YMarkWidth:=35;

FAutoUpdateXMin:=True;
FAutoUpdateXMax:=True;
FAutoUpdateYMin:=True;
FAutoUpdateYMax:=True;

// causes resetting ParentColor ParentColor
//Color:=clBtnFace;                         //  DM 20/11/07
AxisColor:=clBlack;

FXGraphMax:=0;
FXGraphMin:=0;
FYGraphMax:=0;
FYGraphMin:=0;

MirrorX:=False;
Fixed:=False;
Zoom:=False;
FShowTitle:=False;
FShowLegend:=False;
FShowReticule:=False;
FShowVerticalReticule:=False;

FGraphBrush:=TBrush.Create;
FGraphBrush.OnChange:=StyleChanged;

FTitleFont:=TFont.Create;
FTitleFont.OnChange:=StyleChanged;
end;

destructor TTAChart.Destroy;
var
   MySerie:TTASerie;
begin
while SeriesCount>0 do
   begin
   MySerie:=SeriesList[0];
   FSerieCount:=FSerieCount-1;
   MySerie.Free;
   SeriesList.Delete(0);
   end;

SeriesList.Free;
FGraphBrush.Free;

Bitmap.Free;                                //  DM  20/11/08

inherited Destroy;
end;

procedure TTAChart.StyleChanged(Sender: TObject);
begin
Invalidate;
end;

procedure TTAChart.Paint;
begin
//YImageMin:=Height-20;
YImageMin:=Height-20-BottomMargin;                          // DM 09/10/07
YImageMax:=5;

if FShowTitle then
   YImageMax:=YImageMax+5+Canvas.TextHeight(FTitle);

if FShowAxisLabel then
   begin
   YImageMax:=YImageMax+5+Canvas.TextHeight(FYAxisLabel);
   //YImageMin:=YImageMin-5-Canvas.TextHeight(FXAxisLabel);
   YImageMin:=YImageMin-5-Canvas.TextHeight(FXAxisLabel)-BottomMargin;  // DM 09/10/07
   end;

if FMirrorX then
   begin
   //XImageMin:=Width-YMarkWidth-GetLegendWidth;
   XImageMin:=Width-YMarkWidth-GetLegendWidth-RightMargin;  // DM 09/10/07
   XImageMax:=10;
   end
else
   begin
   XImageMin:=YMarkWidth;
   //XImageMax:=Width-10-GetLegendWidth;
   XImageMax:=Width-10-GetLegendWidth-RightMargin;          // DM 09/10/07
   end;

Refresh;
end;

procedure TTAChart.Clean;
begin
Canvas.Pen.Mode:=pmCopy;
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Color:=Color;
Canvas.Brush.Color:=Color;
Canvas.Brush.Style:=bsSolid;
Canvas.Rectangle(0,0,Width,Height);
end;

procedure TTAChart.DrawTitle;
begin
//Canvas.Brush.Assign(FGraphBrush);
if FShowTitle then
   begin
   Canvas.Brush.Color:=Color;
   Canvas.Font.Color:=clBlack;
   {$IFDEF fpc}
   Canvas.Font.Height:=13;
   {$ELSE}
   Canvas.Font.Height:=10;
   {$ENDIF}
   Canvas.TextOut((Width-Canvas.TextWidth(FTitle)) div 2,5,Title);
   end;
end;

procedure TTAChart.DrawAxis;
var
  LargTexte,MaxLargTexte,HautTexte:Integer;
  XTemp,YTemp,XPos:Integer;
  MyText:string;
  Marque,Debut,Pas:Double;
begin
// Find max mark width
MaxLargTexte:=0;
Debut:=FYGraphMax;
Pas:=1;
CalculateIntervals(FYGraphMin,FYGraphMax,Debut,Pas);
if FYGraphMin<>FYGraphMax then
   begin
   Marque:=Debut;
   while Marque<=FYGraphMax+Pas*10e-10 do
      begin
      if (Marque>=FYGraphMin) then
         begin
         YGraphToImage(Marque,YTemp);
         MyText:=Trim(Format('%6.4g',[Marque]));
         LargTexte:=Canvas.TextWidth(MyText);
         if LargTexte>MaxLargTexte then MaxLargTexte:=LargTexte;
         end;
      Marque:=Marque+Pas;
      end;
   end;

YMarkWidth:=35;
if MaxLargTexte+7+7>YMarkWidth then //35+7
   begin
   YMarkWidth:=MaxLargTexte+7+7;

   if FMirrorX then
      begin
      //XImageMin:=Width-YMarkWidth-GetLegendWidth;
      XImageMin:=Width-YMarkWidth-GetLegendWidth-RightMargin;   // DM 09/10/07
      XImageMax:=10;
      end
   else
      begin
      XImageMin:=YMarkWidth;
      //XImageMax:=Width-10-GetLegendWidth;
      XImageMax:=Width-10-GetLegendWidth-RightMargin;           // DM 09/10/07
      end;

   // Update coefs
   ax:=(XImageMax-XImageMin)/(FXGraphMax-FXGraphMin);
   bx:=XImageMax-ax*FXGraphMax;
   ay:=(YImageMax-YImageMin)/(FYGraphMax-FYGraphMin);
   by:=YImageMax-ay*FYGraphMax;
   end;

// Back
(* сразу делается очистка и рисуется рамка *)
//Canvas.Pen.Style:=psClear;                                    //  DM 20/11/07
Canvas.Pen.Mode:=pmCopy;                                        //  DM 20/11/07
Canvas.Pen.Color:=AxisColor;                                    //  DM 20/11/07
Canvas.Pen.Style:=psSolid;                                      //  DM 20/11/07
Canvas.Pen.Width:=1;                                            //  DM 20/11/07
(* д.б. имеено белый, чтобы линии рисовались через xor требуемым цветом *)
Canvas.Brush.Color:=clWhite;                                    //  DM 20/11/07
Canvas.Brush.Assign(FGraphBrush);
Canvas.Rectangle(XImageMin,YImageMin,XImageMax,YImageMax);

// Axes
//Canvas.Pen.Style:=psSolid;                                    //  DM 20/11/07
//Canvas.Pen.Mode:=pmCopy;
//Canvas.Pen.Color:=AxisColor;
//Canvas.Pen.Style:=psSolid;
//Canvas.Pen.Width:=2;
//Canvas.MoveTo(XImageMin,YImageMin);
//Canvas.LineTo(XImageMin,YImageMax);
//Canvas.MoveTo(XImageMin,YImageMin);
//Canvas.LineTo(XImageMax,YImageMin);
//Canvas.Pen.Width:=1;
//Canvas.MoveTo(XImageMin,YImageMax);
//Canvas.LineTo(XImageMax,YImageMax);
//Canvas.MoveTo(XImageMax,YImageMin);
//Canvas.LineTo(XImageMax,YImageMax);

// Axis Labels
if FShowAxisLabel then
   begin
   Canvas.Brush.Color:=Color;
   Canvas.Font.Color:=clBlack;
   {$IFDEF fpc}
   Canvas.Font.Height:=13;
   {$ELSE}
   Canvas.Font.Height:=10;
   {$ENDIF}
   if FMirrorX then
      begin
      if FShowTitle then
         Canvas.TextOut(Width-Canvas.TextWidth(FYAxisLabel)-5,25,FYAxisLabel)
      else
         Canvas.TextOut(Width-Canvas.TextWidth(FYAxisLabel)-5,5,YAxisLabel);
      Canvas.TextOut(5,Height-5-Canvas.TextHeight(FXAxisLabel),FXAxisLabel);
      end
   else
      begin
      if FShowTitle then
         Canvas.TextOut(5,25,FYAxisLabel)
      else
         Canvas.TextOut(5,5,YAxisLabel);
      Canvas.TextOut(Width-Canvas.TextWidth(FXAxisLabel)-5,
         Height-5-Canvas.TextHeight(FXAxisLabel),FXAxisLabel);
      end;
   end;

// X graduations
Debut:=FXGraphMax;
Pas:=1;
CalculateIntervals(FXGraphMin,FXGraphMax,Debut,Pas);
if FXGraphMin<>FXGraphMax then
   begin
   Marque:=Debut;
   while Marque<=FXGraphMax+Pas*10e-10 do
      begin
      if (Marque>=FXGraphMin) then
         begin
         XGraphToImage(Marque,XTemp);
         Canvas.Pen.Width:=1;
         Canvas.Pen.Color:=clGray;
         Canvas.Pen.Style:=psDot;
         Canvas.Brush.Assign(FGraphBrush);
         if (XTemp<>XImageMax) and (XTemp<>XImageMin) then
            begin
            Canvas.MoveTo(XTemp,YImageMin);
            Canvas.LineTo(XTemp,YImageMax);
            end;
         Canvas.Pen.Color:=AxisColor;
         Canvas.Pen.Style:=psSolid;
         Canvas.Pen.Mode:=pmCopy;
         Canvas.MoveTo(XTemp,YImageMin-4);
         Canvas.LineTo(XTemp,YImageMin+4);
         {$IFDEF fpc}
         Canvas.Font.Height:=13;
         {$ELSE}
         Canvas.Font.Height:=10;
         {$ENDIF}
         Canvas.Brush.Color:=Color;
         MyText:=Trim(Format('%6.4g',[Marque]));
         LargTexte:=Canvas.TextWidth(MyText) div 2;
         XPos:=XTemp-LargTexte;
         if XPos<1 then Xpos:=1;
         if XPos+LargTexte*2>Width then Xpos:=Width-LargTexte*2-1;
         Canvas.TextOut(Xpos,YImageMin+4,MyText);
         end;
      Marque:=Marque+Pas;
      end;
   end;

// Y graduations
MaxLargTexte:=0;
Debut:=FYGraphMax;
Pas:=1;
CalculateIntervals(FYGraphMin,FYGraphMax,Debut,Pas);
if FYGraphMin<>FYGraphMax then
   begin
   Marque:=Debut;
   while Marque<=FYGraphMax+Pas*10e-10 do
      begin
      if (Marque>=FYGraphMin) then
         begin
         YGraphToImage(Marque,YTemp);
         Canvas.Pen.Width:=1;
         Canvas.Pen.Color:=clGray;
         Canvas.Pen.Style:=psDot;
         Canvas.Brush.Assign(FGraphBrush);
         if (YTemp<>YImageMax) and (YTemp<>YImageMin) then
            begin
            Canvas.MoveTo(XImageMin,YTemp);
            Canvas.LineTo(XImageMax,YTemp);
            end;
         Canvas.Pen.Color:=AxisColor;
         Canvas.Pen.Style:=psSolid;
         Canvas.Pen.Mode:=pmCopy;
         Canvas.MoveTo(XImageMin-4,YTemp);
         Canvas.LineTo(XImageMin+4,YTemp);
         {$IFDEF fpc}
         Canvas.Font.Height:=13;
         {$ELSE}
         Canvas.Font.Height:=10;
         {$ENDIF}
         Canvas.Brush.Color:=Color;
         MyText:=Trim(Format('%6.4g',[Marque]));
         LargTexte:=Canvas.TextWidth(MyText);
         if LargTexte>MaxLargTexte then MaxLargTexte:=LargTexte;
         HautTexte:=Canvas.TextHeight(MyText) div 2;
         if FMirrorX then
            Canvas.TextOut(XImageMin+6,YTemp-HautTexte,MyText)
         else
            Canvas.TextOut(XImageMin-7-LargTexte,YTemp-HautTexte,MyText);
         end;
      Marque:=Marque+Pas;
      end;
   end;

end;

procedure TTAChart.DrawLegend;
var
   w,h,x1,y1,x2,y2,i,TH:Integer;
   MySerie:TTASerie;
begin
w:=GetLegendWidth;
TH:=Canvas.TextHeight('I');
h:=5+SeriesCount*(TH+5);
x1:=Width-w-5;
y1:=(Height-h) div 2;
x2:=x1+w;
y2:=y1+h;

// Border
Canvas.Brush.Assign(FGraphBrush);
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Mode:=pmCopy;
Canvas.Pen.Color:=AxisColor;
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Width:=1;
Canvas.Rectangle(x1,y1,x2,y2);

// Lines and Series titles
for i:=0 to SeriesCount-1 do
   begin
   MySerie:=SeriesList[i];
   Canvas.TextOut(x1+20,y1+5+i*(TH+5),MySerie.Title);
   Canvas.Pen.Color:=MySerie.GetColor(0);
   Canvas.MoveTo(x1+5,y1+5+i*(TH+5)+TH div 2);
   Canvas.LineTo(x1+15,y1+5+i*(TH+5)+TH div 2);
   end;

end;

procedure TTAChart.SetAutoUpdateXMin(Value:Boolean);
begin
FAutoUpdateXMin:=Value;
end;

procedure TTAChart.SetAutoUpdateXMax(Value:Boolean);
begin
FAutoUpdateXMax:=Value;
end;

procedure TTAChart.SetAutoUpdateYMin(Value:Boolean);
begin
FAutoUpdateYMin:=Value;
end;

procedure TTAChart.SetAutoUpdateYMax(Value:Boolean);
begin
FAutoUpdateYMax:=Value;
end;

procedure TTAChart.SetXGraphMin(Value:Double);
begin
FXGraphMin:=Value;
//Invalidate;   DM 10/10/07
end;

procedure TTAChart.SetYGraphMin(Value:Double);
begin
FYGraphMin:=Value;
//Invalidate;   DM 10/10/07
end;

procedure TTAChart.SetXGraphMax(Value:Double);
begin
FXGraphMax:=Value;
//Invalidate;   DM 10/10/07
end;

procedure TTAChart.SetYGraphMax(Value:Double);
begin
FYGraphMax:=Value;
//Invalidate;   DM 10/10/07
end;

procedure TTAChart.SetMirrorX(Value:Boolean);
begin
if Value<>FMirrorX then
   begin
   if FMirrorX then
      begin
      XImageMin:=YMarkWidth;
      //XImageMax:=Width-10-GetLegendWidth;
      XImageMax:=Width-10-GetLegendWidth-RightMargin;           // DM 09/10/07
      FMirrorX:=False;
      end
   else
      begin
      //XImageMin:=Width-YMarkWidth-GetLegendWidth;
      XImageMin:=Width-YMarkWidth-GetLegendWidth-RightMargin;   // DM 09/10/07
      XImageMax:=10;
      FMirrorX:=True;
      end;
   Invalidate;
   end;
end;

procedure TTAChart.SetShowTitle(Value:Boolean);
begin
FShowTitle:=Value;
Invalidate;
end;

procedure TTAChart.SetShowLegend(Value:Boolean);
begin
FShowLegend:=Value;
Invalidate;
end;

procedure TTAChart.SetTitle(Value:string);
begin
FTitle:=Value;
Invalidate;
end;

procedure TTAChart.SetTitleFont(Value:TFont);
begin
FTitleFont.Assign(Value);
end;

procedure TTAChart.SetShowAxisLabel(Value:Boolean);
begin
FShowAxisLabel:=Value;
Invalidate;
end;

procedure TTAChart.SetXAxisLabel(Value:string);
begin
FXAxisLabel:=Value;
Invalidate;
end;

procedure TTAChart.SetYAxisLabel(Value:string);
begin
FYAxisLabel:=Value;
Invalidate;
end;

function TTAChart.GetLegendWidth:Integer;
var
   i,j,k:Integer;
   MySerie:TTASerie;
begin
if not FShowLegend then begin Result:=0; Exit; end;

j:=0;
for i:=0 to SeriesCount-1 do
   begin
   MySerie:=SeriesList[i];
   k:=Canvas.TextWidth(MySerie.Title);
   if k>j then j:=k;
   end;
Result:=j+20+10;
end;

procedure TTAChart.SetGraphBrush(Value:TBrush);
begin
FGraphBrush.Assign(Value);
end;

procedure TTAChart.AddSerie(Serie:TComponent);
begin
if FShowVerticalReticule then
   DrawVerticalReticule(XMarkOld);
if FShowReticule then
   DrawReticule(XMarkOld,YMarkOld);

Inc(FSerieCount);
SeriesList.Add(Serie);
if Serie is TTASerie then (Serie as TTASerie).Chart:=Self;
if Serie is TTALine then (Serie as TTALine).Chart:=Self;
end;

//procedure TTAChart.DeleteSerie(Serie:TTASerie);
procedure TTAChart.DeleteSerie(Serie:TComponent);
var
   i:Integer;
   MySerie:TComponent;
begin
i:=0;
while i< SeriesCount do
   begin
   MySerie:=SeriesList[i];
   if Serie=MySerie then
      begin
      SeriesList.Delete(i);
      Dec(FSerieCount);
      Invalidate;
      end
   else Inc(i);
   end;
end;

function TTAChart.GetSerie(i:Integer):TComponent;
begin
Result:=SeriesList[i];
end;

procedure TTAChart.SetAutoXMin(Auto:Boolean);
begin
FAutoUpdateXMin:=Auto;
Refresh;
end;

procedure TTAChart.SetAutoXMax(Auto:Boolean);
begin
FAutoUpdateXMax:=Auto;
Refresh;
end;

procedure TTAChart.SetAutoYMin(Auto:Boolean);
begin
FAutoUpdateYMin:=Auto;
Refresh;
end;

procedure TTAChart.SetAutoYMax(Auto:Boolean);
begin
FAutoUpdateYMax:=Auto;
Refresh;
end;

procedure TTAChart.Refresh;
var
   Tolerance,Valeur:Double;
   i:Integer;
   NBPointsMax:Integer;
   Serie:TComponent;
   XMinSeries,XMaxSeries,YMinSeries,YMaxSeries:Double;
   SerieNumber,PointNumber:Integer;
   R:TRect;                                             //  DM  20/11/08
begin
if FShowVerticalReticule then               (* гашение маркерных линий *)
   DrawVerticalReticule(XMarkOld);
if FShowReticule then
   DrawReticule(XMarkOld,YMarkOld);

// Search # of points, min and max of all series
if Zoom then
   begin
   Zoom:=False;
   Fixed:=True;
   XImageToGraph(ZoomRect.Left,FXGraphMin);     (* границы прямоугольника в пикселах *)
   XImageToGraph(ZoomRect.Right,FXGraphMax);    (* преобразуются в единицы графика *)
   YImageToGraph(ZoomRect.Bottom,FYGraphMin);
   YImageToGraph(ZoomRect.Top,FYGraphMax);
   end
else if not Fixed then
   begin
   XMinSeries:=MaxDouble;
   XMaxSeries:=MinDouble;
   YMinSeries:=MaxDouble;
   YMaxSeries:=MinDouble;
   NBPointsMax:=0;
   for i:=0 to SeriesList.Count-1 do
      begin
      Serie:=SeriesList[i];
      if Serie is TTASerie then
         with TTASerie(Serie) do
            begin
            NBPointsMax:=NBPointsMax+Count;
            if XGraphMin<XMinSeries then XMinSeries:=XGraphMin;
            if YGraphMin<YMinSeries then YMinSeries:=YGraphMin;
            if XGraphMax>XMaxSeries then XMaxSeries:=XGraphMax;
            if YGraphMax>YMaxSeries then YMaxSeries:=YGraphMax;
            end;
      if Serie is TTALine then
         with TTALine(Serie) do
            begin
            if Visible then
               begin
               NBPointsMax:=NBPointsMax+1;
               case LineStyle of
                  lsHorizontal:
                     begin
                     if Position<YMinSeries then YMinSeries:=Position;
                     if Position>YMaxSeries then YMaxSeries:=Position;
                     end;
                  lsVertical:
                     begin
                     if Position<XMinSeries then XMinSeries:=Position;
                     if Position>XMaxSeries then XMaxSeries:=Position;
                     end;
                  end;
               end;
            end;
      end;

   if XMinSeries>MaxDouble/10 then XMinSeries:=0;
   if YMinSeries>MaxDouble/10 then YMinSeries:=0;   
   if XMaxSeries<MinDouble/10 then XMaxSeries:=0;
   if YMaxSeries<MinDouble/10 then YMaxSeries:=0;

   // Image coordinates calculation
   // Update max in graph
   // If one point : +/-10% of the point coordinates
   Tolerance:=0.1;
   if NBPointsMax=1 then
      begin
      if XMinSeries<>0 then
         begin
         if XMinSeries>0 then
            begin
            if FAutoUpdateXMin then FXGraphMin:=(1-Tolerance)*XMinSeries;
            if FAutoUpdateXMax then FXGraphMax:=(1+Tolerance)*XMinSeries;
            end
         else
            begin
            if FAutoUpdateXMin then FXGraphMin:=(1+Tolerance)*XMinSeries;
            if FAutoUpdateXMax then FXGraphMax:=(1-Tolerance)*XMinSeries;
            end;
         end
      else
         begin
         if FAutoUpdateXMin then FXGraphMin:=XMinSeries-1;
         if FAutoUpdateXMax then FXGraphMax:=XMinSeries+1;
         end;
      if YMinSeries<>0 then
         begin
         if YMinSeries>0 then
            begin
            if FAutoUpdateYMin then FYGraphMin:=(1-Tolerance)*YMinSeries;
            if FAutoUpdateYMax then FYGraphMax:=(1+Tolerance)*YMinSeries;
            end
         else
            begin
            if FAutoUpdateYMin then FYGraphMin:=(1+Tolerance)*YMinSeries;
            if FAutoUpdateYMax then FYGraphMax:=(1-Tolerance)*YMinSeries;
            end;
         end
      else
         begin
         if FAutoUpdateYMin then FYGraphMin:=YMinSeries-1;
         if FAutoUpdateYMax then FYGraphMax:=YMinSeries+1;
         end;
      end
   else if NbPointsMax>1 then
   // If several points : automatic +/-10% of interval
      begin
      Valeur:=Tolerance*(XMaxSeries-XMinSeries);
      if Valeur<>0 then
         begin
         if FAutoUpdateXMin then FXGraphMin:=XMinSeries-Valeur;
         if FAutoUpdateXMax then FXGraphMax:=XMaxSeries+Valeur;
         end
      else
         begin
         if FAutoUpdateXMin then FXGraphMin:=XMinSeries-1;
         if FAutoUpdateXMax then FXGraphMax:=XMaxSeries+1;
         end;
      Valeur:=Tolerance*(YMaxSeries-YMinSeries);
      if Valeur<>0 then
         begin
         if FAutoUpdateYMin then FYGraphMin:=YMinSeries-Valeur;
         if FAutoUpdateYMax then FYGraphMax:=YMaxSeries+Valeur;
         end
      else
         begin
         if FAutoUpdateYMin then FYGraphMin:=YMinSeries-1;
         if FAutoUpdateYMax then FYGraphMax:=YMinSeries+1;
         end;
      end
   else
   // 0 Points
      begin
      FXGraphMin:=0;
      FXGraphMax:=0;
      FYGraphMin:=0;
      FYGraphMax:=0;
      end;
   end;

// Image <-> Graph coeff calculation
if FXGraphMax<>FXGraphMin then
   begin
   ax:=(XImageMax-XImageMin)/(FXGraphMax-FXGraphMin);
   bx:=XImageMax-ax*FXGraphMax;
   end
else
   begin
   ax:=1;
   bx:=0;
   end;
if FYGraphMax<>FYGraphMin then
   begin
   ay:=(YImageMax-YImageMin)/(FYGraphMax-FYGraphMin);
   by:=YImageMax-ay*FYGraphMax;
   end
else
   begin
   ay:=1;
   by:=0;
   end;

Clean;
DrawTitle;
DrawAxis;
DisplaySeries;
if FShowLegend then DrawLegend;

if FShowVerticalReticule then
   DrawVerticalReticule(XMarkOld);
if FShowReticule then
   DrawReticule(XMarkOld,YMarkOld);

R.Left:=0;R.Right:=ClientWidth;
R.Top:=0;R.Bottom:=ClientHeight;
with Self as TGraphicControl do             //  DM  20/11/08
    Canvas.CopyRect(R,Bitmap.Canvas,R);
end;

procedure TTAChart.XGraphToImage(Xin:Double;var XOut:Integer);
begin
XOut:=Round(ax*XIn+bx);
end;

procedure TTAChart.YGraphToImage(Yin:Double;var YOut:Integer);
begin
YOut:=Round(ay*YIn+by);
end;

procedure TTAChart.GraphToImage(Xin,Yin:Double;var XOut,YOut:Integer);
begin
XGraphToImage(Xin,XOut);
YGraphToImage(Yin,YOut);
end;

procedure TTAChart.XImageToGraph(XIn:Integer;var XOut:Double);
begin
XOut:=(XIn-bx)/ax;
end;

procedure TTAChart.YImageToGraph(YIn:Integer;var YOut:Double);
begin
YOut:=(YIn-by)/ay;
end;

procedure TTAChart.ImageToGraph(XIn,YIn:Integer;var XOut,YOut:Double);
begin
XImageToGraph(XIn,XOut);
YImageToGraph(YIn,YOut);
end;

procedure TTAChart.DisplaySeries;
var
   i:Integer;
   Serie:TComponent;
begin
// Update all series
for i:=0 to SeriesList.Count-1 do
   begin
   Serie:=SeriesList[i];
   if Serie is TTASerie then
      begin
      Serie:=TTASerie(SeriesList[i]);
      (Serie as TTASerie).Draw;
      end;
   if Serie is TTALine then
      begin
      Serie:=TTALine(SeriesList[i]);
      if (Serie as TTALine).Visible then (Serie as TTALine).Draw;      
      end;
   end;
end;

procedure TTAChart.SetShowVerticalReticule(Value:Boolean);
begin
{if FShowVerticalReticule then
   begin
   DrawVerticalReticule(XMarkOld);
   FShowVerticalReticule:=False;
   end;}
FShowVerticalReticule:=Value;
Invalidate;
end;

procedure TTAChart.SetShowReticule(Value:Boolean);
begin
//if Value=False then
//   DrawReticule(XMarkOld,YMarkOld);
FShowReticule:=Value;
Invalidate;
end;

procedure TTAChart.GetPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
var
   j,k,XPoint,YPoint,SerieNumber,PointNumber:Integer;
   Mini,Dist,Xg,Yg,XgOut,YgOut:Double;
   Serie:TComponent;
   TASerie:TTASerie;
   T1,T2:Double;
begin
Mini:=MaxDouble;
for SerieNumber:=0 to SeriesList.Count-1 do
   begin
   Serie:=SeriesList[SerieNumber];
   if Serie is TTASerie then
      begin
      TASerie:=TTASerie(Serie);
      if TASerie.ShowPoints or TASerie.ShowLines then   //  DM 27/11/07
         begin
            for PointNumber:=0 to TASerie.Count-1 do
               begin
               XPoint:=TASerie.GetXImgValue(PointNumber);
               YPoint:=TASerie.GetYImgValue(PointNumber);
               T1:=X-XPoint;
               T2:=Y-YPoint;
               Dist:=Sqrt(Sqr(T1)+Sqr(T2));
               if Dist<=Mini then
                  begin
                  Mini:=Dist;
                  SerieNumberOut:=SerieNumber;
                  PointNumberOut:=PointNumber;
                  XOut:=XPoint;
                  YOut:=YPoint;
                  XgOut:=TASerie.GetXValue(PointNumber);
                  YgOut:=TASerie.GetYValue(PointNumber);
                  end;
               end;
            if SerieNumberOut=SerieNumber then DoDrawReticule(SerieNumberOut,PointNumberOut,XOut,YOut,XgOut,YgOut);
         end;
      end;
   end;
end;

procedure TTAChart.GetXPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
var
   j,k,XPoint,YPoint,SerieNumber,PointNumber:Integer;
   Mini,Dist,Xg,Yg:Double;
   Serie:TComponent;
   TASerie:TTASerie;
begin
Mini:=MaxDouble;
SerieNumberOut:=-1;
for SerieNumber:=0 to SeriesList.Count-1 do
   begin
   Serie:=SeriesList[SerieNumber];
   if Serie is TTASerie then
      begin
      TASerie:=TTASerie(Serie);
      if TASerie.ShowPoints or TASerie.ShowLines then   //  DM 27/11/07
         begin
            for PointNumber:=0 to TASerie.Count-1 do
               begin
               XPoint:=TASerie.GetXImgValue(PointNumber);
               Dist:=Abs(X-XPoint);
               if Dist<=Mini then
                  begin
                  Mini:=Dist;
                  SerieNumberOut:=SerieNumber;
                  PointNumberOut:=PointNumber;
                  XOut:=XPoint;
                  YOut:=TASerie.GetYImgValue(PointNumber);
                  Xg:=TASerie.GetXValue(PointNumber);
                  Yg:=TASerie.GetYValue(PointNumber);
                  end;
               end;
            if SerieNumberOut=SerieNumber then DoDrawVertReticule(SerieNumberOut,PointNumberOut,XOut,YOut,Xg,Yg);
         end;
      end;
   end;
end;

procedure TTAChart.GetYPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
var
   j,k,XPoint,YPoint,SerieNumber,PointNumber:Integer;
   Mini,Dist,Xg,Yg:Double;
   Serie:TComponent;
   TASerie:TTASerie;
begin
Mini:=MaxDouble;
for SerieNumber:=0 to SeriesList.Count-1 do
   begin
   Serie:=SeriesList[SerieNumber];
   if Serie is TTASerie then
      begin
      TASerie:=TTASerie(Serie);
      for PointNumber:=0 to TASerie.Count-1 do
         begin
         YPoint:=TASerie.GetYImgValue(PointNumber);
         Dist:=Abs(Y-YPoint);
         if Dist<=Mini then
            begin
            Mini:=Dist;
            SerieNumberOut:=SerieNumber;
            PointNumberOut:=PointNumber;
            XOut:=XPoint;
            YOut:=YPoint;
            end;
         end;
      end;
   end;
end;

procedure TTAChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
Down:=True;
XDown:=X;
YDown:=Y;
XOld:=X;
YOld:=Y;
(* гашение маркерных линий *)
if FShowVerticalReticule then
   DrawVerticalReticule(XMarkOld);                                  // DM 22/11/07
if FShowReticule then
   DrawReticule(XMarkOld,YMarkOld);                                 // DM 22/11/07

if Assigned(OnMouseDown) then OnMouseDown(Self,Button,Shift,X,Y);   // DM 10/10/07
end;

procedure TTAChart.DrawReticule(X,Y:Integer);
begin
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Mode:=pmXor;
(* получится как-раз AxisColor, если фон белый *)
Canvas.Pen.Color:=(not AxisColor) and $00FFFFFF;                    //ClWhite; DM 20/11/07
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Width:=1;
(* проверено, что здесь координаты те же, что и в Rectangle,
однако линии рисуются правильно только при этих добавках *)
//Canvas.MoveTo(X,YImageMin);                                       // DM 20/11/07
Canvas.MoveTo(X,YImageMin-2);
//Canvas.LineTo(X,YImageMax);                                       // DM 20/11/07
Canvas.LineTo(X,YImageMax+1);
//Canvas.MoveTo(XImageMin,Y);                                       // DM 20/11/07
Canvas.MoveTo(XImageMin+1,Y);
Canvas.LineTo(XImageMax,Y);
end;

procedure TTAChart.DrawVerticalReticule(X:Integer);
begin
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Mode:=pmXor;
Canvas.Pen.Color:=(not AxisColor) and $00FFFFFF;                    //ClWhite; DM 20/11/07
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Width:=1;

Canvas.MoveTo(X,YImageMin);
Canvas.LineTo(X,YImageMax);
end;

procedure TTAChart.MouseMove(Shift: TShiftState; X, Y: Integer);
var
   i,SerieNumber,PointNumber,XMin,Xmax,YMin,YMax,Temp:Integer;
   MySerie:TTASerie;
begin
if Down then
   begin
   Canvas.Brush.Style:=bsClear;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Mode:=pmXor;
   Canvas.Pen.Color:=ClWhite;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Width:=1;

   Canvas.Rectangle(XDown,YDown,XOld,YOld); (* стирается пред. прямоуг. *)
   Canvas.Rectangle(XDown,YDown,X,Y);       (* рисуется новый прямоуг. *)
   
   XOld:=X;
   YOld:=Y;
   end
else
   begin
   XMin:=XImageMin;
   XMax:=XImageMax;
   YMin:=YImageMin;
   YMax:=YImageMax;
   if XMin>XMax then
      begin
      Temp:=XMin;
      XMin:=XMax;
      XMax:=Temp;
      end;
   if YMin>YMax then
      begin
      Temp:=YMin;
      YMin:=YMax;
      YMax:=Temp;
      end;

   for i:=0 to SeriesCount-1 do
      begin
      MySerie:=SeriesList[i];
      if FShowVerticalReticule then
         begin
         GetXPointNextTo(X,Y,SerieNumber,PointNumber,XReticule,YReticule);
         if (XReticule<>XMarkOld) and (XReticule>XMin) and (XReticule<XMax) then
            begin
            DrawVerticalReticule(XMarkOld);
            DrawVerticalReticule(XReticule);
            FShowVerticalReticule:=True;
            XMarkOld:=XReticule;
            YMarkOld:=YReticule;
            end;
         end;
      if FShowReticule then
         begin
         GetPointNextTo(X,Y,SerieNumber,PointNumber,XReticule,YReticule);
         if (XReticule<>XMarkOld) or (YReticule<>YMarkOld) then
            if (XReticule>=XMin) and (XReticule<=XMax) and (YReticule>=YMin) and (YReticule<=YMax) then
               begin
               DrawReticule(XMarkOld,YMarkOld);     (* гашение *)
               DrawReticule(XReticule,YReticule);   (* рисование заново *)
               FShowReticule:=True;
               XMarkOld:=XReticule;
               YMarkOld:=YReticule;
               end;
         end;
      end;
   end;
end;

procedure TTAChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Down then                                    (* клавиша была нажата *)
   begin
   XMarkOld:=X;
   YMarkOld:=Y;

   Canvas.Brush.Style:=bsClear;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Mode:=pmXor;
   Canvas.Pen.Color:=ClWhite;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Width:=1;

   Canvas.Rectangle(XDown,YDown,XOld,YOld);     (* стирается прямоуг. *)

   Down:=False;
   (* !!! прежний алгоритм при FSeriecount=0 вызывает искл. !!!
      нельзя вызывать Invalidate, потому что маркерные линиии
      в таком режиме не отображаются
   *)
   if FSeriecount=0 then Exit;                  //  DM 03/10/07
   (* выделяется особый случай, когда координаты совпадают,
      в остальном обработка остается прежней; д. вызывать
      Invalidate для гашения маркерных линий;
   *)
   if (XDown=XOld) and (YDown=YOld) then
   begin
        Fixed:=True;                            //  DM 01/10/07
   end
   else
   begin
       if (XDown<XOld) and (YDown<YOld) then
          begin                                 (* выделена область вправо и вниз от начальной точки *)
          Zoom:=True;                           (* только при этом условии zoom *)
          end
       else
          begin
          Zoom:=False;
          Fixed:=False;
          end;
       if XDown<XOld then
          begin
          ZoomRect.Left:=XDown;                 (* устанавливается правильный порядок координат в ZoomRect *)
          ZoomRect.Right:=XOld;
          end
       else
          begin
          ZoomRect.Left:=XOld;
          ZoomRect.Right:=XDown;
          end;
       if YDown<YOld then
          begin
          ZoomRect.Bottom:=YOld;
          ZoomRect.Top:=YDown;
          end
       else
          begin
          ZoomRect.Bottom:=YDown;
          ZoomRect.Top:=YOld;
          end;
   end;

   Invalidate;
   end;
   if Assigned(OnMouseUp) then OnMouseUp(Self,Button,Shift,X,Y);    // DM 10/10/07
end;

procedure TTAChart.DoDrawVertReticule(IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double);
begin
if Assigned(FDrawVertReticule) then FDrawVertReticule(Self,IndexSerie,Index,Xi,Yi,Xg,Yg);
end;

procedure TTAChart.DoDrawReticule(IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double);
begin
if Assigned(FDrawReticule) then FDrawReticule(Self,IndexSerie,Index,Xi,Yi,Xg,Yg);
end;

function TTAChart.GetNewColor:TColor;
var
   i,j:Integer;
   MySerie:TTASerie;
   ColorFound:Boolean;
begin
for i:=1 to MaxColor do
   begin
   ColorFound:=False;
   for j:=0 to SeriesCount-1 do
      begin
      MySerie:=SeriesList[j];
      if MySerie.GetColor(0)=Colors[i] then
         ColorFound:=True;
      end;
   if not ColorFound then
      begin
      Result:=Colors[i];
      Exit;
      end;
   end;
Randomize;
Result:=RGB(Random(255),Random(255),Random(255));
end;

//  DM 25/11/07
procedure TTAChart.ZoomIn;
var D: Double;
begin
    if SeriesCount <> 0 then
    begin   (* без такой проверки перестает отображать данные *)
        Zoom := True;
        D := XGraphMax - XGraphMin;
        D := D * 0.1;
        XGraphToImage(XGraphMin + D, ZoomRect.Left);
        XGraphToImage(XGraphMax - D, ZoomRect.Right);

        D := YGraphMax - YGraphMin;
        D := D * 0.1;
        YGraphToImage(YGraphMin + D, ZoomRect.Bottom);
        YGraphToImage(YGraphMax - D, ZoomRect.Top);

        Invalidate;
    end;
    if Assigned(OnZoom) then OnZoom(Self);
end;

//  DM 25/11/07
procedure TTAChart.ZoomOut;
var D: Double;
begin
    if SeriesCount <> 0 then
    begin   (* без такой проверки перестает отображать данные *)
        Zoom := True;
        D := XGraphMax - XGraphMin;
        D := D * 0.1;
        XGraphToImage(XGraphMin - D, ZoomRect.Left);
        XGraphToImage(XGraphMax + D, ZoomRect.Right);

        D := YGraphMax - YGraphMin;
        D := D * 0.1;
        YGraphToImage(YGraphMin - D, ZoomRect.Bottom);
        YGraphToImage(YGraphMax + D, ZoomRect.Top);

        Invalidate;
    end;
    if Assigned(OnZoom) then OnZoom(Self);
end;

procedure TTAChart.SetWidth(Value: Integer);
begin
    Bitmap.Width := Value;
    TControl(Self).Width := Value;
end;

procedure TTAChart.SetHeight(Value: Integer);
begin
    Bitmap.Height := Value;
    TControl(Self).Height := Value;
end;

function TTAChart.GetWidth: LongInt;
begin
    Result := Bitmap.Width;
end;

function TTAChart.GetHeight: LongInt;
begin
    Result := Bitmap.Height;
end;

procedure TTAChart.Resize;
begin
    inherited;
    Bitmap.Width := ClientWidth;
    Bitmap.Height := ClientHeight;
end;

function TTAChart.GetCanvas: TCanvas;
begin
    Result := Bitmap.Canvas;
end;

procedure Register;
begin
  RegisterComponents('TeleAuto', [TTAChart]);
end;

{$IFDEF fpc}
initialization
{$I tachart.lrs}
{$ENDIF}
end.

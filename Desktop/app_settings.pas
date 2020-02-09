{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of settings containers. 
Names of classes have non standard form because they 
are serialized into setting file.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit app_settings;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, SysUtils, LCLProc, Laz_XMLStreaming, Laz_DOM, Laz_XMLCfg, TypInfo,
    persistent_curve_parameters, SelfCheckedComponentList;

type
    { Contains and serializes attributes of mathematical expression. }
    Curve_type = class(TComponent)
    private
        FName: string;
        FExpression: string;
        FParameters: Curve_parameters;
        
        procedure SetParameters(AParameters: Curve_parameters);
        
        procedure ReadParams(Reader: TReader);
        procedure WriteParams(Writer: TWriter);
        
        function GetParams: TCollection;
        procedure SetParams(AParams: TCollection);

    public
        { File name to serialize / deserialize data. }
        FileName: string;
        
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure DefineProperties(Filer: TFiler); override;
        property Parameters: Curve_parameters
            read FParameters write SetParameters;

    published
        { Published properties are used in XML-serializing. }
    
        property Name: string read FName write FName;
        property Expression: string read FExpression write FExpression;
        //  By this way component is not written into XML-stream, we need to use DefineProperties.
        //property Params: Curve_parameters read FParams write FParams;
        { Expression parameters. }
        property Params: TCollection read GetParams write SetParams;
    end;
    
    { Contains and serializes application settings. }
    Settings_v1 = class(TComponent)
    private
        FCurveTypes: TSelfCheckedComponentList;
        FReserved: LongInt;
        
        procedure ReadCurveTypes(Reader: TReader);
        procedure WriteCurveTypes(Writer: TWriter);

    public
        constructor Create(Owner: TComponent); override;
        destructor Destroy; override;
        { Does not work with XML-streams. }
        procedure DefineProperties(Filer: TFiler); override;

        property Curve_types: TSelfCheckedComponentList
            read FCurveTypes write FCurveTypes;
        
    published
        { Dummy property. Prevents exceptions in reading. }
        property Reserved: LongInt read FReserved write FReserved;
    end;
  
function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
    Append: Boolean; var DestroyDriver: Boolean): TWriter;
function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
    var DestroyDriver: Boolean): TReader;

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    AComponent: TComponent);
procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    { Root component which are read from stream [in, out]. }
    var RootComponent: TComponent;
    OnFindComponentClass: TFindComponentClassEvent;
    { Owner of newly created component. }
    TheOwner: TComponent
    );

implementation

{$warnings off}
function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
    Append: Boolean; var DestroyDriver: Boolean): TWriter;
var Driver: TAbstractObjectWriter;
begin
    Driver := TXMLObjectWriter.Create(ADoc, Path, Append);
    DestroyDriver := True;
    Result := TWriter.Create(Driver);
end;

function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
  var DestroyDriver: boolean): TReader;
var p: Pointer;
    Driver: TAbstractObjectReader;
    DummyStream: TMemoryStream;
begin
    DummyStream := TMemoryStream.Create;
    try
        Result := TReader.Create(DummyStream, 256);
        DestroyDriver := False;
        // hack to set a write protected variable.
        // DestroyDriver := True; TReader will free it
        Driver := TXMLObjectReader.Create(ADoc, Path);
        p := @Result.Driver;
        Result.Driver.Free;
        TAbstractObjectReader(p^) := Driver;
    finally
        DummyStream := nil;
    end;
end;
{$warnings on}

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    AComponent: TComponent);
var
    Writer: TWriter;
    DestroyDriver: boolean;
begin
    Writer := nil;
    DestroyDriver := False;
    try
        Writer := CreateXMLWriter(XMLConfig.Document, Path, False, DestroyDriver);
        XMLConfig.Modified := True;
        Writer.WriteRootComponent(AComponent);
        XMLConfig.Flush;
    finally
        if DestroyDriver then
            Writer.Driver.Free;
        Writer.Free;
    end;
end;

procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    var RootComponent: TComponent;
    OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent);
var
    DestroyDriver: Boolean;
    Reader: TReader;
    IsInherited: Boolean;
    AClassName: String;
    AClass: TComponentClass;
begin
    Reader := nil;
    DestroyDriver := False;
    try
        Reader := CreateXMLReader(XMLConfig.Document, Path, DestroyDriver);
        Reader.OnFindComponentClass := OnFindComponentClass;

        // get root class
        AClassName := (Reader.Driver as TXMLObjectReader).GetRootClassName(IsInherited);
        if IsInherited then begin
            // inherited is not supported by this simple function
            //DebugLn('ReadComponentFromXMLConfig WARNING: "inherited" is not supported by this simple function');
        end;
        AClass := nil;
        //  poisk tipa klassa po imeni klassa
        OnFindComponentClass(nil, AClassName, AClass);
        if AClass=nil then
            raise EClassNotFound.CreateFmt('Class "%s" not found',  [AClassName]);

        if RootComponent=nil then begin
            // create root component
            // first create the new instance and set the variable ...
            RootComponent := AClass.NewInstance as TComponent;
            // then call the constructor
            RootComponent.Create(TheOwner);
        end else begin
            // there is a root component, check if class is compatible
            if not RootComponent.InheritsFrom(AClass) then begin
                raise EComponentError.CreateFmt('Cannot assign a %s to a %s.',
                                        [AClassName, RootComponent.ClassName]);
            end;
        end;

        Reader.ReadRootComponent(RootComponent);
    finally
        if DestroyDriver then
            Reader.Driver.Free;
        Reader.Free;
    end;
end;

{================================ Settings_v1 =================================}

constructor Settings_v1.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    FCurveTypes := TSelfCheckedComponentList.Create(nil);
    FReserved := -1;
end;

destructor Settings_v1.Destroy;
begin
    FCurveTypes.Free;
    inherited Destroy;
end;

{$hints off}
procedure Settings_v1.DefineProperties(Filer: TFiler);
begin
    //  !!! ne rabotaet s XML-potokami !!!
    //Filer.DefineProperty('Curve types', ReadCurveTypes, WriteCurveTypes, True);
end;

procedure Settings_v1.ReadCurveTypes(Reader: TReader);
begin
    FCurveTypes.Free;
    FCurveTypes := TSelfCheckedComponentList(Reader.ReadComponent);
end;
{$hints on}

procedure Settings_v1.WriteCurveTypes(Writer: TWriter);
begin
    Writer.WriteComponent(FCurveTypes);
end;

{================================ Curve_type ==================================}

constructor Curve_type.Create(AOwner: TComponent);
begin
    inherited;
    Parameters := Curve_parameters.Create(nil);
end;

destructor Curve_type.Destroy;
begin
    Parameters.Free;
    inherited;
end;

procedure Curve_type.SetParameters(AParameters: Curve_parameters);
begin
    FParameters.Free; FParameters := AParameters;
end;

{$hints off}
procedure Curve_type.DefineProperties(Filer: TFiler);
begin
    //Filer.DefineProperty('Params', ReadParams, WriteParams, True);
end;
{$hints on}

procedure Curve_type.ReadParams(Reader: TReader);
begin
    Parameters := Curve_parameters(Reader.ReadComponent(nil));
end;

procedure Curve_type.WriteParams(Writer: TWriter);
begin
    Writer.WriteComponent(Parameters);
end;

function Curve_type.GetParams: TCollection;
begin
    Result := Parameters.Params;
end;

procedure Curve_type.SetParams(AParams: TCollection);
begin
    Parameters.Params.Free;
    Parameters.Params := AParams;
end;

initialization
end.





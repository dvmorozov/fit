unit Settings;

//{$mode objfpc}{$H+}
{$MODE Delphi}//  trebuetsya dlya togo, chtoby skompilirovat' TFindContainer

interface

uses Classes, Laz_DOM, Laz_XMLCfg, Laz_XMLStreaming, LCLProc, SysUtils, TypInfo;

type
    Project_v1 = class(TComponent)
    protected
        FName: string;
        FDescription: string;

    published
        property Name: string read FName write FName;
        property Description: string read FDescription write FDescription;
    end;

    File_v1 = class(TComponent)
    protected
        FName: string;
        FDescription: string;

    published
        property Name: string read FName write FName;
        property Description: string read FDescription write FDescription;
    end;

    TFindContainer = class(TObject)
    public
        procedure OnFindComponentClass(Reader: TReader;
            const ClassName: string; var ComponentClass: TComponentClass);
    end;

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
    Append: boolean; var DestroyDriver: boolean): TWriter;
function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
    var DestroyDriver: boolean): TReader;

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    AComponent: TComponent);
procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    var RootComponent: TComponent;  //  [in, out] - kornevoy komponent,
    //  kotoryy chitaetsya iz potoka
    OnFindComponentClass: TFindComponentClassEvent;
    TheOwner: TComponent            //  vladelets novogo kornevogo komponenta
    );

function ReadProjectProperties_v1(FileName: string): Project_v1;
function ReadFileProperties_v1(FileName: string): File_v1;
procedure WriteProperties(FileName: string; Prop: TComponent);

implementation

var
    FC: TFindContainer;

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
    Append: boolean; var DestroyDriver: boolean): TWriter;
var
    Driver: TAbstractObjectWriter;
begin
    Driver := TXMLObjectWriter.Create(ADoc, Path, Append);
    DestroyDriver := True;
    Result := TWriter.Create(Driver);
end;

function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
    var DestroyDriver: boolean): TReader;
var
    p:      Pointer;
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
        p      := @Result.Driver;
        Result.Driver.Free;
        TAbstractObjectReader(p^) := Driver;
    finally
        DummyStream := nil;
    end;
end;

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
    var RootComponent: TComponent; OnFindComponentClass: TFindComponentClassEvent;
    TheOwner: TComponent);
var
    DestroyDriver: boolean;
    Reader:      TReader;
    IsInherited: boolean;
    AClassName:  string;
    AClass:      TComponentClass;
begin
    Reader := nil;
    DestroyDriver := False;
    try
        Reader := CreateXMLReader(XMLConfig.Document, Path, DestroyDriver);
        Reader.OnFindComponentClass := OnFindComponentClass;

        // get root class
        AClassName := (Reader.Driver as TXMLObjectReader).GetRootClassName(IsInherited);
        if IsInherited then
            // inherited is not supported by this simple function
            //DebugLn('ReadComponentFromXMLConfig WARNING: "inherited" is not supported by this simple function');
        ;
        AClass := nil;
        //  poisk tipa klassa po imeni klassa
        OnFindComponentClass(nil, AClassName, AClass);
        if AClass = nil then
            raise EClassNotFound.CreateFmt('Class "%s" not found', [AClassName]);

        if RootComponent = nil then
        begin
            // create root component
            // first create the new instance and set the variable ...
            RootComponent := AClass.NewInstance as TComponent;
            // then call the constructor
            RootComponent.Create(TheOwner);
        end
        else
        if not RootComponent.InheritsFrom(AClass) then
            raise EComponentError.CreateFmt('Cannot assign a %s to a %s.',
                [AClassName, RootComponent.ClassName])
        // there is a root component, check if class is compatible
        ;

        Reader.ReadRootComponent(RootComponent);
    finally
        if DestroyDriver then
            Reader.Driver.Free;
        Reader.Free;
    end;
end;

procedure TFindContainer.OnFindComponentClass(Reader: TReader;
    const ClassName: string; var ComponentClass: TComponentClass);
begin
    if CompareText(ClassName, 'Project_v1') = 0 then
        ComponentClass := Project_v1
    else
    if CompareText(ClassName, 'File_v1') = 0 then
        ComponentClass := File_v1
    else
        ComponentClass := nil;
end;

function ReadProjectProperties_v1(FileName: string): Project_v1;
var
    XMLConfig: TXMLConfig;
begin
    Result := nil;
    if FileExists(FileName) then
    begin
        Result := Project_v1.Create(nil);
        try
            XMLConfig := TXMLConfig.Create(FileName);
            try
                ReadComponentFromXMLConfig(XMLConfig, 'Component',
                    TComponent(Result), FC.OnFindComponentClass, nil);
            finally
                XMLConfig.Free;
            end;
        except
            Result.Free;
            raise;
        end;
    end;
end;

function ReadFileProperties_v1(FileName: string): File_v1;
var
    XMLConfig: TXMLConfig;
begin
    Result := nil;
    if FileExists(FileName) then
    begin
        Result := File_v1.Create(nil);
        try
            XMLConfig := TXMLConfig.Create(FileName);
            try
                ReadComponentFromXMLConfig(XMLConfig, 'Component',
                    TComponent(Result), FC.OnFindComponentClass, nil);
            finally
                XMLConfig.Free;
            end;
        except
            Result.Free;
            raise;
        end;
    end;
end;

procedure WriteProperties(FileName: string; Prop: TComponent);
var
    XMLConfig: TXMLConfig;
begin
    XMLConfig := TXMLConfig.Create(Filename);
    try
        WriteComponentToXMLConfig(XMLConfig, 'Component', Prop);
        XMLConfig.Flush;
    finally
        XMLConfig.Free;
    end;
end;

initialization
    FC := TFindContainer.Create;

finalization
    FC.Free;
end.

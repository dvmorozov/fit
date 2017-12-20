{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of component which can serialize itself.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit SelfSaved;

{$MODE Delphi}

interface

uses Classes, ClassInheritIDs;

type
    { Attribute descriptor for storing in file. }
    TPropHeaderRec = record
        { Unique class identifier in the inheritance chain. }
        ClassInheritID: Byte;           
        { Version number of class attribute. }
        PropVersionNum: Byte;
    end;

    { Component which can serialize itself into stream allowing 
      adding new attributes and changing order of inheritance without
      loosing ability to read existing files (backward compatibility).
      Writing (reading) class attributes starts from attributes of 
      the class which is last in the inheritance chain. 
      Reading / writing operations are implemented as class operations
      to provide possibility of moving by inheritance chain. }
    TSelfSavedComponent = class(TComponent)
    private
        { Calls the method of reading attributes of inheritance chain 
          based on results of reading starting and ending markers. }
        procedure ReadData(Reader: TReader);
        { Sequentially reads attribute descriptors from the file and
          searches class in the inheritance chain which is responsible
          for reading these data. }
        class procedure ReadInheritChain(
            const Reader: TReader;
            { The object attributes of which are read. }
            const AnObject: TSelfSavedComponent
            );
        { Reads attributes of class. }
        class procedure ReadPropHeader(const Reader: TReader;
            out PropHeaderRec: TPropHeaderRec
            );
        { Writes class attributes and markers of start and end of data. }
        procedure WriteData(Writer: TWriter);
        { Writes attributes of all inheritance chain up to TSelfSavedComponent 
          class sequentially moving by the chain. }
        class procedure WriteInheritChain(
            const Writer: TWriter;
            { Object attributes of which are written. }
            const AnObject: TSelfSavedComponent
            ); virtual;
        { Writes attribute descriptor. }
        class procedure WritePropHeader(const Writer: TWriter); virtual;

    protected
        { Checks if the class suppors given inheritance ID. }
        class function IsClassInheritIDValid(
            { Unique class id in the inheritance chain. }
            const ClassInheritID: Byte
            ): Boolean; virtual;
        { Returns attribute descriptor for given class. }
        class function GetPropHeaderRec: TPropHeaderRec; virtual;
        class procedure ReadProperties(
            { Reads class attributes from file (for this class does nothing). If number 
              of version in file is less than the current then proper initialization 
              algorithm is executed for values which can not be read from the file. }
            const Reader: TReader;
            const PropHeaderRec: TPropHeaderRec;
                //  запись заголовка свойств, прочитанная из файла
                //  запись передается полностью (а не только номер версии свойств)
                //  на тот случай, если класс поддерживает несколько ID
            const AnObject: TSelfSavedComponent
                //  объект, свойства которого нужно прочитать
            ); virtual;

        class procedure WriteProperties(
            //  выполняет запись свойств класса в файл (здесь ничего не делает)
            const Writer: TWriter;
            const AnObject: TSelfSavedComponent
                //  объект, свойства которого нужно записать
            ); virtual;

    public
        procedure DefineProperties(Filer: TFiler); override;
    end;

    TSelfSavedComponents = class of TSelfSavedComponent;

implementation

{ TSelfSavedComponent }

class function TSelfSavedComponent.IsClassInheritIDValid(
    const ClassInheritID: Byte): Boolean;
begin
    if ClassInheritID = GetPropHeaderRec.ClassInheritID then
        Result := True else Result := False;
end;

procedure TSelfSavedComponent.DefineProperties(Filer: TFiler);
begin
    Filer.DefineProperty(' ', ReadData, WriteData, True)
end;

class function TSelfSavedComponent.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SelfSavedInheritID;
    Result.PropVersionNum := SelfSavedCurVerNum;
end;

procedure TSelfSavedComponent.ReadData(Reader: TReader);
begin
    with Reader do
    begin
        ReadListBegin;
        ReadInheritChain(Reader, Self);
        ReadListEnd;
    end;
end;

class procedure TSelfSavedComponent.ReadProperties(
    const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent
    );
begin
end;

class procedure TSelfSavedComponent.ReadPropHeader(const Reader: TReader;
    out PropHeaderRec: TPropHeaderRec);
begin
    with Reader, PropHeaderRec do
    begin
        ClassInheritID := ReadInteger;
        PropVersionNum := ReadInteger;
    end;
end;

procedure TSelfSavedComponent.WriteData(Writer: TWriter);
begin
    with Writer do
    begin
        WriteListBegin;
        WriteInheritChain(Writer, Self);
        WriteListEnd;
    end;
end;

class procedure TSelfSavedComponent.WriteInheritChain(
    const Writer: TWriter;
    const AnObject: TSelfSavedComponent
    );
var CurClassType: TSelfSavedComponents;
begin
    CurClassType := TSelfSavedComponents(AnObject.ClassType);
    repeat
        (*with CurClassType do
        begin*)
            CurClassType.WritePropHeader(Writer);
            CurClassType.WriteProperties(Writer, AnObject);
        (*end;*)
        CurClassType := TSelfSavedComponents(CurClassType.ClassParent);
    until CurClassType = TSelfSavedComponent.ClassParent;
end;

class procedure TSelfSavedComponent.WriteProperties(
    const Writer: TWriter;
    const AnObject: TSelfSavedComponent
    );
begin
end;

class procedure TSelfSavedComponent.WritePropHeader(const Writer: TWriter);
var PropHeaderRec: TPropHeaderRec;
begin
    PropHeaderRec := GetPropHeaderRec;
    with Writer, PropHeaderRec do
    begin
        WriteInteger(ClassInheritID);
        WriteInteger(PropVersionNum);
    end;
end;

class procedure TSelfSavedComponent.ReadInheritChain(
    const Reader: TReader;
    const AnObject: TSelfSavedComponent
    );
var PropHeaderRec: TPropHeaderRec;
    CurClassType: TSelfSavedComponents;
begin
    while not Reader.EndOfList do
    begin
        ReadPropHeader(Reader, PropHeaderRec);

        CurClassType := TSelfSavedComponents(AnObject.ClassType);
        repeat
            (*with CurClassType do*)
                if CurClassType.IsClassInheritIDValid(PropHeaderRec.ClassInheritID) then
                begin
                    CurClassType.ReadProperties(Reader, PropHeaderRec, AnObject);
                    Break;
                end;
            CurClassType := TSelfSavedComponents(CurClassType.ClassParent);
        until CurClassType = TSelfSavedComponent.ClassParent;
    end;
end;

end.

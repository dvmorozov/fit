program readbuffer_sample;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, TypInfo,
  user_service_intf_proxy,
  synapse_tcp_protocol, synapse_http_protocol, library_protocol,
  soap_formatter, binary_formatter,
  user_service_intf, xmlrpc_formatter;

type
  TUser = TUser_Type;
  TUserCategory = TUserCategory_Type;

var
  UserServiceInst : UserService;

procedure ShowUser(AUser : TUser);
begin
  if ( AUser <> nil ) then begin
    WriteLn('  Name = ',AUser.UserName);
    WriteLn('  Category = ',GetEnumName(TypeInfo(TUserCategory),Ord(AUser.Category)));
    WriteLn('  e-Mail = ',AUser.eMail);
    WriteLn('  Preferences = ',AUser.Preferences);
    WriteLn('  Note = ',AUser.Note.Data);
    {WriteLn('    Author = ',AUser.Note.Author);
    WriteLn('    Date = ',AUser.Note.Date);}
  end else begin
    WriteLn('<Empty User>');
  end;
end;

procedure ShowUserArray(AArray : TUserArray);
var
  i, c : Integer;
  usr : TUser;
begin
  if ( AArray <> nil ) then begin
    c := AArray.Length;
    for i := 0 to Pred(c) do begin
      usr := AArray[i];
      WriteLn();
      WriteLn(Format('User[%d] : ',[(i+1)]));
      ShowUser(usr);
    end;
  end;
end;

procedure HandleShowAll();
var
  userArray : TUserArray;
begin
  userArray := UserServiceInst.GetList();
  try
    if ( userArray <> nil ) and ( userArray.Length > 0 ) then begin
      ShowUserArray(userArray);
    end else begin
      WriteLn('Empty Array.');
    end;
  finally
    FreeAndNil(userArray);
  end;
end;

type TAddType = ( atAdd, atUpdate );
procedure HandleAdd(const AType :TAddType);
const CAPTIONS : array[TAddType] of string = ( 'Adding a user :', 'Updating a user :' );
  function ReadItem(const APrompt : string; const ANonNull : Boolean):string ;
  begin
    Result := '';
    Write(APrompt);
    ReadLn(Result);
    Result := Trim(Result);
    if ANonNull and ( Length(Result) = 0 ) then
      Raise Exception.Create('Invalid User Name!');
  end;
var
  usr : TUser;
  buff : string;
begin
  buff := '';
  WriteLn(CAPTIONS[AType]);
  try
    usr := TUser.Create();
    try
      usr.UserName := ReadItem('Enter user name : ',True);
      buff := UpperCase(ReadItem('Enter user Category( A : Admin; N : normal ) : ',True));
      if ( buff[1] = 'A' ) then
        usr.Category:= Admin
      else
        usr.Category:= Normal;
      usr.eMail := ReadItem('Enter user e-mail : ',False);
      usr.Preferences := ReadItem('Enter user Preferences : ',False);
      buff := UpperCase(ReadItem('Do you want to add some notes : ',True));
      if ( buff[1] = 'O' ) then begin
        usr.Note.Data := ReadItem('Enter user Note : ',False);
        {usr.Note.Author := ReadItem('Enter user Note.Author : ',False);
        usr.Note.Date := ReadItem('Enter user Note.Date : ',False);}
      end;
      if ( AType = atUpdate ) then
        UserServiceInst.Update(usr)
      else
        UserServiceInst.Add(usr);
    finally
      FreeAndNil(usr);
    end;
  except
    on e : Exception do begin
      WriteLn(e.Message);
    end;
  end;
end;

procedure HandleFindUser();
var
  user : TUser;
  buff : string;
begin
  Write('Enter User Name : ');
  ReadLn(buff);
  user := UserServiceInst.Find(buff);
  try
    ShowUser(user);
  finally
    FreeAndNil(user);
  end;
end;

procedure HandleDeleteUser();
var
  buff : string;
begin
  Write('Enter User Name : ');
  ReadLn(buff);
  UserServiceInst.Delete(buff);
end;

type
  TTransportType = ( ttLibrary, ttTCP, ttHTTP );
  TFormatType = ( ftBinary, ftSoap, ftXmlRPC );
var
  TransportType : TTransportType;
  FormatValue : TFormatType;
procedure CreateProxy();
const ADDRESS_MAP : array[TTransportType] of string = (
        'LIB:FileName=..\library_server\lib_server.dll;target=UserService',
        'TCP:Address=127.0.0.1;Port=1234;target=UserService',
        //'http:Address=http://127.0.0.1:8080/wst/services/UserService/?format=soap'
        'http:Address=http://127.0.0.1:8000/services/UserService'
      );
      FORMAT_MAP : array[TFormatType] of string =( 'binary', 'soap', 'xmlrpc' );
var
  buff : string;
begin
  if ( TransportType = ttHTTP ) then
    buff := Format('%s/?format=%s',[ADDRESS_MAP[TransportType],FORMAT_MAP[FormatValue]])
  else
    buff := ADDRESS_MAP[TransportType];
  if ( TransportType = ttLibrary ) then
    buff := StringReplace(buff,'\',DirectorySeparator,[rfReplaceAll, rfIgnoreCase]);
  UserServiceInst := TUserService_Proxy.Create(
                       'UserService',
                       FORMAT_MAP[FormatValue] + ':',
                       buff
                     );
end;

procedure ReadTransportType();
var
  buff : string;
begin
  WriteLn();
  WriteLn('Select a transport protocol : ');
  WriteLn('  L : Library, the lib_server project must have been built');
  WriteLn('  T : TCP, the tcp_server must have been built');
  WriteLn('  H : HTTP, the http_server must have been built');
  WriteLn();
  Write('Your selection : ');
  while True do begin
    ReadLn(buff);
    buff := UpperCase(Trim(buff));
    if ( Length(buff) > 0 ) and ( buff[1] in ['L','T', 'H'] ) then begin
      case buff[1] of
        'L' : TransportType := ttLibrary;
        'T' : TransportType := ttTCP;
        'H' : TransportType := ttHTTP;
      end;
      Break;
    end;
  end;
end;

procedure ReadFormatType();
var
  buff : string;
begin
  WriteLn();
  WriteLn('Select a messaging format : ');
  WriteLn('  B : binary ( binary_formatter.pas )');
  WriteLn('  S : soap   ( soap_formatter.pas )');
  WriteLn('  X : XmlRpc ( xmlrpc_formatter.pas )');
  WriteLn();
  Write('Your selection : ');
  while True do begin
    ReadLn(buff);
    buff := UpperCase(Trim(buff));
    if ( Length(buff) > 0 ) and ( buff[1] in ['B','S', 'X'] ) then begin
      case buff[1] of
        'B' : FormatValue := ftBinary;
        'S' : FormatValue := ftSoap;
        'X' : FormatValue := ftXmlRPC;
      end;
      Break;
    end;
  end;
end;

var
  strBuffer : string;
begin
  SYNAPSE_RegisterTCP_Transport();
  SYNAPSE_RegisterHTTP_Transport();
  LIB_Register_Transport();
  WriteLn('Sample Application using Web Services Toolkit');
  ReadFormatType();
  ReadTransportType();
  CreateProxy();
  WriteLn('Menu :');
  WriteLn(' L : Show the user list');
  WriteLn(' A : Add a new user');
  WriteLn(' U : Update a user');
  WriteLn(' D : Delete a user');
  WriteLn(' F : Find a new');
  WriteLn(' C : Change the communication protocol');
  WriteLn(' Z : Change the messaging format');
  WriteLn(' X : Exit');
  WriteLn();
  Write('Choose a item : ');
  while True do begin
    strBuffer := '';
    ReadLn(strBuffer);
    strBuffer := UpperCase(Trim(strBuffer));
    if ( Length(strBuffer) > 0 ) then begin
      case strBuffer[1] of
        'L' : HandleShowAll();
        'A' : HandleAdd(atAdd);
        'U' : HandleAdd(atUpdate);
        'D' : HandleDeleteUser();
        'F' : HandleFindUser();
        'C' :
          begin
            ReadTransportType();
            CreateProxy();
          end;
        'Z' :
          begin
            ReadFormatType();
            CreateProxy();
          end;
        'X' : Break;
      end;
      WriteLn();
      Write('Choose a item : ');
    end;
  end;
end.



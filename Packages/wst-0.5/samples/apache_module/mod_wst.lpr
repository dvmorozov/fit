library mod_wst;

{$mode objfpc}{$H+}

{$IFDEF WIN32}
  {$DEFINE WINDOWS}
{$ENDIF}

uses
  SysUtils,
  httpd, apr, apriconv, aprutil, wst_apache_binding;

var
 wst_module: module; {$ifdef Unix} public name 'wst_module'; {$endif}
 default_module_ptr: Pmodule;

const
  MODULE_NAME = 'mod_wst.so';

{$ifdef WINDOWS}
exports
 wst_module name 'wst_module';
{$endif}


function DefaultHandler(r: Prequest_rec): Integer;cdecl;
begin
  if not SameText(r^.handler, 'wst-handler') then
  begin
    Result := DECLINED;
    Exit;
  end;

  Result := wst_RequestHandler(r);
end;

procedure RegisterHooks(p: Papr_pool_t);cdecl;
begin
  ap_hook_handler(@DefaultHandler, nil, nil, APR_HOOK_MIDDLE);
end;

begin
  default_module_ptr := @wst_module;
  FillChar(default_module_ptr^, SizeOf(default_module_ptr^), 0);
  STANDARD20_MODULE_STUFF(default_module_ptr^);
  with wst_module do
  begin
    name := MODULE_NAME;
    magic := MODULE_MAGIC_COOKIE;
    register_hooks := @RegisterHooks;
  end;
end.

// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Dumps every registration this build actually contains, as JSON.)

WHY A PROGRAM AND NOT A SCRIPT. The published architecture diagrams have to show
what is registered, and only the registries know that. A grep over the sources
gets it wrong in ways that matter: the REST verbs are added through a local
helper, so a naive pattern reports one verb instead of fourteen; the test
fixtures call the real registries with fakes, so every count is inflated; and no
line match can tell which module directory was on the unit search path, which is
the whole point of the module mechanism.

So this links the framework's own registration front doors, in the order
fit_server.lpr uses, and reads the registries back through the same public
functions the application and the tests use. What it prints is what this binary
would actually do.

It deliberately reports a seam with NOTHING registered rather than omitting it.
Five seams exist purely for modules and are empty in the public build; to an
extender those are the most interesting entries on the page, and a generator
that dropped empty collections would hide exactly the extension points the
diagrams exist to advertise.

Output goes to stdout, or to the file named by the single optional argument.
}
program dump_registries;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
    cthreads,
{$ENDIF}
    Interfaces,
    Classes, SysUtils, fpjson,
    //  The registration front doors, in the order fit_server.lpr calls them.
    app_modules,
    curve_type_registration,
    data_loader_registration,
    minimizer_registration,
    //  The registries themselves, for reading back.
    curve_types_singleton, int_curve_factory, int_curve_type_iterator,
    named_points_set, argument_axis,
    data_loader_registry,
    minimizer_registry, int_fit_backend,
    fit_loss,
    action_registry, fit_rest_api,
    module_registry, int_app_module,
    int_ui_host,
    int_module_overlay,
    curve_builder_registry,
    python_sidecar;

//  Every seam this build declares. Asserted against what is dumped, so a seam
//  that disappears fails the run instead of quietly vanishing from the diagram.
const
    SEAM_COUNT = 10;

function CurveTypesJson: TJSONArray;
var
    Iter: ICurveTypeIterator;
    Cls:  TCurveClass;
    Item: TJSONObject;
    Axis: TArgumentAxis;
begin
    Result := TJSONArray.Create;
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    if Iter = nil then
        Exit;
    Iter.FirstCurveType;
    if Iter.GetCurrentCurveClass = nil then
        Exit;
    //  EndCurveType means "the current type IS the last one", not "past the
    //  end", and NextCurveType RAISES when called on the last. So this walks
    //  bottom-tested: a while-not-End loop would silently drop the final type
    //  (it dropped Voigt), which is exactly the kind of quiet omission the
    //  generated diagrams exist to prevent.
    repeat
        Cls  := Iter.GetCurrentCurveClass;
        Item := TJSONObject.Create;
        Item.Add('name', Iter.GetCurveTypeName);
        Item.Add('id', GUIDToString(Iter.GetCurveTypeId));
        if Cls <> nil then
        begin
            Item.Add('class', Cls.ClassName);
            //  The capability questions the framework derives behaviour from.
            //  Read from the class, so an inherited answer is reported as the
            //  answer - which is what the application sees and what no line
            //  match over the sources could work out.
            //
            //  IsAnalytic and not GetCurveExpression: the expression is an
            //  INSTANCE method, and there is nothing to instantiate here. It
            //  costs nothing, because IsAnalytic is defined as "the expression
            //  is non-empty" and a test walks the registry asserting the two
            //  agree - so the class-level answer is the same fact.
            Item.Add('analytic', Cls.IsAnalytic);
            Item.Add('group', Cls.GetCurveTypeGroup);
            Item.Add('amplitude_unbounded', Cls.AmplitudeIsUnbounded);
            Item.Add('placed_by_point_set', Cls.PlacedByPointSet);
            Axis := Cls.CreatePreferredAxis(1.0);
            try
                if Axis <> nil then
                    Item.Add('preferred_axis', Axis.ClassName)
                else
                    Item.Add('preferred_axis', '');
            finally
                Axis.Free;
            end;
        end;
        Result.Add(Item);
        if Iter.EndCurveType then
            Break;
        Iter.NextCurveType;
    until False;
end;

function DataLoadersJson: TJSONArray;
var
    Infos: TDataLoaderInfoArray;
    i:     longint;
    Item:  TJSONObject;
begin
    Result := TJSONArray.Create;
    Infos  := RegisteredDataLoaders;
    for i := 0 to High(Infos) do
    begin
        Item := TJSONObject.Create;
        Item.Add('class', Infos[i].LoaderClass.ClassName);
        Item.Add('extensions', Infos[i].Extensions);
        Item.Add('format', Infos[i].FormatName);
        Result.Add(Item);
    end;
end;

function MinimizersJson: TJSONArray;
var
    Infos: TMinimizerInfoArray;
    i:     longint;
    Item:  TJSONObject;
begin
    Result := TJSONArray.Create;
    Infos  := RegisteredMinimizers;
    for i := 0 to High(Infos) do
    begin
        Item := TJSONObject.Create;
        Item.Add('kind', Infos[i].Kind);
        Item.Add('name', Infos[i].Name);
        Item.Add('description', Infos[i].Description);
        Item.Add('needs_formula', Infos[i].NeedsFormula);
        Item.Add('needs_python_sidecar', Infos[i].NeedsPythonSidecar);
        Item.Add('supports_weighting', Infos[i].SupportsWeighting);
        Item.Add('supports_curve_scaling', Infos[i].SupportsCurveScaling);
        //  Whether a factory is declared at all. Calling it would need a live
        //  context and could legitimately answer nil, which says nothing about
        //  the declaration - so only the declaration is reported.
        Item.Add('has_backend_factory', Assigned(Infos[i].CreateBackend));
        Result.Add(Item);
    end;
end;

function LossesJson: TJSONArray;
var
    Infos: TLossInfoArray;
    i:     longint;
    Item:  TJSONObject;
begin
    Result := TJSONArray.Create;
    Infos  := RegisteredLosses;
    for i := 0 to High(Infos) do
    begin
        Item := TJSONObject.Create;
        Item.Add('kind', Infos[i].Kind);
        Item.Add('name', Infos[i].Name);
        Item.Add('description', Infos[i].Description);
        Item.Add('self_normalising', Infos[i].IsSelfNormalising);
        Item.Add('least_squares', Infos[i].IsLeastSquares);
        //  A nil pooler is a declaration, not an oversight: it is what
        //  LossFromParts refuses on. Report it as declared.
        Item.Add('poolable', Assigned(Infos[i].Pool));
        Result.Add(Item);
    end;
end;

function ActionsJson: TJSONArray;
var
    Infos: TActionInfoArray;
    i:     longint;
    Item:  TJSONObject;
begin
    Result := TJSONArray.Create;
    Infos  := RegisteredActions;
    for i := 0 to High(Infos) do
    begin
        Item := TJSONObject.Create;
        Item.Add('name', Infos[i].Name);
        Item.Add('description', Infos[i].Description);
        Item.Add('asynchronous', Infos[i].IsAsynchronous);
        Result.Add(Item);
    end;
end;

function ModulesJson: TJSONArray;
var
    Mods: TAppModuleArray;
    Res:  TModuleResourceArray;
    i, j: longint;
    Item, R: TJSONObject;
    Arr:  TJSONArray;
begin
    Result := TJSONArray.Create;
    Mods   := RegisteredModules;
    for i := 0 to High(Mods) do
    begin
        Item := TJSONObject.Create;
        Item.Add('name', Mods[i].Name);
        Arr := TJSONArray.Create;
        Res := Mods[i].Resources;
        for j := 0 to High(Res) do
        begin
            R := TJSONObject.Create;
            R.Add('name', Res[j].Name);
            Arr.Add(R);
        end;
        Item.Add('resources', Arr);
        Result.Add(Item);
    end;
end;

function UiModulesJson: TJSONArray;
var
    Mods: TUiModuleArray;
    i:    longint;
    Item: TJSONObject;
begin
    Result := TJSONArray.Create;
    Mods   := RegisteredUiModules;
    for i := 0 to High(Mods) do
    begin
        Item := TJSONObject.Create;
        Item.Add('name', Mods[i].Name);
        //  The id a module pushes its structure rows under. It no longer
        //  buys the module a TAB - the Model panel is the framework's, and
        //  there is one of it - so the caption a module declares is not what
        //  the panel is called any more and is not reported as though it were.
        Item.Add('structure_id', Mods[i].PanelId);
        Result.Add(Item);
    end;
end;

{ One seam's entry. AEntryPoint and AAccepts are what an extender has to write;
  ADocs points at the prose. AItems is what is registered here and now, and is
  never omitted when empty - empty IS the finding for a module-only seam. }
function Seam(const AKey, AName, AEntryPoint, AAccepts, APurpose: string;
    AModuleOnly: boolean; AItems: TJSONArray): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('key', AKey);
    Result.Add('name', AName);
    Result.Add('entry_point', AEntryPoint);
    Result.Add('accepts', AAccepts);
    Result.Add('purpose', APurpose);
    Result.Add('module_only', AModuleOnly);
    Result.Add('count', AItems.Count);
    Result.Add('items', AItems);
end;

{ A seam whose registry has a count but no enumerable detail. }
function CountOnlySeam(const AKey, AName, AEntryPoint, AAccepts,
    APurpose: string; AModuleOnly: boolean; ACount: longint): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('key', AKey);
    Result.Add('name', AName);
    Result.Add('entry_point', AEntryPoint);
    Result.Add('accepts', AAccepts);
    Result.Add('purpose', APurpose);
    Result.Add('module_only', AModuleOnly);
    Result.Add('count', ACount);
    Result.Add('items', TJSONArray.Create);
end;

function SidecarPacksJson: TJSONArray;
var
    Packs: string;
    List:  TStringList;
    i:     longint;
    Item:  TJSONObject;
begin
    Result := TJSONArray.Create;
    Packs  := SidecarModules;
    if Packs = '' then
        Exit;
    List := TStringList.Create;
    try
        List.Delimiter := PathSeparator;
        List.StrictDelimiter := True;
        List.DelimitedText := Packs;
        for i := 0 to List.Count - 1 do
            if Trim(List[i]) <> '' then
            begin
                Item := TJSONObject.Create;
                Item.Add('name', Trim(List[i]));
                Result.Add(Item);
            end;
    finally
        List.Free;
    end;
end;

var
    Root:  TJSONObject;
    Seams: TJSONArray;
    Text:  string;
    Dest:  string;
    Out:   TStringList;
begin
    //  The order fit_server.lpr uses. RegisterAppModules is the stub that a
    //  module directory overrides through the unit search path, so whether
    //  anything follows from it is decided by which projct file built this.
    RegisterAppModules;
    RegisterAllCurveTypes;
    RegisterAllDataLoaders;
    RegisterAllMinimizers;
    RegisterBuiltInLosses;
    //  The REST verbs register lazily, from the first RunAction. Nothing here
    //  runs an action, so ask for them explicitly.
    RegisterBuiltInActions;

    Seams := TJSONArray.Create;
    Seams.Add(Seam('curve_types', 'Curve type',
        'TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType',
        'TCurveClass (class of TNamedPointsSet)',
        'A shape that can be placed on data and fitted. The only seam whose ' +
        'members self-register, from the unit''s own initialization section.',
        False, CurveTypesJson));
    Seams.Add(Seam('data_loaders', 'Data loader',
        'RegisterDataLoader', 'TDataLoaderClass + extensions + format name',
        'Reads one file format. The Open dialog''s filter is built from this ' +
        'registry, so a build can only offer what something can actually read.',
        False, DataLoadersJson));
    Seams.Add(Seam('minimizers', 'Minimizer',
        'RegisterMinimizer', 'TMinimizerInfo (with a TBackendFactory)',
        'An engine, and the capability answers the client greys choices out ' +
        'from. Its factory returns the IFitBackend that does the work.',
        False, MinimizersJson));
    Seams.Add(Seam('losses', 'Loss function',
        'RegisterLoss', 'TLossInfo (evaluate + pool)',
        'What "best fit" means. Compatibility with a curve type is DERIVED ' +
        'from the two capability flags, never from a table of type names.',
        False, LossesJson));
    Seams.Add(Seam('actions', 'REST action',
        'RegisterAction', 'TActionInfo (with a TActionHandler)',
        'One verb of the HTTP API the desktop client and any other caller use.',
        False, ActionsJson));
    Seams.Add(Seam('app_modules', 'Server module',
        'RegisterAppModule', 'IAppModule',
        'An analysis vertical''s server side: the resources it answers and ' +
        'its per-problem session state.',
        True, ModulesJson));
    Seams.Add(Seam('ui_modules', 'UI module',
        'RegisterUiModule', 'IUiModule',
        'A module''s contribution to the window, declared as data. One ' +
        'declaration serves three surfaces: the menu bar, the Tools pane and ' +
        'the Model panel''s context menu - Surface says where an entry is ' +
        'shown and Scope whether it acts on the selected row. Structure for ' +
        'the Model panel is pushed separately under structure_id, and the ' +
        'panel itself belongs to the framework. WHICH contributor fills it is ' +
        'not arbitrated: it follows the selected curve type''s ' +
        'placed_by_point_set, reported on the curve_types seam - empty means ' +
        'the framework''s flat list of curves, and a named set means the ' +
        'module owning that set describes what its markup produced. The ' +
        'client names no module type.',
        True, UiModulesJson));
    Seams.Add(Seam('sidecar_packs', 'Python sidecar pack',
        'RegisterSidecarModule', 'a package name',
        'Names a module''s <name>_routes.py, which the sidecar imports on ' +
        'start. Only the name travels; the sidecar finds the file itself.',
        True, SidecarPacksJson));
    Seams.Add(CountOnlySeam('curve_builders', 'Curve builder',
        'RegisterCurveBuilder', 'point set name -> TCurveBuilder',
        'Builds every curve a marked point set describes. Without one, the ' +
        'engine takes the position-based path.',
        True, CurveBuilderCount));
    Seams.Add(CountOnlySeam('module_overlays', 'Chart overlay',
        'RegisterModuleOverlay', 'TModuleOverlayProc',
        'Redraws a module''s own marks after the model is recomputed, so the ' +
        'client need not name the module to refresh it.',
        True, ModuleOverlayCount));

    if Seams.Count <> SEAM_COUNT then
        raise Exception.CreateFmt(
            'This build declares %d extension seams, but %d were dumped. A seam ' +
            'was added or removed without updating dump_registries, and the ' +
            'published diagram would silently stop mentioning it.',
            [SEAM_COUNT, Seams.Count]);

    Root := TJSONObject.Create;
    try
        Root.Add('seams', Seams);
        //  Cross-check for the generator: the loaders DECLARED in the sources
        //  against the loaders actually registered. The gap is real - a stub
        //  loader that raises ENotImplemented is declared but not registered -
        //  and the site reports it as Partial rather than smoothing it over.
        Root.Add('data_loader_registered_count', DataLoaderCount);
        Root.Add('minimizer_count', MinimizerCount);
        Root.Add('action_count', ActionCount);
        Root.Add('app_module_count', ModuleCount);
        Root.Add('ui_module_count', UiModuleCount);

        Text := Root.FormatJSON;
    finally
        Root.Free;
    end;

    if ParamCount >= 1 then
    begin
        Dest := ParamStr(1);
        Out := TStringList.Create;
        try
            //  Text, not Add: Add would split the JSON on its own newlines and
            //  SaveToFile would then re-join them with the platform separator.
            Out.Text := Text;
            Out.SaveToFile(Dest);
        finally
            Out.Free;
        end;
        WriteLn(StdErr, 'Registries dumped to ', Dest);
    end
    else
        WriteLn(Text);
end.

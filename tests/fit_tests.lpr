program fit_tests;
{$mode objfpc}{$H+}
uses
{$IFDEF UNIX}
  //  Must come first: tests that start a thread need a thread manager.
  cthreads,
{$ENDIF}
{$IFDEF UseNoGUI}
  Interfaces,
{$ENDIF}
  consoletestrunner, testcase_smoke, testcase_suite_split, testcase_dat_loader,
  testcase_data_loader_registry, testcase_loss_registry,
  testcase_lineshapes,
  testcase_curve_class, testcase_curve_expression, testcase_parameter_bounds,
  testcase_first_varying_parameter,
  testcase_pearson7, testcase_moffat, testcase_doniach_sunjic, testcase_emg, testcase_voigt,
  testcase_skewed_gaussian, testcase_step,
  testcase_expr_fidelity, testcase_minimizer,
  //  The downhill simplex driven through its own server interface: the
  //  stopping rules, the restart machinery, and the simulated-annealing
  //  variant, which is a second algorithm class nothing had ever instantiated.
  testcase_simplex,
  //  What a minimiser needs before it will run - nine near-identical guards
  //  nothing exercised - and the three abscissae one diffraction sample has.
  testcase_minimizer_contract,
  //  How one flat parameter vector is assembled from several curves. With one
  //  curve the mapping is the identity, so the defect it guards against cannot
  //  appear in any test written by hand.
  testcase_simplex_server,
  //  How a curve's position is shown in the parameter table and read back when
  //  it is typed. On the identity axis the transform is nothing, so a broken
  //  one and a correct one behave identically - which is the default.
  testcase_curve_list_axis,
  testcase_parameter_policies,
  testcase_parameter_choices,
  testcase_chart_panning,
  testcase_view_mode_restore,
  testcase_sidecar_startup,
  //  The captions the axis presenter puts on a position. It reaches
  //  mscr_specimen_list, which has been LCL-free since the grid concerns moved
  //  to Desktop/curve_list_grid.pas - so it builds in the light suite too.
  testcase_axis_presenter,
  testcase_argument_axis,
  testcase_native_math_expr, testcase_user_formula_parameters,
  testcase_background_parameters, testcase_background_search,
  testcase_worker_protocol, testcase_fit_problem_json,
  testcase_ohlc_loader,
  testcase_fit_loss, testcase_fit_advice,
  testcase_param_typed_value,
  testcase_checks,
  //  Plain arithmetic over pixels, so it belongs in the suite that runs in
  //  seconds - and what it defends is a click the user makes constantly.
  testcase_pick_target,
  //  What pixel density the interface lays itself out for. Extracted from
  //  ui_dpi, which opens with `uses Forms` - so none of it could be tested,
  //  and all of it is wrong only on somebody else's desktop.
  testcase_ui_scaling,
  //  Which commands the window offers and which are ticked. Extracted from four
  //  methods of the main form that packed bit flags into widget Tags; a command
  //  wrongly enabled or a tick left behind is the user's whole experience of the
  //  program being broken.
  testcase_action_state,
  //  What the user is told next while picking, and when a gesture ends. Taken
  //  out of a chart click handler, where the only way to reach it was to click.
  testcase_pick_guidance,
  //  How a parameter is treated, in the terms the user reads - the parameter
  //  table's colouring is the only sign of which numbers the fit may move.
  testcase_parameter_kinds,
  //  The tree a module's flattened outline describes. Exercising it needed a
  //  window with a module installed, and the framework ships no module - so the
  //  rule that decides what belongs to what had never been run by a test.
  testcase_outline_layout,
  //  What a formula editor's keypad does to the text and the caret. Forty button
  //  handlers call one of two methods, so a caret one character out is a wrong
  //  formula under every one of them.
  testcase_formula_editing,
  //  Which parameter of a user-defined curve holds which role. "At most one" was
  //  written out four times in an LCL dialog and asserted nowhere; two
  //  amplitudes is a fit seeded twice from one peak.
  testcase_parameter_roles,
  //  Which of the chart's sixteen colours a curve is drawn in. The rule it
  //  replaced read outside the palette for every thirty-second curve.
  testcase_series_palette,
  //  How each series on the chart is drawn - which shape says "asked for" and
  //  which says "achieved", and which series the View markers toggle may leave
  //  blank. Ten methods set it by hand behind a chart, a form and the widget
  //  set, so none of it had ever been run by a test.
  testcase_series_style,
  //  What choosing a curve type from the menu means. The rule for a type whose
  //  setup the user cancels - select it on its defaults, or refuse and say so -
  //  was a nested if/else with an empty branch inside a while-true loop inside
  //  an LCL action handler.
  testcase_curve_type_choice,
  //  What closing the window does about unsaved work - and in particular that a
  //  save the user asked for and which failed must stop the close. Two copies of
  //  thirty lines in an LCL close handler, which had already begun to differ.
  testcase_close_query,
  //  What the window has been asked to do later, and when it may - the
  //  machinery behind two freezes, and until now reachable only by reproducing
  //  them. Includes the rule that looks like a mistake: a second message while
  //  one is outstanding is dropped, because a repeating fault would otherwise
  //  stack dialogs until nothing else on screen can be reached.
  testcase_deferred_ui,
  //  Pointing the client at a compute server. The rule that matters is
  //  invisible when it is wrong: a profile loaded while no server was reachable
  //  exists only in the client, so naming one that answers has to hand it over.
  testcase_server_connection,
  //  The small tables beside the chart. The intervals one has to show the
  //  half-picked interval the user is in the middle of marking, which is the
  //  ordinary state rather than an edge.
  testcase_points_tables,
  //  What editing a cell of the profile table means, and how a table leaves
  //  this program as text. The one place data is typed in, and the only export.
  testcase_grid_edit,
  //  How the curve-type menu is laid out. The grouping had never been exercised
  //  with more than one group: the framework ships no curve pack, and building
  //  the menu needed a window.
  testcase_curve_type_menu,
  //  THE ONE TABLE EVERY COMMAND SURFACE IS DRAWN FROM. The window used to map
  //  commands onto widgets by hand, in a method that needs a window, so a
  //  command bound to the wrong action - or to none - was found by clicking.
  testcase_ui_commands,
  //  A component name made from data that the widget set will accept. Setting
  //  one it will not RAISES, and the shipped curve types include a name that
  //  begins with a digit.
  testcase_ui_names,
  //  What the Model panel shows when the framework is the one filling it, and
  //  which of the two contributors that is - the decision named_points_set
  //  records as having presented as a hang when it was made the other way.
  testcase_model_outline,
  //  Where each heading and button of the Tools pane sits. The pane is
  //  generated from a table a module can add rows to, so how many rows and
  //  groups there are is unknown until run time - and a button drawn over
  //  another looks like the pane simply not offering that command.
  testcase_tool_pane_layout,
  //  A module's declarations, all the way to positioned buttons in that pane.
  //  Every piece of that path had tests and the path itself had none, which is
  //  how a build shipped where the pane showed no module at all.
  testcase_module_pane,
  //  Which series on the chart were drawn for which curve. A curve is not one
  //  line, and nothing on the chart says which series belong together - a
  //  title is neither unique nor an identity.
  testcase_series_register,
  //  Whether the surfaces built from one command table agree with each other.
  //  The check itself runs inside the application, over real widgets, and is
  //  the one thing that cannot verify its own rules.
  testcase_ui_selfcheck,
  //  The menu a module's declarations describe - the load-bearing half of the
  //  contract that lets a module name no widget, and never run against anything
  //  but the one pack that exists.
  testcase_module_menu,
  //  The user-defined argument axis. An axis with only f(x) draws perfectly and
  //  cannot be clicked in, which is worse than one that does not work.
  testcase_custom_axis,
  //  Where the pieces of an owner-drawn legend row sit. None of these failures
  //  raise anything - the legend just looks slightly wrong, on one widget set.
  testcase_legend_layout,
  //  The numbers along the bottom of the window. During a long fit the status
  //  bar is the only thing on screen that changes.
  testcase_status_readout,
  //  A number as a user typed it. The box this came out of swapped the
  //  process-wide decimal separator around a call that raises.
  testcase_typed_number,
  //  Whatever module tests this build contains - see module_tests.
  module_tests,
  //  log.pas pulls nothing but SysUtils, and what it guards - every process
  //  logging everything without being asked - is cross-cutting enough that
  //  it should fail in the suite that runs in seconds, not the one that
  //  runs in minutes.
  testcase_log,
  //  What the client does with a fault that reached the top level. The decision
  //  is plain classes, so it belongs in the suite that runs in seconds; what it
  //  guards is a freeze that took the whole desktop session with it.
  testcase_client_fault,
  //  The curve window: which samples a curve holds and where they sit.
  testcase_curve_window,
  //  What the datasheet says about a fit - the only place it is readable as
  //  numbers. Extracted from fit_viewer, where it was written one grid cell at
  //  a time inside a unit that reaches into the main form by name.
  testcase_summary_table,
  //  TPointsSet.Sort. points_set pulls nothing but SimpMath, and what it guards
  //  is an out-of-bounds read that a release build performed silently - so it
  //  belongs in the suite that runs in seconds.
  testcase_points_set,
  //  Copying and ownership in the list every model is built out of. Two kinds of
  //  copy differing only in who frees the items, which is exactly the kind of
  //  distinction that fails silently on the machine that wrote it.
  testcase_self_copied,
  //  Curve INSTANCE identity: the handle itself, and the registry that says
  //  which instance each pick stands for. Both are plain classes over plain
  //  values - no profile, no fit, no server - so they belong in the suite that
  //  runs in seconds, and in the half where coverage is measured.
  testcase_curve_instance_id,
  testcase_curve_identity_registry,
  //  Goodness-of-fit statistics. fit_statistics.pas uses nothing but Math, so
  //  there was never a reason for this to sit in the LCL-linked half - it was
  //  simply written next to the fitting tests it resembles. It belongs in the
  //  suite that runs in seconds, and in the half where coverage is measured.
  testcase_fit_statistics,
  //  What the parameter table shows and accepts. Plain objects over plain
  //  values - no grid - which is the whole point: these rules used to live
  //  inside methods taking a TStringGrid, and an LCL grid cannot be driven
  //  headlessly, so none of them could be tested at all.
  testcase_curve_list,
  testcase_curve_list_memory,
  //  Which REST routes are heartbeats. One pure function over string literals -
  //  it was asserted inside the log test, whose class needs files, so it ran only
  //  in the slow half and counted toward nothing.
  testcase_rest_polling,
  //  The DAT format's silent decisions, driven over string literals. The loader
  //  test beside it still reads Data/1.dat, which is about the file being
  //  reachable rather than about the format.
  testcase_dat_parser,
  testcase_data_loader,
  //  Which reader opens a file and who owns it afterwards. No file is opened:
  //  the decision is made from the name alone.
  testcase_loader_injector,
  //  The points wire format: what both processes exchange for every profile.
  testcase_fit_points_json,
  //  The two contracts int_module_overlay documents and nothing enforced, plus
  //  the view vocabulary a module draws with.
  testcase_module_overlay,
  //  The rules that keep a build's modules unambiguous. Needs a mock module,
  //  because the framework deliberately contains no real one.
  testcase_module_registry,
  testcase_curve_builder_registry,
  //  The registry a feature module's UI registers into. int_ui_host is LCL-free
  //  by design - a module names no widget - so this belongs in the fast half,
  //  and it needed a mock module because the framework ships none.
  testcase_ui_module_registry,
  //  A tick that arrives before the row it belongs to. The list box reports the
  //  two in the wrong order, and applying the tick anyway fixes the parameter
  //  the user was previously looking at - one interaction after the cause.
  testcase_deferred_tick,
  //  Which computed series is named and which is drawn when it comes back empty.
  //  Five copies of two conditions inside a hundred-line routine, two of which
  //  differed from the other three - and the difference is the rule.
  testcase_computed_series,
  //  Which of the compute service states admit which operation. Five rules the
  //  service stated about thirty times inline, where none of them could be
  //  reached without the optimiser; each is a function of the state alone.
  testcase_service_state_rules,
  //  Which route a request names, as a table rather than as twenty-one
  //  conjunctions inside the largest routine in the counted half of the program.
  testcase_rest_routes,
  //  How a residual is weighted. The name was a bare literal in six places and
  //  the rule that reads it lives in the Python sidecar, so the Pascal side
  //  matches its exact, case-sensitive test deliberately rather than by luck.
  testcase_fit_weighting,
  //  Which of the two dialogs comes next when a user defines a curve. It was
  //  two labels and three gotos around two ShowModal calls, so no branch of it
  //  could be reached without opening a window - which is to say, none were.
  testcase_user_curve_wizard,
  //  SimpMath's vector, affine and numeric helpers. The affine machinery is
  //  pinned in the orthonormal case, where every function must reduce exactly to
  //  its Cartesian counterpart - an answer that needs no second derivation.
  testcase_simpmath,
  //  The solution representations the minimizers are built on. TFloatDecision is
  //  what the downhill simplex actually uses, so its copying and comparison are
  //  load-bearing for every fit.
  testcase_decisions, testcase_comb_enumerator,
  testcase_my_exceptions,
  //  The thread runner a fit runs inside, and the OnProcessMessages seam that let
  //  it stop depending on the LCL. Threads are in-process, so this is a unit test.
  testcase_running_thread,
  //  The two ways an algorithm is run - on the caller thread, or handed to that
  //  runner. The contract is one order of two calls, and it is the reason a
  //  descendant never has to remember to report its own result.
  testcase_algorithm_container,
  //  The thread the in-process engine computes on. Never started: Synchronize
  //  called from the main thread runs its method inline, which is the only way
  //  the eight callbacks it marshals can be observed at all.
  testcase_calc_thread,
  //  What the client makes of the server's replies. http_fit_service is LCL-free
  //  since the curve-list split, and its transport now has a seam - so seven
  //  hundred lines of marshalling are reachable without a socket.
  testcase_http_service_marshalling,
  testcase_service_actions,
  testcase_service_reading,
  testcase_service_problem,
  testcase_service_user_curve,
  //  How the Python sidecar is addressed and started. Extracted from
  //  python_sidecar so that the argument list - which cannot be read back off a
  //  running child - is decided somewhere a test can see it.
  testcase_sidecar_launch,
  //  What the launcher a package installs decides before the client starts: which
  //  port, whether a server is already answering, and what a file the desktop
  //  handed over has to become before the client will look at it.
  testcase_launcher_rules,
  //  What the client accepts on its own command line - the half the launcher
  //  translates into. It lived nested inside Fit.lpr, where no test could reach
  //  it, and it is the rule that decides whether a file opened from the desktop
  //  reaches the application at all.
  testcase_command_line_switches,
  //  What the client asks the chart to draw. IFitViewer was declared so this
  //  could be driven without a chart and nothing had ever implemented it for a
  //  test; fit_client sat at 18 % as a result.
  testcase_fit_client_view,
  testcase_selection_modes,
  testcase_client_settings,
  testcase_client_profile,
  testcase_client_log,
  testcase_client_commands,
  testcase_client_async_commands,
  testcase_client_picking,
  testcase_points_set_edit,
  testcase_curve_parameters,
  //  The bridge from the fitting statistics to a live service: which points are
  //  inside the window, how many parameters are varying, and each precondition it
  //  refuses on. Reached through the mocked transport, so no fit is run.
  testcase_service_statistics
{$IFDEF UseNoGUI}
  //  Integration test that spawns the worker binary (built by build-full.sh);
  //  nogui-only so the light suite need not build the worker.
  //  Pulls every curve unit, and user_points_set needs the LCL - so it can only
  //  build in the LCL-linked nogui suite, not the plain-FPC light one.
  , testcase_curve_type_registration
  //  Both link the curve types - one to build a TFitTask and compute profiles
  //  from them, one to walk the minimizer registry whose backends are declared
  //  over the same units - and the user-defined type among them names LCL
  //  Controls in its configuration dialog. Verified by moving them and watching
  //  the light build fail on that unit, not assumed.
  , testcase_profile_fidelity
  , testcase_minimizer_registry
  //  Reaches fit_rest_api, which pulls the service and so the LCL - the same
  //  reason the REST tests live here.
  , testcase_action_registry
  , testcase_rest_api
  , testcase_http_fit_service
  , testcase_worker_process
  //  Both reach the user-defined curve type, whose configuration dialog names
  //  LCL Controls - so they only build in the LCL-linked nogui suite. Unit
  //  tests all the same: no dialog is opened and nothing leaves the process.
  , testcase_curve_configuration
  , testcase_user_points_set
  //  Nothing here opens a window - the definition sequence is behind five
  //  interfaces and driven by doubles - but it reaches app_settings and the
  //  curve-type machinery, which the light plain-FPC suite does not link.
  , testcase_user_curve_flow
  //  The piece that sequence makes. Same reason for being here: no window, but
  //  app_settings and the curve-type machinery.
  , testcase_curve_type_factory
  //  The user's own stored curve types - the search behind three menu handlers.
  //  Here rather than in the light suite for the same reason as its neighbours:
  //  it reads app_settings, which names Laz_DOM, and the light suite links no
  //  Lazarus packages.
  , testcase_user_curve_library
  //  The engine's own surface - its settings, its callbacks and its refusals.
  //  Twenty-two methods of the largest unit in the project were entirely cold;
  //  it turns out the engine is an ordinary object and nothing had tried.
  , testcase_service_surface
  //  Pushing picks back WITH their curves' handles, which is what makes a saved
  //  fit resume rather than re-seed. Beside its neighbour and for the same
  //  reason: it drives TFitService directly, which the light suite cannot link.
  , testcase_service_identity_restore
  //  The project file's container. Here rather than in the light suite only
  //  because the light suite's unit path does not reach Common; every byte it
  //  moves goes through a TMemoryStream, so it needs nothing else.
  , testcase_project_archive
  //  The project document and its JSON sections. Beside the container for the
  //  same reason - Common is not on the light suite's unit path - and it needs
  //  nothing else: records, JSON and a memory stream.
  , testcase_project_document
  //  The order a restore has to happen in, as a pure function of the document.
  //  No engine and no socket - but the unit under test lives in Desktop, whose
  //  path only the nogui half has.
  , testcase_project_restore_order
  //  Capturing a live problem into a document and applying one back. Drives two
  //  real TFitService instances in process - no socket, no file, no run to
  //  convergence - so it is a unit test by the dependency rule.
  , testcase_project_session
  //  What the application opens at start-up. A pure decision - the existence
  //  check is passed in - but the unit lives in Desktop.
  , testcase_recent_project
  //  And what the application DOES with that decision, driven exactly as
  //  Fit.lpr drives it - including the production entry point that
  //  supplies its own existence check, which is where the defect was.
  , testcase_startup_sequence
  //  How a long server operation is run off the main thread, and what
  //  happens to it when the operation - or the refresh after it - fails.
  , testcase_client_async
  //  What a refusal calls the gesture the user should have made instead,
  //  and what it says when no module answers for it.
  , testcase_service_placement_gesture
  //  What the File menu's document commands decide, with the dialogs left in
  //  the window where nothing can test them.
  , testcase_project_commands
  //  The whole feature end to end, through real files - which is what makes it
  //  the integration half's business rather than the unit half's.
  , testcase_project_file
  //  How an analysis pack keeps its own state in a project without the
  //  framework knowing anything about it.
  , testcase_project_module_state
  //  Where a project's data came from, and whether that file still says the
  //  same thing. Split: hashing bytes is a unit test, reading a file is not.
  , testcase_project_provenance
  //  The document commands as sequences, over a faked window. Integration:
  //  it writes real files.
  , testcase_project_workflow
  //  The loop that gets an export name out of the user. It was inside the
  //  window until this feature; the questions were already tested, the loop
  //  around them was not reachable.
  , testcase_export_conversation
  //  The window's half of a project: what is captured, and how much of it
  //  comes back. Three fields were silently missing while this lived in the
  //  form, which is excluded from the coverage target.
  , testcase_project_ui_context
  //  What the client holds after a project is restored under it, and after
  //  New. Two defects lived here because every project test drove the
  //  service and none looked at the client.
  , testcase_client_project_state
  //  Both link the user-defined curve type so that its self-registration runs,
  //  and its configuration dialog names LCL Controls.
  , testcase_axis_defaulting
  , testcase_pick_refresh
  , testcase_fit, testcase_settings_persistence, testcase_user_curve,
  //  The settings model and its XML serialization with no file involved. Needs
  //  LazUtils for TXMLConfig, which only the nogui half has a unit path for -
  //  but it is a UNIT test: the document never leaves memory.
  testcase_settings_model,
  testcase_curve_type_selection,
  testcase_curve_params_persistence, testcase_user_curve_fit,
  //  Writing a backend's answer back onto the model - the last step of every fit
  //  that does not run in this process, and the one whose rules all fail as a
  //  plausible curve rather than as an error.
  testcase_outcome_apply,
  testcase_task_preconditions,
  //  Which routes the threaded server answers without taking the problem lock.
  //  Both ways of getting it wrong are invisible from outside: a polled route
  //  that waits freezes the client for the length of every fit, and an engine
  //  route that does not is a data race.
  testcase_route_locking,
  //  What may be done to a menu while the user is standing in one, and the entry
  //  shape - a submenu parent that is also tickable - whose widget the widget set
  //  destroys to give it a check box, leaving the open submenu owned by nothing.
  testcase_ui_menus,
  testcase_fit_marshalling,
  //  The adapter that fits on the Python sidecar, with no sidecar. Nogui-only
  //  for the same reason the marshalling tests are: it builds a real TFitTask.
  testcase_python_backend,
  testcase_open_without_server,
  testcase_python_backend_process,
  testcase_python_real_data,
  //  Reads the version resource with LazUtils' fileinfo, which the plain-FPC
  //  light suite has no unit path for: nogui links the LCL, and LazUtils with it.
  testcase_app_version,
  //  Touches fit_task/fit_task_marshalling (LCL-linked), so nogui-only.
  testcase_loss_real_data
{$ENDIF}
  ;
var
  App: TTestRunner;
begin
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'Fit unit tests';
  App.Run;
  App.Free;
end.

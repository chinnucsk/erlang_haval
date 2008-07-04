{application, haval,
  [ {description, "HAVAL bindings for Erlang"},
    {vsn, "1.0.0"},
    {modules, [haval, haval_app, haval_sup, haval_server]},
    {registered, [haval_sup]},
    {applications, [kernel, stdlib]},
    {mod, {haval_app, []}},
    {start_phases, []}]}.

{application, worker,
 [{description, "tk server worker appcalition!!"},
  {id, "worker"},
  {vsn, "0.1"},
  {modules, []},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {worker_app, []}},
  {env, []}
  ]}.

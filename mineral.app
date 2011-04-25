%% This is the application resource file (.app file) for the 'base' 
%% application. 
{application, mineral, 
[{description, "Erlang Minecraft Server"}, 
{vsn, "1.0"},
{kernel,
  [{distributed, [{mineral, 5000, [mineral@local]}]},
   {sync_nodes_mandatory, [mineral@local]},
   {sync_nodes_timeout, 5000}
  ]
 },
{modules, [mineral_app, mineral_supervisor]}, 
{registered,[]}, 
{applications, [kernel,stdlib]}, 
{mod, {mineral_app,[]}}, 
{start_phases, []} 
]}.
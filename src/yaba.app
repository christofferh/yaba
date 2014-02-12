{application, yaba,
	[{description,  "Yaba PubSub"},
	{vsn,          "1.0"},
	{modules,      [node_db, msg_db, node_sup, node_netbuilder_sup, node_netbuilder_serv, node_dist_serv, node_ipc_serv]},
	{registered,   [node_netbuilder_serv, node_dist_serv, node_ipc_serv]},
	{applications, [kernel, stdlib]},
	{mod,          {node_app,[]}}]}.
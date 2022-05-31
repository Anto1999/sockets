{application, chat_system,[
	{description,"Simple_chat"},
	{vsn,"1"},
	{modules,[chat_client,chat_server,chat_supervisor,chat_system,chat_operator,db]},
	{registered,[]},
	{applications,[kernel,stdlib]},
	{mod, {chat_system,[]}},
	{env,[]}
]}.
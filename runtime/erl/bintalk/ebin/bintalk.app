{application, bintalk, [
	{description, "bintalk erlang runtime library."},
	{vsn, "1.0.0"},
	{modules, [
		bintalk_prot_reader,
		bintalk_prot_writer
		]},
	{registered, []},
	{applications, [kernel, stdlib]},
	{env, []}
]}.

:- initialization((
	logtalk_load(loader),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	lgtunit::run_test_sets([
		test_config,
		test_file,
		test_prolog_file,
		test_list_extras,
		test_term_extras,
		test_term_file_index,
		test_reporting,
		test_banpipe_module_path,
		test_module,
		test_module_task,
		test_invoke_task,
		test_invoke_task_custom,
		test_task_typecheck1,
		test_typecheck_builtin_file,
		test_scheduler_tree,
		test_uri,
		test_builtin_module_file
	])
)).

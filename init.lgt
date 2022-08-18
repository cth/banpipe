
banpipe_required_libraries([
	basic_types(loader),
	sets(loader),
	metapredicates(loader),
	dates(loader),
	meta_compiler(loader),
	os(loader)
]).

banpipe_own_files([
	banpipe(shell),
	banpipe(config),
	banpipe(uri),
	banpipe(file),
	banpipe(list_extras),
	banpipe(term_extras),
	banpipe(reporting),
	banpipe(file_index),
	banpipe(builtin_modules),
	banpipe(module),
	banpipe(invokers),
	banpipe(parser),
	banpipe(interpreter),
	banpipe(sequential_interpreter),
	banpipe(task_semantics),
	banpipe(scheduler_tree),
	banpipe(scheduler),
	banpipe(banpipe)
]).

banpipe_file_options([hook(meta_compiler), optimize(on)]).

:- initialization((
	banpipe_required_libraries(Libraries),
	logtalk_load(Libraries),
	banpipe_own_files(Files),
	banpipe_file_options(Options),
	logtalk_load(Files, Options),
	banpipe_module_path::check_set
)).

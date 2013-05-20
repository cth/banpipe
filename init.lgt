
banpipe_required_libraries([
	library(termp),
	library(term),
	library(compound),
	library(listp),
	library(list),
	library(setp),
	library(set),
	library(metap),
	library(meta),
	library(timep),
	library(time),
	library(datep),
	library(date),
	library(gensym),
	library(meta_compiler)
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
	banpipe(events),
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

:- if(current_logtalk_flag(version, version(3,_,_))).
	banpipe_file_options([hook(meta_compiler), optimize(on)]).
:- else.
	banpipe_file_options([hook(meta_compiler), optimize(on), reload(skip)]).
:- endif.

:- initialization((
	banpipe_required_libraries(Libraries),
	logtalk_load(Libraries),
	banpipe_own_files(Files),
	banpipe_file_options(Options),
	logtalk_load(Files, Options),
	banpipe_module_path::check_set
)).

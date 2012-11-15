:- object(test_config, extends(lgtunit)).
	:- initialization(::run).

	succeeds(get_defaults) :-
		config::get(execution_mode,sequential),
		config::get(default_invoker,prism),
		config::get(result_file_directory,_),
		config::get(index_file,_),
		config::get(file_manager,_).
:- end_object.

:- object(test_file, extends(lgtunit)).	
	:- initialization(::run).

	succeeds(write_and_read) :-
		Contents = "This is a test\nwith multiple\nlines.\n",
		file('/tmp/test-file')::write(Contents),
		file('/tmp/test-file')::read(Contents).

	succeeds(exists_1) :-
		file('/tmp/test-file')::write("test 123"),
		file('/tmp/test-file')::exists.

	fails(exists_2) :-
		file('/tmp/no-such-file')::exists.
		
	succeeds(dirname) :-
		file('/path/to/somefile')::dirname('/path/to/').
		
	succeeds(touch_exists) :-
		file('/tmp/touchme')::touch,
		file('/tmp/touchme')::exists.
		
	succeeds(touch_delete) :-
		file('/tmp/touchme')::touch,
		file('/tmp/touchme')::delete.
	
	fails(touch_delete_exists) :-
		file('/tmp/touchme')::touch,
		file('/tmp/touchme')::delete,
		file('/tmp/touchme')::exists.
:- end_object.

:- object(test_prolog_file, extends(lgtunit)).
	:- initialization(::run).
	
	succeeds(write_and_read) :-
		Contents = "This is a test\nwith multiple\nlines.\n",
		prolog_file('/tmp/test-file')::write(Contents),
		prolog_file('/tmp/test-file')::read(Contents).
	
	succeeds(write_and_read_terms) :-
		Terms = [a(1),b('2'),c("3")],
		prolog_file('/tmp/prolog-test-file')::write_terms(Terms),
		prolog_file('/tmp/prolog-test-file')::read_terms(Terms).
		
	succeeds(member) :-
		Terms = [a(1),b('2'),c("3")],
		prolog_file('/tmp/prolog-test-file')::write_terms(Terms),
		forall(list::member(T,Terms), prolog_file('/tmp/prolog-test-file')::member(T)).
		
	succeeds(select) :-
		Terms = [a(1),b('2'),c("3")],
		prolog_file('/tmp/prolog-test-file')::write_terms(Terms),
		forall(list::select(T,Terms,TermsRest), prolog_file('/tmp/prolog-test-file')::select(T,TermsRest)).
		
	succeeds(append) :-
		Terms = [a(1),b('2'),c("3")],
		list::append(Terms,Terms,T2),
		prolog_file('/tmp/prolog-test-file')::write_terms(Terms),
		prolog_file('/tmp/prolog-test-file')::append(Terms),
		prolog_file('/tmp/prolog-test-file')::read_terms(T2).
:- end_object.

:- object(test_list_extras, extends(lgtunit)).
	:- initialization(::run).
	
	succeeds(intersperse1) :-
		list_extras::intersperse(',', ['a','b','c'], ['a',',','b',',','c']).
		
	succeeds(intersperse2) :-
		list_extras::intersperse(X, ['a','b','c'], ['a',',','b',',','c']),
		X == ','.
		
	succeeds(intersperse3) :-
		list_extras::intersperse(',', X, ['a',',','b',',','c']),
		X == [a,b,c].
		
	succeeds(sublist_split1) :-
		list_extras::sublist_split(':',[a,':',b,c,':',d], [[a],[b,c],[d]]).
:- end_object.

:- object(test_term_manipulation, extends(lgtunit)).
	:- initialization(::run).
	
	succeeds(atom_integer1) :-
		term_manipulation::atom_integer('123',X),
		X == 123.
		
	succeeds(atom_integer2) :-
		term_manipulation::atom_integer(X,123),
		X == '123'.
		
	succeeds(term_to_atom1) :-
		term_manipulation::term_to_atom(a(b(c,d(1,f))), 'a(b(c,d(1,f)))').
:- end_object.

:- object(test_term_file_index, extends(lgtunit)).
	:- initialization(::run).
	
	index_file('/tmp/test-file-index').

	succeeds(allocate0) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[Out1,Out2]),
		ground(Out1),
		ground(Out2),
		Out1 \= Out2.
		
	succeeds(allocate_commit0) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		term_file_index(FI)::result_files_commit('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]).
		
	succeeds(allocate_rollback) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		term_file_index(FI)::result_files_rollback('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]).

	fails(allocate_commit_rollback) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files_rollback('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]).

	fails(allocate_rollback_commit) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_rollback('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]).
		
	succeeds(allocate_commmit_results) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[Out1,Out2]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],ResultFiles),
		ResultFiles == [Out1,Out2].
		
	succeeds(allocate_allocate_time) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_allocate_time('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],time(AYear,AMon,ADay,AHour,AMin,ASec)),
		date::valid(AYear,AMon,ADay),
		time::valid(AHour,AMin,ASec).
		
	fails(allocate_commit_time) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_commit_time('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],_).
				
	succeeds(allocate_commit_alloctime) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files_allocate_time('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],time(AYear,AMon,ADay,AHour,AMin,ASec)),
		!,
		term_file_index(FI)::result_files_commit_time('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],time(CYear,CMon,CDay,CHour,CMin,CSec)),
		date::valid(AYear,AMon,ADay),
		time::valid(AHour,AMin,ASec),
		date::valid(CYear,CMon,CDay),
		time::valid(CHour,CMin,CSec).
:- end_object.

:- object(test_reporting, extends(lgtunit)).
	:- initialization(::run).

	succeeds(test_throw_if) :- 
		catch(throw_if(true)::error(blah), blah,true).
		
	succeeds(test_throw_unless) :-
		catch(throw_unless(false)::error(blah),blah,true).
		
	succeeds(test_throw_if_warning) :- 
		tell('/tmp/warning'),
		throw_if(true)::warning(blah),
		told,
		file('/tmp/warning')::read(Warning),
		list::member(NewLine,[[10],[10,13]]),
		atom_codes(blah,Blah),
		list::append(Blah,NewLine,Warning).

	succeeds(test_throw_unless_warning) :- 
		tell('/tmp/warning'),
		throw_if(false)::warning(blah),
		told,
		file('/tmp/warning')::read(Warning),
		list::member(NewLine,[[10],[10,13]]),
		atom_codes(blah,Blah),
		list::append(Blah,NewLine,Warning).
:- end_object.

:- object(test_shell, extends(lgtunit)).
	:- initialization(::run).
	
	succeeds(execute_simple_cmd) :-
		shell::exec('echo "hello from testcase"').
		
	succeeds(execute_simple_cmd_with_status) :-
		shell::exec('echo "hello from testcase"',X),
		X == 0.
		
	fails(execute_non_exist_cmd_with_status) :-
		shell::exec('this_command_is_bogus',X),
		X == 0.
		
	succeeds(evironment_variable) :-
		shell::environment_variable('PATH',_).
		
	fails(environment_variable_nonexisting) :-
		shell::environment_variable('NO_SUCH_VARIABLE','').

	% FIXME: More test-cases needed for shell object
:- end_object.


:- object(test_banpipe_module_path, extends(lgtunit)).
	:- initialization(::run).
	succeeds(get_module_paths) :-
		TestDirs = ['/tmp/mod1','/tmp/mod2'],
		forall(list::member(Dir,TestDirs),banpipe_module_path::include_directory(Dir)),
		banpipe_module_path::get_paths(SearchDirectories),
		forall(list::member(TD,TestDirs),list::member(TD,SearchDirectories)).
:- end_object.

:- object(test_module, extends(lgtunit)).
	:- initialization(::run).
	
	setup :-
		shell::exec('rm -rf /tmp/testmodule').
	
	succeeds(interface_file) :-
		shell::make_directory('/tmp/testmodule'),
		file('/tmp/testmodule/interface.pl')::touch,
		banpipe_module_path::include_directory('/tmp'),
		module(testmodule)::interface_file('/tmp/testmodule/interface.pl'),
		file('/tmp/testmodule/interface.pl')::delete,
		shell::delete_directory('/tmp/testmodule').
:- end_object.

:- object(test_module_task, extends(lgtunit)).
	:- initialization(::run).

	setup :-
		InterfaceFileContentsLines = [
		% Task declaration
		":- task(test([test(in_type1),test(in_type2)], [version(1.0),debug(true)], [out_type(1),out_type(2)])).\n",
		% (dummy) Task implementation
		"test(In,Opt,Out) :- true.\n"
		],
		list::flatten(InterfaceFileContentsLines,InterfaceFileContents),
		shell::exec('rm -rf /tmp/testmodule'),
		shell::make_directory('/tmp/testmodule'),
		file('/tmp/testmodule/interface.pl')::write(InterfaceFileContents).
		
	succeeds(task_options) :-
		module_task(testmodule,test)::options(Options),
		list::member(version(1.0), Options),
		list::member(debug(true), Options).

	succeeds(task_input_types) :-
		module_task(testmodule,test)::input_types(InputTypes),
		list::member(test(in_type1),InputTypes),
		list::member(test(in_type2),InputTypes).
		
	succeeds(task_output_types) :-
		module_task(testmodule,test)::output_types(OutputTypes),
		list::member(out_type(1),OutputTypes),
		list::member(out_type(2),OutputTypes).
	
	succeeds(valid_task_call) :-
		module_task(testmodule,test)::valid([version(1.0),debug(true)]).

	succeeds(expand_options1) :-
		module_task(testmodule,test)::expand_options([], [debug(true),version(1.0)]).

	succeeds(expand_options2) :-
		module_task(testmodule,test)::expand_options([version(2.0)], [debug(true),version(2.0)]).
:- end_object.

:- object(test_invoke_task,extends(lgtunit)).
	:- initialization(::run).

	setup :-
		InterfaceFileContentsLines = [
		% Task declaration
		":- task(test([test(in_type1),test(in_type2)], [version(1.0),debug(true)], [out_type(1),out_type(2)])).\n",
		% (dummy) Task implementation
		"test(In,Opt,[Out1,Out2]) :- tell(Out1), writeln(file1), told, tell(Out2), writeln(file2), told.\n"
		],
		list::flatten(InterfaceFileContentsLines,InterfaceFileContents),
		shell::exec('rm -rf /tmp/testmodule'),
		shell::make_directory('/tmp/testmodule'),
		file('/tmp/testmodule/interface.pl')::write(InterfaceFileContents),
		banpipe_module_path::include_directory('/tmp'),
		writeln('setup finished').
	
	succeeds(invoke_task1) :-
		task(testmodule,test,[],[])::run([F1,F2]),
		writeln(ran_task),
		file(F1)::read("file1"),
		file(F2)::read("file2").
:- end_object.
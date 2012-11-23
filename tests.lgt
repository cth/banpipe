:- object(test_config, extends(lgtunit)).
	:- initialization(::run).

	succeeds(get_defaults) :-
		config::get(execution_mode,sequential),
		config::get(default_invoker,prism),
		config::get(result_file_directory,_),
		config::get(index_file,_),
		config::get(file_manager,_).
		
	succeeds(push_and_pop) :-
		config::push(test,1),
		config::get(test,1),
		config::push(test,2),
		config::get(test,2),
		config::pop(test),
		config::get(test,1).
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
		
	succeeds(copy_to) :-
		From = file('/tmp/somefile'),
		To = '/tmp/otherfile',
		From::write("test"),
		From::copy_to(To),
		file(To)::read("test"),
		From::delete,
		file(To)::delete.

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

:- object(test_term_extras, extends(lgtunit)).
	:- initialization(::run).
	
	succeeds(atom_integer1) :-
		term_extras::atom_integer('123',X),
		X == 123.
		
	succeeds(atom_integer2) :-
		term_extras::atom_integer(X,123),
		X == '123'.
		
	succeeds(vars) :-
		term_extras::vars([A,B,C],[A,B,C]),
		term_extras::vars(a([A,B],C),[A,B,C]),
		term_extras::vars(a(b(c)),[]),
		term_extras::vars([A,A,A],A).
		
	succeeds(term_to_atom1) :-
		term_extras::term_to_atom(a(b(c,d(1,f))), 'a(b(c,d(1,f)))').
		
	succeeds(term_to_atom2) :-
		term_extras::term_to_atom([a,b,c],'[a,b,c]').
		
	succeeds(term_to_atom3) :-
		term_extras::term_to_atom('_a','\'_a\'').

	succeeds(term_to_atom4) :-
		term_extras::term_to_atom(a(b(c(d(_,_)))),'a(b(c(d(V1,V2))))').
		
	succeeds(term_to_atom5) :-
		term_extras::term_to_atom(a(b(c(d(X,X)))),'a(b(c(d(V1,V1))))').
		
	succeeds(term_to_atom6) :-
		term_extras::term_to_atom([],'[]').
		
	succeeds(conjunction_as_list) :-
		term_extras::conjunction_as_list((a,b,c,d),[a,b,c,d]).
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

	succeeds(test_report_if) :- 
		catch(report_if(true)::error(blah), blah,true).
		
	succeeds(test_report_unles) :-
		catch(report_unless(false)::error(blah),blah,true).
		
	succeeds(test_report_if_warning) :- 
		tell('/tmp/warning'),
		report_if(true)::warning(blah),
		told,
		file('/tmp/warning')::read(Warning),
		list::member(NewLine,[[10],[10,13]]),
		atom_codes(blah,Blah),
		list::append(Blah,NewLine,Warning).

	succeeds(test_report_unles_warning) :- 
		tell('/tmp/warning'),
		report_if(false)::warning(blah),
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

	succeeds(make_change_change_delete) :-
	    D=testdir42,
	    shell::working_directory(Current),
	    shell::make_directory(D),
	    shell::change_directory(D),
	    shell::change_directory(Current),
	    shell::delete_directory(D).


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
		(file('/tmp/index-file')::exists ->
			file('/tmp/index-file')::delete
			;
			true).

	task_setup :-
		InterfaceFileContentsLines = [
		% Task declaration
		":- task(test([test(in_type1),test(in_type2)], [version(1.0),debug(true)], [out_type(1),out_type(2)])).\n",
		% (dummy) Task implementation
		"test(In,Opt,[Out1,Out2]) :- write('hello from prolog'), tell(Out1), write(file1), told, tell(Out2), write(file2), told.\n"
		],
		list::flatten(InterfaceFileContentsLines,InterfaceFileContents),
		shell::exec('rm -rf /tmp/testmodule'),
		shell::make_directory('/tmp/testmodule'),
		file('/tmp/testmodule/interface.pl')::write(InterfaceFileContents),
		banpipe_module_path::include_directory('/tmp'),
		config::push(result_file_directory,'/tmp/'),
		config::push(index_file,'/tmp/index-file'),
		config::push(file_manager,term_file_index('/tmp/index-file')).
	
	succeeds(invoke_task_default) :-
		task_setup,
		invoke_task,
		task_cleanup.
		
	succeeds(invoke_task_prism) :-
		task_setup,		
		config::push(default_invoker,prism_invoker),
		invoke_task,
		task_cleanup.
		
	succeeds(invoke_task_bp) :-
		task_setup,
		config::push(default_invoker,bp_invoker),
		invoke_task,
		task_cleanup.
		
	succeeds(invoke_task_swipl) :-
		task_setup,
		config::push(default_invoker,swipl_invoker),
		invoke_task,
		task_cleanup.
		
	succeeds(invoke_task_yap) :-
		task_setup,
		config::push(default_invoker,yap_invoker),
		invoke_task,
		task_cleanup.

	/* FIXME: invocation on gprolog needs attention
	succeeds(invoke_task_gprolog) :-
		task_setup,
		config::push(default_invoker,gprolog_invoker),
		invoke_task,
		task_cleanup.
	*/
	
	
				
	:- private(invoke_task/0).
	invoke_task :-
		writeln(task(testmodule,test,[],[])::run([F1,F2])),
		task(testmodule,test,[],[])::run([F1,F2]),
		file(F1)::read("file1"),
		file(F2)::read("file2"),
		file(F1)::delete,
		file(F2)::delete.
				
	task_cleanup :-
		config::setup_defaults,
		file('/tmp/index-file')::delete.
:- end_object.


:- object(test_invoke_task_custom,extends(lgtunit)).
	:- initialization(::run).
	
	% Just in case the previous test didn't finish well and forgot cleanup of file
	setup :-
		(file('/tmp/index-file')::exists ->
			file('/tmp/index-file')::delete
			;
			true).

	task_setup(PrologName) :-
		InterfaceFileContentsLines = [
		% Task declaration
		":- task(test([test(in_type1),test(in_type2)], [version(1.0),debug(true)], [out_type(1),out_type(2)])).\n",
		":- invoke_with(", PrologName, ").\n",
		% (dummy) Task implementation
		"test(_,_,[Out1,Out2]) :- write('hello from prolog'), tell(Out1), write(file1), told, tell(Out2), write(file2), told.\n"
		],
		list::flatten(InterfaceFileContentsLines,InterfaceFileContents),
		shell::exec('rm -rf /tmp/testmodule'),
		shell::make_directory('/tmp/testmodule'),
		file('/tmp/testmodule/interface.pl')::write(InterfaceFileContents),
		%shell::exec('cat /tmp/testmodule/interface.pl'),
		banpipe_module_path::include_directory('/tmp'),
		config::push(result_file_directory,'/tmp/'),
		config::push(index_file,'/tmp/index-file'),
		config::push(file_manager,term_file_index('/tmp/index-file')).
	
	succeeds(invoke_task_prism) :-
		task_setup("prism"),
		invoke_task,
		task_cleanup.
		
	succeeds(invoke_task_bp) :-
		task_setup("bp"),
		invoke_task,
		task_cleanup.
		
	succeeds(invoke_task_swipl) :-
		task_setup("swipl"),
		invoke_task,
		task_cleanup.
		
	succeeds(invoke_task_yap) :-
		task_setup("yap"),
		invoke_task,
		task_cleanup.
		
	/* FIXME: invocation on gprolog needs attention
	succeeds(invoke_task_gprolog) :-
		task_setup(gprolog),
		config::push(default_invoker,gprolog_invoker),
		invoke_task,
		task_cleanup.
	*/
				
	:- private(invoke_task/0).
	invoke_task :-
		writeln(task(testmodule,test,[],[])::run([F1,F2])),
		task(testmodule,test,[],[])::run([F1,F2]),
		file(F1)::read("file1"),
		file(F2)::read("file2"),
		file(F1)::delete,
		file(F2)::delete.
				
	task_cleanup :-
		config::setup_defaults,
		file('/tmp/index-file')::delete.
:- end_object.

:- object(test_task_typecheck1,extends(lgtunit)).
	:- initialization(::run).
	
	setup :-
		InterfaceFileContentsLines = [
		":- task(test([_], [filetype(X)], [X])).\n",
		"test([InFile],_Options,[OutFile]) :- atom_concat('cp ',InFile,T1),atom_concat(T1,' ',T2),atom_concat(T2,OutFile,Cmd),system(Cmd).\n"
		],
		list::flatten(InterfaceFileContentsLines,InterfaceFileContents),
		shell::exec('rm -rf /tmp/testmodule'),
		shell::make_directory('/tmp/testmodule'),
		file('/tmp/testmodule/interface.pl')::write(InterfaceFileContents),
		banpipe_module_path::include_directory('/tmp').

	succeeds(type_check1) :-
		task(testmodule,test,[type(blah)],[filetype(test(type))])::typecheck([OutputType]),
		OutputType == test(type).
		
	fails(type_check2) :-
		task(testmodule,test,[wrong,number,of,inputs],[filetype(test(type))])::typecheck([OutputType]),
		OutputType == test(type).

	cleanup :-
		config::setup_defaults.
:- end_object.

:- object(test_typecheck_builtin_file,extends(lgtunit)).
	:- initialization(::run).
	
	succeeds(type_check1) :-
		task(file,get,[type(blah)],[filetype(test(type))])::typecheck([OutputType]),
		OutputType == test(type).
	
	% No filetype option supplied
	succeeds(type_check2) :-
		task(file,get,[type(blah)],[])::typecheck([OutputType]),
		var(OutputType).
:- end_object.


:- object(test_scheduler_tree,extends(lgtunit)).
	:- initialization(::run).
	
	
	/*
	+1 ready module1::task1
	|--+3 ready module3::task3
	|--+2 ready module2::task2
	*/
	make_small_tree(Tree4) :-
		scheduler_tree::create(Tree1),
		scheduler_tree::add(module1,task1,nil,Tree1,Tree2,TaskId1),
		scheduler_tree::add(module2,task2,TaskId1,Tree2,Tree3,_TaskId2),
		scheduler_tree::add(module3,task3,TaskId1,Tree3,Tree4,_TaskId3).

	/* 
	+1 ready module1::task1
	|--+6 ready module5::task5
	   |--+8 ready module7::task7
	|--+2 ready module2::task2
	   |--+3 ready module3::task3
	      |--+5 ready module4::task4
	      |--+4 ready module3::task3
	         |--+9 ready module7::task7
	         |--+7 ready module6::task6
	*/
	make_large_tree(Tree10) :-
		scheduler_tree::create(Tree1),
		scheduler_tree::add(module1,task1,nil,Tree1,Tree2,TaskId1),
		scheduler_tree::add(module2,task2,TaskId1,Tree2,Tree3,TaskId2),
		scheduler_tree::add(module3,task3,TaskId2,Tree3,Tree4,TaskId3),
		scheduler_tree::add(module3,task3,TaskId3,Tree4,Tree5,TaskId4),
		scheduler_tree::add(module4,task4,TaskId3,Tree5,Tree6,_TaskId5),
		scheduler_tree::add(module5,task5,TaskId1,Tree6,Tree7,TaskId6),
		scheduler_tree::add(module6,task6,TaskId4,Tree7,Tree8,_TaskId7),
		scheduler_tree::add(module7,task7,TaskId6,Tree8,Tree9,TaskId8),
		scheduler_tree::add(module7,task7,TaskId4,Tree9,Tree10,_TaskId9).
	
	succeeds(make_small_tree) :-
		make_small_tree(T),
		scheduler_tree::print(T).
		
	succeeds(make_large_tree) :-
		make_large_tree(T),
		scheduler_tree::print(T).

	succeeds(test_ready) :-
		make_large_tree(T1),
		findall(TaskId,scheduler_tree::ready_task(T1,TaskId),AllReady),
		sort(AllReady,[5,7,8,9]).

	% After removing the root element, the tree should be empty.
	succeeds(test_remove) :-
		make_small_tree(T),
		scheduler_tree::remove(1,T,T2),
		scheduler_tree::empty(T2).
	
	succeeds(test_replace_node) :-
		make_small_tree(T),
		scheduler_tree::lookup(2,T,SubTree1),
		SubTree1 = [node(Id,State,_,_,Children)],
		SubTree1_new = node(Id,State,replaced_module,replace_goal,Children),
		scheduler_tree::replace(SubTree1_new,T,T2), % replace it
		T \= T2, % It is changed
		scheduler_tree::replace(Subtree1,T2,T). % Replace it again with original subtree
		
	succeeds(test_set_running1) :-
		make_small_tree(T1),
		scheduler_tree::set_running(2,T1,T2),
		scheduler_tree::lookup(2,T2,[node(2,running,_,_,_)]).
			
	fails(test_set_running2) :-
		make_small_tree(T1),
		scheduler_tree::set_running(4,T1,_). % there is no task with id 4
		
	fails(test_set_running_twice) :-
		make_small_tree(T1),
		scheduler_tree::set_running(2,T1,T2),
		scheduler_tree::set_running(2,T2,_).

	succeeds(test_set_completed) :-
		make_small_tree(T1),
		scheduler_tree::set_running(2,T1,T2),
		scheduler_tree::set_completed(2,T2,T3).
		
	fails(test_set_completed_write) :-
		make_small_tree(T1),
		scheduler_tree::set_running(2,T1,T2),
		scheduler_tree::set_completed(2,T2,T3),
		scheduler_tree::set_completed(2,T3,_).

	succeeds(test_complete_all) :-
		make_small_tree(T1),
		scheduler_tree::set_running(3,T1,T2),
		scheduler_tree::set_completed(3,T2,T3),
		scheduler_tree::set_running(2,T3,T4),
		scheduler_tree::set_completed(2,T4,T5),
		scheduler_tree::set_running(1,T5,T6),
		scheduler_tree::set_completed(1,T6,T7),
		scheduler_tree::empty(T7).
:- end_object.

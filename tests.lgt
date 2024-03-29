
:- set_prolog_flag(double_quotes, codes).


:- object(test_config, extends(lgtunit)).

	succeeds(get_defaults) :-
		config::get(execution_mode,sequential),
		config::get(default_invoker,prism_invoker),
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

	succeeds(write_and_read) :-
		atom_codes('This is a test\nwith multiple\nlines.\n', Contents),
		file('$HOME/tmp/test-file')::write(Contents),
		file('$HOME/tmp/test-file')::read(Contents).

	succeeds(exists_1) :-
		atom_codes('test 123', Contents),
		file('$HOME/tmp/test-file')::write(Contents),
		file('$HOME/tmp/test-file')::exists.

	fails(exists_2) :-
		file('$HOME/tmp/no-such-file')::exists.

	succeeds(dirname) :-
		file('/path/to/somefile')::dirname('/path/to/').

	succeeds(touch_exists) :-
		file('$HOME/tmp/touchme')::touch,
		file('$HOME/tmp/touchme')::exists.

	succeeds(touch_delete) :-
		file('$HOME/tmp/touchme')::touch,
		file('$HOME/tmp/touchme')::delete.

	fails(touch_delete_exists) :-
		file('$HOME/tmp/touchme')::touch,
		file('$HOME/tmp/touchme')::delete,
		file('$HOME/tmp/touchme')::exists.

	succeeds(copy_to) :-
		From = file('$HOME/tmp/somefile'),
		To = '$HOME/tmp/otherfile',
		From::write("test"),
		From::copy_to(To),
		file(To)::read("test"),
		From::delete,
		file(To)::delete.

	succeeds(canonical_normal_file) :-
		file('$HOME/tmp//file')::canonical('$HOME/tmp/file').

	succeeds(canonical_windows_file) :-
		file('c:\\some\\file')::canonical('c:/some/file').

	succeeds(canonical_url) :-
		file('http://banpipe.org//index.html')::canonical('http://banpipe.org/index.html').

:- end_object.


:- object(test_prolog_file, extends(lgtunit)).

	succeeds(write_and_read) :-
		atom_codes('This is a test\nwith multiple\nlines.\n', Contents),
		os::absolute_file_name('$HOME/tmp/test-file', File),
		prolog_file(File)::write(Contents),
		prolog_file(File)::read(Contents).

	succeeds(write_and_read_terms) :-
		Terms = [a(1),b('2'),c("3")],
		os::absolute_file_name('$HOME/tmp/prolog-test-file', File),
		prolog_file(File)::write_terms(Terms),
		prolog_file(File)::read_terms(Terms).

	succeeds(member) :-
		Terms = [a(1),b('2'),c("3")],
		os::absolute_file_name('$HOME/tmp/prolog-test-file', File),
		prolog_file(File)::write_terms(Terms),
		forall(list::member(T,Terms), prolog_file(File)::member(T)).

	succeeds(select) :-
		Terms = [a(1),b('2'),c("3")],
		os::absolute_file_name('$HOME/tmp/prolog-test-file', File),
		prolog_file(File)::write_terms(Terms),
		forall(list::select(T,Terms,TermsRest), prolog_file(File)::select(T,TermsRest)).

	succeeds(append) :-
		Terms = [a(1),b('2'),c("3")],
		list::append(Terms,Terms,T2),
		os::absolute_file_name('$HOME/tmp/prolog-test-file', File),
		prolog_file(File)::write_terms(Terms),
		prolog_file(File)::append(Terms),
		prolog_file(File)::read_terms(T2).

:- end_object.


:- object(test_list_extras, extends(lgtunit)).

	succeeds(intersperse1) :-
		list_extras::intersperse(',', ['a','b','c'], ['a',',','b',',','c']).

	succeeds(intersperse2) :-
		list_extras::intersperse(X, ['a','b','c'], ['a',',','b',',','c']),
		X == (',').

	succeeds(intersperse3) :-
		list_extras::intersperse(',', X, ['a',',','b',',','c']),
		X == [a,b,c].

	succeeds(sublist_split1) :-
		list_extras::sublist_split(':',[a,':',b,c,':',d], [[a],[b,c],[d]]).

:- end_object.


:- object(test_term_extras, extends(lgtunit)).

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

	index_file(File) :-
		os::absolute_file_name('$HOME/tmp/test-file-index', File).

	succeeds(allocate0) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[Out1,Out2]),
		ground(Out1),
		ground(Out2),
		Out1 \= Out2.

	succeeds(allocate_commit0) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		term_file_index(FI)::result_files_commit('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)]).

	succeeds(allocate_rollback) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		term_file_index(FI)::result_files_rollback('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)]).

	fails(allocate_commit_rollback) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files_rollback('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)]).

	fails(allocate_rollback_commit) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_rollback('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)]).

	succeeds(allocate_commmit_results) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[Out1,Out2]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],ResultFiles),
		ResultFiles == [Out1,Out2].

	succeeds(allocate_allocate_time) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_allocate_time('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],time(AYear,AMon,ADay,AHour,AMin,ASec)),
		date::valid(AYear,AMon,ADay),
		time::valid(AHour,AMin,ASec).

	fails(allocate_commit_time) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_commit_time('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],_).
	
	succeeds(allocate_commit_alloctime) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],[_,_]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files_allocate_time('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],time(AYear,AMon,ADay,AHour,AMin,ASec)),
		!,
		term_file_index(FI)::result_files_commit_time('module','task',['$HOME/tmp/1','$HOME/tmp/2'],[opt1(a),opt2(b)],time(CYear,CMon,CDay,CHour,CMin,CSec)),
		date::valid(AYear,AMon,ADay),
		time::valid(AHour,AMin,ASec),
		date::valid(CYear,CMon,CDay),
		time::valid(CHour,CMin,CSec).

:- end_object.


:- object(test_reporting, extends(lgtunit)).

	succeeds(test_report_if) :- 
		catch(report_if(true)::error(blah), blah,true).

	succeeds(test_report_unles) :-
		catch(report_unless(false)::error(blah),blah,true).

	succeeds(test_report_if_warning) :-
		os::absolute_file_name('$HOME/tmp/warning', File),
		tell(File),
		report_if(true)::warning(blah),
		told,
		file(File)::read(Warning),
		atom_codes('WARNING: blah\n',Warning).

	succeeds(test_report_unless_warning) :-
		os::absolute_file_name('$HOME/tmp/warning', File),
		tell(File),
		report_if(false)::warning(blah),
		told,
		file(File)::read([]).

:- end_object.


:- object(test_banpipe_module_path, extends(lgtunit)).

	succeeds(get_module_paths) :-
		os::absolute_file_name('$HOME/tmp/mod1', Mod1),
		os::absolute_file_name('$HOME/tmp/mod2', Mod2),
		TestDirs = [Mod1,Mod2],
		forall(list::member(Dir,TestDirs),banpipe_module_path::include_directory(Dir)),
		banpipe_module_path::get_paths(SearchDirectories),
		forall(list::member(TD,TestDirs),list::member(TD,SearchDirectories)).

:- end_object.


:- object(test_module, extends(lgtunit)).

	setup :-
		os::shell('rm -rf $HOME/tmp/testmodule').

	succeeds(interface_file) :-
		os::make_directory('$HOME/tmp/testmodule'),
		os::absolute_file_name('$HOME/tmp/testmodule/interface.pl', Interface),
		file(Interface)::touch,
		os::absolute_file_name('$HOME/tmp', Directory),
		banpipe_module_path::include_directory(Directory),
		module(testmodule)::interface_file(Interface, _),
		file(Interface)::delete,
		os::delete_directory('$HOME/tmp/testmodule').

:- end_object.


:- object(test_module_task, extends(lgtunit)).

	setup :-
		% Task declaration
		atom_codes(':- task(test([test(in_type1),test(in_type2)], [version(1.0),debug(true)], [out_type(1),out_type(2)])).\n', Line1),
		% (dummy) Task implementation
		atom_codes('test(In,Opt,Out) :- true.\n', Line2),
		list::append(Line1, Line2, InterfaceFileContents),
		os::shell('rm -rf $HOME/tmp/testmodule'),
		os::make_directory('$HOME/tmp/testmodule'),
		file('$HOME/tmp/testmodule/interface.pl')::write(InterfaceFileContents).

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

	setup :-
		(	file('$HOME/tmp/index-file')::exists ->
			file('$HOME/tmp/index-file')::delete
		;	true
		).

	task_setup :-
		% Task declaration
		atom_codes(':- task(test([test(in_type1),test(in_type2)], [version(1.0),debug(true)], [out_type(1),out_type(2)])).\n', Line1),
		% (dummy) Task implementation
		atom_codes('test(In,Opt,[Out1,Out2]) :- write(\'hello from prolog\'), tell(Out1), write(file1), told, tell(Out2), write(file2), told.\n', Line2),
		list::append(Line1,Line2,InterfaceFileContents),
		os::shell('rm -rf $HOME/tmp/testmodule'),
		os::make_directory('$HOME/tmp/testmodule'),
		file('$HOME/tmp/testmodule/interface.pl')::write(InterfaceFileContents),
		os::absolute_file_name('$HOME/tmp', Directory),
		banpipe_module_path::include_directory(Directory),
		config::push(result_file_directory,Directory),
		os::absolute_file_name('$HOME/tmp/index-file', File),
		config::push(index_file,File),
		config::push(file_manager,term_file_index(File)).

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

	succeeds(invoke_task_lvm) :-
		task_setup,
		config::push(default_invoker,lvm_invoker),
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
		write(task(testmodule,test,[],[])::run([F1,F2])), nl,
		task(testmodule,test,[],[])::run([F1,F2]),
		file(F1)::read("file1"),
		file(F2)::read("file2"),
		file(F1)::delete,
		file(F2)::delete.

	task_cleanup :-
		config::setup_defaults,
		file('$HOME/tmp/index-file')::delete.

:- end_object.


:- object(test_invoke_task_custom,extends(lgtunit)).

	:- uses(user, [
		atomic_list_concat/2
	]).

	% Just in case the previous test didn't finish well and forgot cleanup of file
	setup :-
		(	file('$HOME/tmp/index-file')::exists ->
			file('$HOME/tmp/index-file')::delete
		;	true
		).

	task_setup(PrologName) :-
		% Task declaration
		atom_codes(':- task(test([test(in_type1),test(in_type2)], [version(1.0),debug(true)], [out_type(1),out_type(2)])).\n', Line1),
		atomic_list_concat([':- invoke_with(', PrologName, ').\n'], Atom),
		atom_codes(Atom, Line2),
		% (dummy) Task implementation
		atom_codes('test(_,_,[Out1,Out2]) :- write(\'hello from prolog\'), tell(Out1), write(file1), told, tell(Out2), write(file2), told.\n', Line3),
		list::append([Line1,Line2,Line3],InterfaceFileContents),
		os::shell('rm -rf $HOME/tmp/testmodule'),
		os::make_directory('$HOME/tmp/testmodule'),
		file('$HOME/tmp/testmodule/interface.pl')::write(InterfaceFileContents),
		%os::shell('cat $HOME/tmp/testmodule/interface.pl'),
		os::absolute_file_name('$HOME/tmp', Directory),
		banpipe_module_path::include_directory(Directory),
		config::push(result_file_directory,Directory),
		os::absolute_file_name('$HOME/tmp/index-file', File),
		config::push(index_file,File),
		config::push(file_manager,term_file_index(File)).

	succeeds(invoke_task_prism) :-
		task_setup(prism),
		invoke_task,
		task_cleanup.

	succeeds(invoke_task_bp) :-
		task_setup(bp),
		invoke_task,
		task_cleanup.

	succeeds(invoke_task_swipl) :-
		task_setup(swipl),
		invoke_task,
		task_cleanup.

	succeeds(invoke_task_yap) :-
		task_setup(yap),
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
		write(task(testmodule,test,[],[])::run([F1,F2])), nl,
		task(testmodule,test,[],[])::run([F1,F2]),
		file(F1)::read("file1"),
		file(F2)::read("file2"),
		file(F1)::delete,
		file(F2)::delete.

	task_cleanup :-
		config::setup_defaults,
		file('$HOME/tmp/index-file')::delete.

:- end_object.


:- object(test_task_typecheck1,extends(lgtunit)).

	setup :-
		atom_codes(':- task(test([_], [filetype(X)], [X])).\n', Line1),
		atom_codes('test([InFile],_Options,[OutFile]) :- atom_concat(\'cp \',InFile,T1),atom_concat(T1,\' \',T2),atom_concat(T2,OutFile,Cmd),system(Cmd).\n', Line2),
		list::append(Line1,Line2,InterfaceFileContents),
		os::shell('rm -rf $HOME/tmp/testmodule'),
		os::make_directory('$HOME/tmp/testmodule'),
		file('$HOME/tmp/testmodule/interface.pl')::write(InterfaceFileContents),
		banpipe_module_path::include_directory('$HOME/tmp').

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

	succeeds(type_check1) :-
		task(file,get,[type(blah)],[filetype(test(type))])::typecheck([OutputType]),
		OutputType == test(type).

	% No filetype option supplied
	succeeds(type_check2) :-
		task(file,get,[type(blah)],[])::typecheck([OutputType]),
		var(OutputType).

:- end_object.


:- object(test_scheduler_tree,extends(lgtunit)).


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
		scheduler_tree::set_completed(2,T2,_T3).

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


:- object(test_uri,extends(lgtunit)).

	succeeds(elements) :-
		uri('http://banpipe.org/index.html')::elements('http://','banpipe.org/index.html').

	succeeds(uri_valid_file) :-
		uri('file://$HOME/tmp/blah')::valid.

	succeeds(uri_valid_url) :-
		uri('ftp://user:pass@server.org/dir/file')::valid.

	fails(uri_invalid_file) :-
		uri('/just/a/file')::valid.

:- end_object.


:- object(test_builtin_module_file,extends(lgtunit)).

	succeeds(test_file_get1) :-
		file('$HOME/tmp/test.1')::touch,
		file::get(['file://$HOME/tmp/test.1'],[],['$HOME/tmp/test.2']),
		file('$HOME/tmp/test.1')::delete,
		file('$HOME/tmp/test.2')::delete.

	succeeds(test_file_get2) :-
		os::absolute_file_name('$HOME/tmp/test.banpipe', File),
		file::get(['https://logtalk.org/'],[],[File]),
		file(File)::delete.

:- end_object.

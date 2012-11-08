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
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[Out1,Out2]),
		!,
		term_file_index(FI)::result_files_commit_time('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],_).
				
	succeeds(allocate_commit_alloctime) :-
		index_file(FI),
		file(FI)::delete, % make sure the file does not exists
		term_file_index(FI)::result_files_allocate('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],[Out1,Out2]),
		!,
		term_file_index(FI)::result_files_commit('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)]),
		!,
		term_file_index(FI)::result_files_allocate_time('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],time(AYear,AMon,ADay,AHour,AMin,ASec)),
		!,
		writeln(adter_allocate_time),
		term_file_index(FI)::result_files_commit_time('module','task',['/tmp/1','/tmp/2'],[opt1(a),opt2(b)],time(CYear,CMon,CDay,CHour,CMin,CSec)),
		date::valid(AYear,AMon,ADay),
		time::valid(AHour,AMin,ASec),
		date::valid(CYear,CMon,CDay),
		time::valid(CHour,CMin,CSec).
:- end_object.

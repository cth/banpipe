:- object(scheduler_tree).
    	% The object is used by the process scheduler to infer which tasks can be run in parallel.
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/19,
		comment is 'Tree data structure to represent call graphs of banpipe scripts, i.e. nodes in the tree represent tasks.  Tasks one of three states.',
		remarks is [ 	'state ready' - 'in the tree, but not yet running', 
				'state running' - 'tasks being run, but has not yet completed',
				'state completed' - 'completed tasks are removed from the tree']]).

	:- public(from_trace/2).
	from_trace(Trace,Tree) :-
		::create(EmptyTree),
		::from_trace_rec(nil,Trace,EmptyTree,Tree).
		
	:- private(from_trace_rec/4).

	from_trace_rec(_,[],Tree,Tree).

	from_trace_rec(_,[(nil,_)],Tree,Tree).

	from_trace_rec(Parent,[(task(Module,Task,InputFiles,Options),_),ChildCalls],InTree,OutTree) :-
		Goal =.. [Task,InputFiles,Options],
		::add(Module,Goal,Parent,InTree,OutTree1,TaskId),!,
		::from_trace_rec(TaskId,ChildCalls,OutTree1,OutTree).

	from_trace_rec(Parent,[[(task(Module,Task,InputFiles,Options),_),Children]|Siblings],InTree,OutTree) :-
		Goal =.. [Task,InputFiles,Options],
		::add(Module,Goal,Parent,InTree,Tree1,TaskId),!,
		from_trace_rec(TaskId,Children,Tree1,Tree2),!,
		from_trace_rec(Parent,Siblings,Tree2,OutTree).

	:- public(create/1).
	:- info(create/1,[
		comment is 'Creates an empty scheduler tree (EmptyTree)',
		argnames is ['EmptyTree']]).
	create([0,[]]).

	:- public(add/6).
	:- info(add/6, [
		comment is 'UpdatedTree is Tree with a node inserted as a child to ParentId. NextId is the Id of the newly inserted node. If there are allready an existing node for the same Module and Task, but possibly a different parent, then the new node is given the same task id',
		argnames is ['Module','Task','ParentId','Tree','UpdatedTree','NextId']]).
	add(Module,Task,ParentId,[MaxId,Tree],[NextId,UpdatedTree],NextId) :-
		NextId is MaxId + 1,
		writeln(NextId),
		::scheduler_tree_add_rec(NextId,Module,Task,ParentId,Tree,UpdatedTree).
%		!,
%		::reduce_tree(UpdatedTree1,UpdatedTree).
		
	:- private(scheduler_tree_add_rec/6).

	% This one adds to the top of the tree (e.g. the grand parent)
	scheduler_tree_add_rec(NextId,Module,Task,nil,InTree,[node(NextId,ready,Module,Task,InTree)]).

	scheduler_tree_add_rec(NextId,Module,Goal,ParentId,
			[node(ParentId,ParentState,ParentModule,ParentGoal,Children)],
			[node(ParentId,ParentState,ParentModule,ParentGoal, [node(NextId,ready,Module,Goal,[])|Children])]).

	scheduler_tree_add_rec(NextId,Module,Goal,Parent,
						[node(OtherTaskId,ready,OtherModule,OtherGoal,Children)],
						[node(OtherTaskId,ready,OtherModule,OtherGoal,UpdatedChildren)]) :-
			findall(UpdatedChild,
				(
					list::member(Child,Children),
					(::scheduler_tree_add_rec(NextId,Module,Goal,Parent,[Child],[UpdatedTree]) ->
						UpdatedChild = UpdatedTree
						;
						UpdatedChild = Child)
				),
				UpdatedChildren).

	:- public(remove/3).
	:- info(remove/3, [
		comment is 'Description is the initial tree without the subtrees rooted at the nodes with TaskId',
		argnames is ['TaskId','InitialTree','UpdatedTree']]).

	remove(TaskId,[MaxId,Tree],[MaxId,UpdatedTree]) :-
		::scheduler_tree_remove_rec(TaskId,Tree,UpdatedTree).
		
	:- private(scheduler_tree_remove_rec/3).
	
	scheduler_tree_remove_rec(TaskId,[node(TaskId,_State,_Module,_Goal,_Children)],[]).

	scheduler_tree_remove_rec(TaskId,[node(OtherTaskId,State,Module,Goal,[])],[node(OtherTaskId,State,Module,Goal,[])]) :-
		TaskId \= OtherTaskId.

	scheduler_tree_remove_rec(TaskId,[node(OtherTaskId,State,OtherModule,OtherGoal,Children)],[node(OtherTaskId,State,OtherModule,OtherGoal,UpdatedChildren)]) :-
			list::length(Children,X),
			1 =< X,
			findall(UpdatedChild,
				(
					list::member(Child,Children),
					Child=node(ChildTaskId,_,_,_,_),
					ChildTaskId \= TaskId,
					::scheduler_tree_remove_rec(TaskId,[Child],[UpdatedChild])
				),
				UpdatedChildren).

	%% scheduler_tree_replace(+Node,+Tree,UpdatedTree)
	:- public(replace/3).
	:- info(replace/3, [
		comment is 'Node replaces the subtree of the root node has the same task id as node',
		argnames is ['Node','Tree','UpdatedTree']]).

	replace(Node,[MaxId,Tree],[MaxId,UpdatedTree]) :-
		::scheduler_tree_replace_rec(Node,Tree,UpdatedTree).
		
	:- private(scheduler_tree_replace_rec/3).

	scheduler_tree_replace_rec(node(TaskId,State,Module,Goal,Children),[node(TaskId,_,_,_,_)],[node(TaskId,State,Module,Goal,Children)]).

	scheduler_tree_replace_rec(node(TaskId,_,_,_,_),[node(OtherTaskId,State,Module,Goal,[])],[node(OtherTaskId,State,Module,Goal,[])]) :-
		TaskId \== OtherTaskId.

	scheduler_tree_replace_rec(node(TaskId,State,Module,Goal,Children),
			[node(OtherTaskId,OtherState,OtherModule,OtherGoal,OtherChildren)],
			[node(OtherTaskId,OtherState,OtherModule,OtherGoal,OtherUpdatedChildren)]) :-
		TaskId \== OtherTaskId,
		length(OtherChildren,X),
		1 =< X,
		findall(UpdatedChild,(list::member(Child,OtherChildren),::scheduler_tree_replace_rec(node(TaskId,State,Module,Goal,Children),[Child],[UpdatedChild])),OtherUpdatedChildren).

	%% scheduler_tree_replace_by_taskid(+TaskId,+Node,+Tree,UpdatedTree)
	% Node replaces the subtree of the root node has the same task id as node
	
	:- public(replace_by_taskid/4).
	:- info(replace_by_taskid/4, [
		comment is 'Node replaces the subtree of the root node has the same task id as node',
		argnames is ['TaskId','Node','Tree','UpdatedTree']]).

	replace_by_taskid(TaskId,Node,[MaxId,Tree],[MaxId,UpdatedTree]) :-
		::scheduler_tree_replace_by_taskid_rec(TaskId,Node,Tree,UpdatedTree).
		
	:- private(scheduler_tree_replace_by_taskid_rec/4).

	scheduler_tree_replace_by_taskid_rec(TaskId,UpdatedNode,[node(TaskId,_,_,_,_)],UpdatedNode).

	scheduler_tree_replace_by_taskid_rec(TaskId,_UpdatedNode,[node(OtherTaskId,State,Module,Goal,[])],[node(OtherTaskId,State,Module,Goal,[])]) :-
		TaskId \== OtherTaskId.

	scheduler_tree_replace_by_taskid_rec(TaskId,UpdatedNode,
			[node(OtherTaskId,OtherState,OtherModule,OtherGoal,OtherChildren)],
			[node(OtherTaskId,OtherState,OtherModule,OtherGoal,OtherUpdatedChildren)]) :-
		TaskId \== OtherTaskId,
		list::length(OtherChildren,X),
		1 =< X,
		findall(UpdatedChild,(list::member(Child,OtherChildren),::scheduler_tree_replace_by_taskid_rec(TaskId,UpdatedNode,[Child],[UpdatedChild])),OtherUpdatedChildren).

	%% scheduler_tree_reduce(+Tree,-ReducedTree)
	% This reduces a scheduler tree by compacting nodes which are structurally the same (i.e. only differing in the task id. Such task will be given the same task id
	:- public(reduce_tree/2).
	:- info(reduce_tree/2, [
		comment is 'This reduces a scheduler tree by compacting nodes which are structurally the same (i.e. only differing in the task id. Such task will be given the same task id.',
		argnames is ['Tree','ReducedTree']]).

	reduce_tree(Tree,ReducedTree) :-
		fast_tree_reduce(Tree,ReducedTree).

%	reduce_tree([MaxId,Tree],[NewMaxId,ReducedTree]) :-
%		fast_tree_reduce([MaxId,Tree],[NewMaxId,ReducedTree]).
%		scheduler_tree_reduce_rec([MaxId,Tree],[NewMaxId,ReducedTree]).


  :- private(fast_tree_reduce/2).
	fast_tree_reduce(Tree,ReducedTree) :-
		%writeln(fast_tree_reduce),
		findall([TaskId,[Module,Goal]],::lookup(TaskId,Tree,[node(TaskId,_State,Module,Goal,_Children)]),TaskIds),
		!,
		fast_tree_reduce_rec(TaskIds,Tree,ReducedTree).

  :- private(fast_tree_reduce_rec/2).
	fast_tree_reduce_rec([],Tree,Tree).

	fast_tree_reduce_rec(TaskIds,Tree,UpdTree) :-
		TaskIds=[[FirstId,Task]|_],
%		writeln(partition),
		meta::partition([X]>>(X=[_,Task]),TaskIds,TaskIdsSame,TaskIdsRest),
		!,
%		writeln(same_as_first(TaskIdsSame)),
%		writeln(rest(TaskIdsRest)),
%		writeln(fold_left),
		meta::fold_left([TreeP,TaskNode,MergeTree]>>(TaskNode=[CurrentId,_], ::merge_task_ids(FirstId,CurrentId,TreeP,MergeTree)), Tree, TaskIdsSame, UpdTree1),
		!,
		%::print(UpdTree1),
		fast_tree_reduce_rec(TaskIdsRest,UpdTree1,UpdTree).

	:- private(merge_task_ids/4).
	merge_task_ids(SameId,SameId,SameTree,SameTree).

	merge_task_ids(Id,OtherId,Tree,MergeTree) :-
		write('.'),
		::lookup(OtherId,Tree,[node(OtherId,State,Module,Goal,Children)]),
		::replace_by_taskid(OtherId,[node(Id,State,Module,Goal,Children)],Tree,MergeTree).

	:- private(scheduler_tree_reduce_rec/2).

	scheduler_tree_reduce_rec([MaxId,Tree],[NewMaxId,ReducedTree]) :-
		::scheduler_tree_reduce_once([MaxId,Tree],[NewMaxId1,ReducedTree1]),
		!,
		::scheduler_tree_reduce_rec([NewMaxId1,ReducedTree1],[NewMaxId,ReducedTree]).

	scheduler_tree_reduce_rec([MaxId,Tree],[MaxId,Tree]).
	
	:- private(scheduler_tree_reduce_once/2).

	scheduler_tree_reduce_once([MaxId,Tree],[NewTaskId,ReducedTree]) :-
		::lookup(TaskId1,[MaxId,Tree],Subtree1),
		::lookup(TaskId2,[MaxId,Tree],Subtree2),
		Subtree1 \= Subtree2,
		TaskId1 \= TaskId2,
		Subtree1 = [node(TaskId1,State,Module,Goal,Children1)],
		Subtree2 = [node(TaskId2,State,Module,Goal,Children2)],
		NewTaskId is MaxId + 1,
		UpdatedSubtree1 = [node(NewTaskId,State,Module,Goal,Children1)],
		UpdatedSubtree2 = [node(NewTaskId,State,Module,Goal,Children2)],
		::replace_by_taskid(TaskId1,UpdatedSubtree1,[MaxId,Tree],[MaxId,UpdatedTree1]),
		::replace_by_taskid(TaskId2,UpdatedSubtree2,[MaxId,UpdatedTree1],[MaxId,ReducedTree]).

	%% scheduler_tree_lookup(+TaskId,+Tree,-Node)
	% Node is the subtree which rooted at the node identified by TaskId
	
	:- public(lookup/3).
	:- info(lookup/3, [
		comment is 'Node is the subtree which rooted at the node identified by TaskId',
		argnames is ['TaskId','Tree','Node']]).

	lookup(TaskId,[_,Tree],Subtree) :-
		::scheduler_tree_lookup_rec(TaskId,Tree,Subtree).
		
	:- private(scheduler_tree_lookup_rec/3).

	scheduler_tree_lookup_rec(TaskId,[node(TaskId,State,Module,Goal,Children)],[node(TaskId,State,Module,Goal,Children)]).

	scheduler_tree_lookup_rec(TaskId,[node(OtherTaskId,_,_,_,Children)],SubTree) :-
		TaskId \== OtherTaskId,
		list::member(Child,Children),
		scheduler_tree_lookup_rec(TaskId,[Child],SubTree).

	:- public(ready_task/2).
	:- info(ready_task/2, [
		comment is 'Find a task in the tree which is ready to run. This task must be a node which a) is a leaf node and b) has the state \'ready\'.',
		argnames is ['Tree','TaskId']]).

	ready_task(Tree,TaskId) :-
		lookup(TaskId,Tree,[node(TaskId,ready,_,_,[])]).

	:- public(set_running/3).
	:- info(set_running/3, [
		comment is 'True if, a) TaskId points to leaf node and b) that node has state \'ready\'',
		argnames is ['TaskId','Tree','UpdatedTree']]).

	set_running(TaskId,Tree,UpdatedTree) :-
		::lookup(TaskId,Tree,[node(TaskId,ready,Module,Goal,[])]),
		::replace(node(TaskId,running,Module,Goal,[]),Tree,UpdatedTree).

	:- public(set_completed/3).
	:- info(set_completed/3, [
		comment is 'If TaskId corresponds to a leaf node in state state \'running\', then UpdatedTree is InitialTree without the node for TaskId.',
		argnames is ['TaskId','InitialTree','UpdatedTree']]).

	set_completed(TaskId,Tree,UpdatedTree) :-
		::lookup(TaskId,Tree,[node(TaskId,running,_,_,[])]),
		::remove(TaskId,Tree,UpdatedTree).

	:- public(empty/1).
	:- info(empty/1, [
		comment is 'True if Tree is empty',
		argnames is ['Tree']]).

	empty([_,[]]).

	:- public(print/1).
	:- info(print/1, [
		comment is 'Pretty-prints a scheduler tree',
		argnames is ['Tree']]).

	print([_,[]]) :-
		writeln('Tree is empty!').

	print([_,Tree]) :-
		scheduler_tree_print_rec(0,Tree).

	:- private(scheduler_tree_print_rec/2).
	scheduler_tree_print_rec(_,[]).

	scheduler_tree_print_rec(Indent,[node(TaskId,State,Module,Goal,Children)]) :-
		((Indent > 0) ->
			PrevIndent is Indent - 1,
			::indent(PrevIndent), write('|'), write('--')
			;
			true
		),
		%(simplify_goal(Goal,SimpleGoal) ; SimpleGoal = Goal),
		SimpleGoal = Goal,
		write('+'), write(TaskId), write(' '), write(State), write(' '), write(Module), write('::'), write(SimpleGoal), nl,
		NextIndent is Indent + 1,
		!,
		forall(list::member(Child,Children), ::scheduler_tree_print_rec(NextIndent,[Child])).
		% orig: foreach(Child in Children, scheduler_tree_print_rec(NextIndent,[Child])).
		
	:- private(indent/1).

	indent(0).
	indent(N) :-
		write('   '),
		N1 is N - 1,
		indent(N1).

	% FIXME: This wont work..
	/*
	simplify_goal(Goal,Simplified) :-
		Goal =.. [ Functor, InFiles, Opts, OutFile ],
		file_base_name(OutFile,SimplerOutFile),
		map(file_base_name,InFiles,SimplerInFiles),
		Simplified =.. [ Functor, SimplerInFiles, Opts, SimplerOutFile ].
	*/
:- end_object.

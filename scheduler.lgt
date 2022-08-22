:- if(current_logtalk_flag(threads,supported)).

:- object(scheduler).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-20,
		comment is 'Schedules parallel invocation of tasks.'
	]).

	:- threaded.

	:- public(run/2).
	:- info(run/2,[
		comment is 'The scheduler loop. Runs until the scheduler tree is empty. ',
		argnames is ['SchedulerTree','Queue']
	]).

	run(Tree,[]) :-
		scheduler_tree::empty(Tree),
		reporting::info('Parallel scheduler finishes.').

	run(Tree,Queue) :-
		::enqueue_ready(Tree,Tree1,Queue,Queue1),!,
		%{sleep(1.1)}, % Add a delay 
		::dequeue_completed(Tree1,Tree2,Queue1,Queue2),	!,
		::run(Tree2,Queue2).

	:- private(enqueue_ready/4).
	:- info(enqueue_ready/4, [
		comment is 'Enqueue all ready tasks from TreeIn, resulting in update TreeOut and QueueOut',
		argnames is ['TreeIn','TreeOut','QueueIn','QueueOut']
	]).

	% In the case where the queue is full, 
	enqueue_ready(Tree,Tree,Queue,Queue) :-
		config::get(available_cpus,CPUS),
		length(Queue,QueueLen),
		QueueLen >= CPUS.

	enqueue_ready(TreeIn,TreeOut,QueueIn,QueueOut) :-
		scheduler_tree::ready_task(TreeIn,TaskId),
		%TODO: findall ready_task followed by a selection based on resources
		!,
		scheduler_tree::lookup(TaskId,TreeIn,[node(TaskId,ready,Module,Task,[])]),!,
		scheduler_tree::set_running(TaskId,TreeIn,TreeNext),!,
		Task =.. [ TaskName, Inputs, Options ],
		% Since this task is ready, we are guaranteed that all files denoted by inputs are ready,
		% We simply call the sequential_interpreter to get the "real files"
		meta::map([FileId,FileName]>>(sequential_interpreter(execution_semantics)::run(FileId,FileName)),Inputs,InputFiles),
		TaskObject = task(Module,TaskName,InputFiles,Options),
		reporting::info(start(TaskObject)),
		threaded_call(scheduler::run_task_thread(TaskId,TaskObject)),!,
		enqueue_ready(TreeNext,TreeOut,[[TaskId,TaskObject]|QueueIn],QueueOut).

	enqueue_ready(Tree,Tree,Queue,Queue).

	:- private(dequeue_completed/4).
	:- info(dequeue_completed/4,[
		comment is 'Remove from TreeIn and QueueIn all completed tasks, resulting in TreeOut and QueueOut',
		argnames is ['TreeIn','TreeOut','QueueIn','QueueOut']
	]).

	dequeue_completed(Tree,Tree,[],[]).

	dequeue_completed(TreeIn,TreeOut,[[TaskId,TaskObject]|QueueInRest],QueueOutRest) :-
		threaded_peek(scheduler::run_task_thread(TaskId,TaskObject)),
		reporting::info(finish(TaskObject)),
		!,
		threaded_exit(scheduler::run_task_thread(TaskId,TaskObject)),
		scheduler_tree::set_completed(TaskId,TreeIn,TreeNext),
		::dequeue_completed(TreeNext,TreeOut,QueueInRest,QueueOutRest).

	dequeue_completed(TreeIn,TreeOut,[[TaskId,TaskObject]|QueueInRest],[[TaskId,TaskObject]|QueueOutRest]) :-
		::dequeue_completed(TreeIn,TreeOut,QueueInRest,QueueOutRest).

	:- private(run_task_thread/2).
	:- info(run_task_thread/2,[
		comment is 'Run Task with newly spawned thread',
		argnames is ['TaskId','Task']
	]).

	run_task_thread(_,Task) :-
		Task::run(_).

:- end_object.

:- endif.

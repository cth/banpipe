:- if(current_logtalk_flag(threads,supported)).

:- object(scheduler).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/20,
		comment is 'Schedules parallel invocation of tasks.']).
	
	:- threaded.
		
	:- public(run/2).
	:- info(run/2,[
		comment is ['The scheduler loop. Runs until the scheduler tree is empty. '],
		argnames is ['SchedulerTree','Queue']]).
	run(Tree,[]) :-
		scheduler_tree::empty(Tree).
		
	run(Tree,Queue) :-
		enqueue_ready(Tree,Tree1,Queue,Queue1),
		{sleep(0.1)}, % Add a delay 
		dequeue_completed(Tree1,Tree2,Queue1,Queue2),
		!,
		::run(Tree2,Queue2).
		
	:- private(enqueue_ready/4).
	:- info(enqueue_ready/4,[
		comment is 'Enqueue all ready tasks from TreeIn, resulting in update TreeOut and QueueOut',
		argnames is ['TreeIn','TreeOut','QueueIn','QueueOut']]).

	enqueue_ready(TreeIn,TreeOut,QueueIn,[[ReadyTask,Task]|QueueOut]) :-
		scheduler_tree::ready_task(TreeIn,TaskId),
		!,
		scheduler_tree::lookup(TaskId,TreeIn,[node(TaskId,ready,Module,Task,_)]),!,
		scheduler_tree::set_running(TaskId,TreeIn,TreeNext),!,
		threaded_call(scheduler::runk_task_thread(TaskId,Task)),
		enqueue_ready(TreeNext,TreeOut,[[TaskId,Task]|QueueIn],QueueOut).
		
	enqueue_ready(Tree,Tree,Queue,Queue).
	
	:- private(dequeue_completed/4).
	:- info(dequeue_completed/4,[
		comment is 'Remove from TreeIn and QueueIn all completed tasks, resulting in TreeOut and QueueOut',
		argnames is ['TreeIn','TreeOut','QueueIn','QueueOut']]).
 	dequeue_completed(TreeIn,TreeOut,[[TaskId,TaskObject]|QueueIn],QueueOut) :-
		threaded_peek(scheduler::run_task_thread(TaskId,TaskObject)),
		!,
		threaded_exit(scheduler::run_task_thread(TaskId,TaskObject)),
		scheduler_tree::set_completed(TaskId,TreeIn,TreeNext),
		::dequeue_complete(TreeNext,TreeOut,QueueIn,QueueOut).
		
 	dequeue_completed(TreeIn,TreeOut,[[TaskId,TaskObject]|QueueIn],[[TaskId,TaskObject]|QueueOut]) :-
		::dequeue_complete(TreeIn,TreeOut,QueueIn,QueueOut).
	
	:- private(run_task_thread/2).
	:- info(run_task_thread/2,[
		comment is 'Run Task with newly spawned thread',
		argnames is ['TaskId','Task']]).
	run_task_thread(Id,Task) :-
		Task::run(_).
:- end_object.

:- endif.
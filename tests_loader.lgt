:- initialization ((
	logtalk_load(init),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests,[hook(lgtunit)])
)).

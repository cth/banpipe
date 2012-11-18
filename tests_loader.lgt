:- initialization ((
	logtalk_load(init),
	logtalk_load(library(lgtunit)),
	logtalk_load(tests,[hook(lgtunit)])
)).

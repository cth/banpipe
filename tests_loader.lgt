:- initialization ((
	logtalk_load(init),
%	logtalk_load(library(lgtunit)),
%	logtalk_load(tests,[hook(lgtunit)])
%        logtalk_load(library(types_loader)),
        logtalk_load(lgtunit(loader)),
        logtalk_load(tests, [hook(lgtunit)])
)).

:- initialization ((
	logtalk_load(library(lgtunit_loader)),
	logtalk_load(library(metap)),
	logtalk_load(library(meta)),	
	logtalk_load(file),
	logtalk_load(list_extras),
	logtalk_load(term_manipulation),
	logtalk_load(file_index),
	logtalk_load(tests,[hook(lgtunit)])
)).

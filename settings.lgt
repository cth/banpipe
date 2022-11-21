
:- initialization((
    logtalk_load_context(directory, Directory),
    assertz(logtalk_library_path(banpipe, Directory))
)).

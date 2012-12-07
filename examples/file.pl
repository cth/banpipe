% This banpipe script illustrates how to use the builtin module file
% to fetch a file from the internet

indexpage <- file::get('http://banpipe.org/index.html',[filetype(html)]).

<link href="http://jasonm23.github.com/markdown-css-themes/markdown-alt.css" rel="stylesheet"></link>
# BANpipe

BANpipe is a logic-based scripting language designed to model complex compositions of time consuming analyses.
The language (which itself is written in [Logtalk](http://logtalk.org)) supports complex pipelines of Prolog programs, [PRISM](http://sato-www.cs.titech.ac.jp/prism/) models and other types of programs through rules which specify dependencies between computations.

Pipelines for machine learning, natural language processing and biological sequence processing are often complex compositions of time consuming analyses, including calls to external resources and databases. The expected output, intermediate results and original input data are often huge files. Banpipe is designed to facilitate such computations. 

Banpipe is a general pipeline programming language, but it is designed to support a special kind of annotation pipelines which we call [Bayesian Annotation Networks](http://drops.dagstuhl.de/opus/frontdoor.php?source_opus=3164) (BANs). A Bayesian Network is a directed acyclic graph where the nodes are conditional probability distributions and the edges represent conditional dependencies. A BAN is a Bayesian Network where nodes are instead (probabilistic) annotation programs and edges are input/output dependencies between programs. Inference in BANs is performed iteratively by evaluating each program at a time and using its output annotation as input for dependent programs. This is not only similar to the way forward analysis takes place in Bayesian networks, but also a nice fit to the pipeline paradigm.

Banpipe is implemented in [Logtalk](http://logtalk.org) and is portable across multiple Prolog systems.
It has been tested on OSX using [B-Prolog](http://www.probp.com/) and [SWI-Prolog](http://www.swi-prolog.org/) and the latest stable logtalk. Windows support seems to be flaky still.  
To run logtalk, you need Logtalk plus at least one Prolog system installed. 

BANpipe has been developed within the [LoSt research project](http://lost.ruc.dk).
BANpipe is based on what was once called "The LoSt framework", which is available [on github](https://github.com/cth/the-lost-framework).
In addition to being the predecessor of BANpipe, the Lost framework contains 
a lot of libraries and modules specific to biological sequences and is 
developed exclusively for PRISM as host language. 

If you have questions, please don't hesitate to contact me - [Christian Theil Have](mailto:cth@ruc.dk)

## License

(C) [Christian Theil Have](mailto:cth@ruc.dk), 2012.

This program is distributed under the terms of the GNU General Public License:
[http://www.gnu.org/licenses/gpl.html](http://www.gnu.org/licenses/gpl.html)

## Installation

To install banpipe you first need to download ***Logtalk***, available from [logtalk.org](http://logtalk.org).

Next, you need to download the source code for banpipe. You can download the latest version as a tgz or a zip file [here](https://github.com/cth/banpipe/downloads). 

Alternative you can clone the git repository, i.e., 

`git clone https://github.com/cth/banpipe.git`

Place the source code in any directory that you see fit, e.g. /home/myaccount/banpipe. 

After downloading banpipe, you will need to setup banpipe as a logtalk library.

You can do this by editing your `$LOGTALK_HOME/settings.lgt` file. 
You need to add a fact,  `logtalk_library_path(banpipe, '/home/myaccount/banpipe')`, 
to this file as well as multifile/1 and dynamic/1 directives. The file 
allready contains commented example. 


You also need to add any directory which contains banpipe modules to the 
`$BANPIPE_MODULE_PATH` variable. For instance, to add the example modules,

`export BANPIPE_MODULE_PATH=/home/myacccount/banpipe/examples`

The `$BANPIPE_MODULE_PATH` environment variable works like the normal PATH variable and you can add multiple directories separated by ':'

Now, you should be able to start logtalk (e.g., using `swilgt`) and call,

`{banpipe(init)}.`

which will load the banpipe library. 

You can now load banpipe scripts, e.g. the helloworld.pl examples, by calling the goal,

`banpipe::load('/home/myaccount/examples/helloworld.pl').`

and subsequently run the script using 

`banpipe::run(helloworld(english))`

See more about [running banpipe scripts](running-banpipe-scripts).

## Running BANpipe scripts

Once installation of banpipe is completed, you can load banpipe using the command

`{banpipe(init)}.`


### Loading a banpipe script

A banpipe script is loaded using the command 

`banpipe::load(Filename)`

where `Filename` is a relative or absolute file name in the filesystem. 
You need to supply the full filename including possible extensions, e.g. `.pl`


### Running a goal in a banpipe script

To run a goal in a banpipe script the command,

`banpipe::run(Goal)`

is used. This will run the scripts sequentially in a bottom-up, left-to-right fashion, i.e., dependencies are 
computed first left-most dependencies in a task are computed before right-most dependencies.

To get the filename corresponding to the goal, call instead,

`banpipe:run(Goal,File)`

This will unify `File` to the generated file corresponding to `Goal`.

### Running scripts in parallel.

When banpipe::run(Goal) is called, scripts are run in a sequential fashion.
It is possible also to run scripts with automatic parallelization, so that all independent tasks
will be run simultaneously. This is achieved by calling,

`banpipe::prun(Goal).`

Runnings scripts with parallization is only available when running banpipe with logtalk on a Prolog
system which has threads, e.g., SWI-Prolog.

### Typechecking

Scripts are not type checked when calling `banpipe::run`or `banpipe::prun`. Is perfectly possible to 
run a script that is not well-typed. However, type checking a script in advance may help to avoid 
errors. To typecheck a script use the goal,

`banpipe::typecheck(Goal)`

or similarly

`banpipe::typecheck(Goal,Type)`

to get the `Type` corresponding to `Goal`. `banpipe::typecheck` fails with a report of the type error,
if the script is not well-typed. 

### Generating a call-graph

A call graph shows the interdependencies between tasks and may be used as a way to debug
scripts. Even though it is a graph, it is displayed as a tree where nodes which are shared 
in the graph are shown as multiple branches of the tree. A call graph is obtained by calling,

`banpipe::callgraph(Goal)`

For instance, for the hello world example, the following call graph is produced:

    ?- banpipe::callgraph(helloworld(english)).
    +1 ready helloworld::helloworld([hello(english),world(english)],[])
    |--+3 ready helloworld::world([],[language(english)])
    |--+2 ready helloworld::hello([],[language(english)])

### Tracing a script

Scripts can be run through an interactive trace. This is enabled by typing `banpipe::trace.` 
To disable tracing, type `banpipe::notrace.`

### Change propagation 

There is really nothing special about change propagation. Applying change propagation is 
achieved be re-running goals. However, changes to modules are not detected automatically. 
Instead, each task declaration in a module may include a `version` option.
When the the version option is changed, the task gets a new signature and will be recomputed 
the next time a goal involving the task is called. 

## BANpipe modules

Banpipe modules are not to be confused with Prolog modules. A banpipe module is simply a collection of source files residing in a particular directory. The name of the module is the same as the directory, e.g., 
if a module is placed in `/home/user/modules/foo` then the name of the module is `foo`.

In the module directory, banpipe expects to find a file called `interface.pl`. This is a Prolog file provided by the the module creator which contains declarations and predicates for invoking the module.

### Task declarations and implementations

The interface file contains declarations of ***task predicates*** provided by the module.
An ***task declaration*** has the form,

`:- task(<taskname>(<input-file-types>,<options>,<output-file-types>)).`

The declaration specifies that the module provides a task predicate with the name `<taskname>`. 
Such predicates always takes three arguments, a list of input files, a list of options and a list of output files. `<input-file-types>` is a Prolog list of terms which specifies the number and types of input files that the task predicate expects. The types are used defined and any Prolog term is a valid type. Similarly, `<output-file-types>` is a list of terms. The terms may contain variables, which may be shared between input types and output types. In this way, an output file type, may depend on an input file type. `<options>` is a of terms of arity one. The functor serves as key and the argument serves as value. Options may also contain variables. 

### The connection between modules and banpipe scripts

Suppose that the interface file `/home/user/modules/foo/interface.pl`, i.e. of the module `foo` includes a ***task declaration***:

`:- task(bar([_],[],[_])`

The name of task is `bar` and it takes one input file (of unspecified type `_`), it takes no options and it writes one output file (of unspecified type `_`). Furthermore, the interface file contains a ***task predicate***,

`bar([InFile],_,[OutFile]) :- ...`

This predicate will be called (see below) with ground filenames. Banpipe guarantees that `InFile` is available when the predicate is called and expects are `OutFile` is available (has been written) when the predicate terminates.

The task predicate gets called when a goal of a dependency rule in the banpipe script refers to the task predicate in its body, e.g., 

`foobar <- foo::bar(['file:://tmp/infile']).`

When the goal `banpipe::run(foobar,File)` is called then a new Prolog process will be started and the `/home/user/modules/foo/interface.pl` will be consulted (from the `/home/user/modules/foo/` directory). This prolog process will then call the `bar` task predicate with `Infile='/tmp/infile'` and `OutFile` being a unique filename generated by the system. When `banpipe::run(foobar,File)` finishes `File` is unified to this unique file.

#### invoke_with/1 declaration 

Interface files may also contain an opptional `invoke_with/1` declaration, i.e., the declaration 

`:- invoke_with(swipl)`

specifies that the `interface.pl` should be loaded with SWI-prolog. The current list of valid arguments for `invoke_with/1` are

- prism (PRISM - this is the default)
- bp (B-Prolog)
- swipl (SWI-Prolog)
- gprolog (GNU-Prolog)

### Examples

Documented examples are available in the `examples/` folder of banpipe [(on github)](https://github.com/cth/banpipe/tree/master/examples).

## For developers

- [Code on github](https://github.com/cth/banpipe)
- [Generated documentation](http://banpipe.org/apidoc/)

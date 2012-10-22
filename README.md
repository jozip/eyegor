Eyegor
======

> Suit yourself. I'm easy.

Eyegor is a quick-and-dirty development helper for Erlang projects. It
currently only consists of an automatic recompiler and a reloader shamelessly
ripped from the [Mochiweb](http://github.com/mochi/mochiweb) project.

Getting started
----------------

### With [Rebar](http://github.com/basho/rebar) ###

Add the following tuple in the `deps` array of your project's rebar.config file:

    {eyegor, ".*",
      {git, "git://github.com/toki/eyegor.git", "master"}}
           
And then execute

    $ ./rebar get-deps compile
    
to get and compile it.

### By hand ###

    $ git clone git://github.com/toki/eyegor.git
    $ cd eyegor
    $ ./rebar compile

### Usage ###

Add the following options when starting a node:

    -pa path/to/eyegor/ebin -s eyegor 


Credits
-------

* Johan Persson <johan@programlabbet.se>  
* Matthew Dempsky <matthew@mochimedia.com> for `reloader.erl`

---

> Dr. Frederick Frankenstein: You must be Igor.  
> Igor: No, it's pronounced "eye-gor."  
> Dr. Frederick Frankenstein: But they told me it was "ee-gor."  
> Igor: Well, they were wrong then, weren't they?  

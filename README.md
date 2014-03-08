# Simple Restful service for shoes

## The project

To build the server and run the tests:

```bash

  cabal init
  cabal install --enable-tests --denpendencies-only
  cabal build
  cabal test
```

As the `server` program is only part of the setup, to actually serve requests,
after installing [nginx](http://nginx.org/) &
[PostgreSQL](http://www.postgresql.org/):

```bash

  ./run_server.sh <photo dir> <postgres connection string> [testing]
```

1. `photo dir` is the directory to save uploaded photos, which will automatically
created if non-existent. Note that all uploaded photos are directly saved under
that directory, so for test purpose, depending on the file system you're using,
don't upload, say 1 million images.
1. `postgres connection string` is the connection string used to connect to the
PostgreSQL database. The `server` program only supports the `PostgreSQL`
database. Though it will be easy to switch to MySQL.
1. `testing` is an optional flag to indicate if it is a test run, and if it is,
the `photo dir` will be deleted after the script exits.

The script will start an `nginx` process with the configuration coming with the
code. It will also migrate tables if necessary. After that, it will run `server`
and block until the `server` process is interrupted or killed. Then it will clean
up the nginx process and left over log files. If it is a test run, the photo
directory will also be deleted.

The main modules are written in Literate Haskell with Markdown, and to
produce the formatted document, after installing
[pandoc](http://johnmacfarlane.net/pandoc/):

```bash

  ./generate_pandoc.sh
```

## About me

I have a day job, for which I write in C++ and Python. However, after trying out
with various languages like Smalltalk and Lisp, I setttled with Haskell as my
personal preference for writing software. I happen to be writing web servers as
a side project, and have researched the existing Web frameworks in Haskell. I
chose Scotty as it is small and easier to start with by myself. Also it helps
me under design decisions in more sophisticated frameworks, like sessions and
authentication.

I have written distributed services on Linux for 6 years. I have learned how to
keep the services up, and how to scale them. I have designed APIs as well as
system internals, and I am keen to possible defects in designs. I am also very
self-motivated. On this project, for example, I often code till early morning to
fix compilation errors. I hate to sleep with unsolved problems, though, I do have
to sleep, everyday that is.

As a Haskeller, I cannot say I'm more than intermediate. I am totally self
taught, and am willing to learn from the experienced in this language. I have
pratical skills to tackle engineering problems, and can help with service
development. I am also fancinated by the open source movement, and am eager to
contribute.

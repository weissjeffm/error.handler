error.handler is a small error handling library for Clojure.  It is similar to clojure.contrib.condition, but adds a bit of functionality while taking away the need for AOT compilation.

What it intends to provide:
===========================

* The ability to specify error handlers at the caller's level, that 
are accessible all the way up the stack from them. 
* Ability to include more data in an error than just a message and 
stack trace.  That data should be accessible to handlers. 
* The ability to specify pre-defined methods of recovering from an 
error, at any point on the stack between the caller and the origin of 
the error.  The caller can select the recovery method by name. 
* Ability to handle errors by type, in a hierarchical fashion, similar 
to java's catch. 
* Compatibility with java exceptions. 
* Ability to nest handler forms 
* No AOT compilation 

Usage:
======

See the unit tests for examples.

Copyright (C) 2011

Distributed under the Eclipse Public License, the same as Clojure.

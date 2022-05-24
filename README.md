# What

This repository contains code to measure the worst-case pauses
observable from of a specific workflow in many languages.

The workflow (allocating N 1Kio strings with only W kept in memory at
any time, and the oldest string deallocated) comes from [James
Fisher](https://jameshfisher.com)'s blog post [Low latency, large
working set, and GHC’s garbage collector: pick two of
three](https://blog.pusher.com/latency-working-set-ghc-gc-pick-two/),
May 2016, who identified it as a situation in which the GHC garbage
collector (Haskell) exhibits unpleasant latencies.

# Results

| Language                             | Longest pause (ms)                      |
| -------------------------------------| --------------------------------------- |
| C (GCC 12.1)                         | 0.379                                   |
| Common Lisp (CCL 1.12)               | Unknown - not supported on architecture |
| Common Lisp (ECL 21.2.1 compiled)    | 6                                       |
| Common Lisp (ECL 21.2.1 interpreted) | 7                                       |
| Common Lisp (SBCL 2.2.4 interpreted) | 779                                     |
| Crystal (1.4)                        | Unknown - not supported on architecture |
| D (LDC 1.9.0)                        | 1.926                                   |
| Erlang (25.0)                        | 16.076                                  |
| .NET (6.0)                           | 17.343                                  |
| Go (1.18)                            | 4.209                                   |
| Haskell (GHC 9.2)                    | 116.283                                 |
| Java (OpenJDK 19)                    | 15                                      |
| Node (18.2)                          | 70                                      |
| OCaml (4.12)                         | 81.594                                  |
| PHP (8.1)                            | 6.694                                   |
| Python (3.9)                         | 11.830                                  |
| Racket (7.9)                         | 323.15                                  |
| Ruby (3.1)                           | 5.887                                   |

On Macbook Air M1 with 8GB memory.

# How to run

Because each benchmark requires a language-specific toolchain to build/run,
we have included Dockerfiles to make this environment consistent.
With Docker downloaded, a benchmark can be run with

```
make racket/results.txt
```

or by running Docker directly:

```
docker build -t gc-racket racket
docker run gc-racket
```

replacing `racket` with whatever language you are interested in.

# How to contribute

The reference repository for this benchmark is Will Sewell's
<https://github.com/WillSewell/gc-latency-experiment>. It was
previously maintained by Gabriel Scherer at
<https://gitlab.com/gasche/gc-latency-experiment>.

Pull requests to implement support for a new language are
welcome.

The benchmark should write the worst case pause time in milliseconds
to STDOUT. You must include a Dockerfile which installs the
benchmark dependencies and runs the benchmark in the entrypoint.

We encourage you to use the best performing compiler/runtime
systems options.

# How to measure worst-case latency

The benchmark is essentially a loop where each iteration allocates
a new string and adds it to the message set, and (if the maximum
window size W is reached) also removes the oldest message in the set.

## Runtime instrumentation vs. manual measurement

There are two ways to measure worst-case pause time. One,
"instrumentation", is to activate some sort of runtime
monitoring/instrumentation that is specific to the language
implementation, and get its worst-case-pause number. Another, "manual
measure" is to measure time at each iteration, and compute the
maximal difference.

We recommend trying both ways (it's good to build knowledge of how to
measure GC latencies, and having a `Makefile` full of instructions for
many languages is useful). One has to be careful with instrumentation,
as it may be incomplete (not account for certain sources of pauses);
if the two measures disagrees, we consider the manual measure to be the
reference.

## Message payloads

The fact that the messages themselves take some space is an essential
aspect of the benchmark: without it, GHC's garbage collector does an
excellent job. Please ensure that your implementation actually
allocates 1Kio of memory for each message (no copy-on-write,
etc.). (It's fine if the GC knows that this memory doesn't need to
be traversed). You should also avoid using less-compact string
representations (UTF16, or linked lists of bytes, etc.).

## Message set structure

The message set is an associative data structure where each message is
indexed by the time it was inserted.

We are not trying to measure latency caused by the specific choice of
associative data-structure. For the initial tests Haskell, OCaml and
Racket, using an array, a balanced search tree or a hashtable makes no
difference. For a new language, feel free to choose whichever gives
the best results; but if one of them creates large latencies, you may
want to understand why -- there were bugs in Go's maps that made
latencies much higher, and some of them were since fixed. For GC-ed
languages it may be the case that contiguous arrays are actually worse
than structures with more pointer indirections, if the GC doesn't
incrementally scan arrays; then use another data structure.

# Links and reference

Gabriel Scherer wrote a blog post on measuring the latency of the
OCaml benchmark through GC instrumentation, and of how Racket
developers advised to tune the Racket benchmark and decided to make
small changes to their runtime: [Measuring GC latencies in Haskell,
OCaml,
Racket](http://prl.ccs.neu.edu/blog/2016/05/24/measuring-gc-latencies-in-haskell-ocaml-racket/).

Will Sewell, working at the same company as Jame Fisher's, has
a follow-up blog post on this work where Go latencies are discussed:
[Golang’s Real-time GC in Theory and
Practice](https://blog.pusher.com/golangs-real-time-gc-in-theory-and-practice/).

Gorgi Kosev runs another latency-copmarison benchmark, also inspired
by the same blog post, but also involving HTTP requests, at
<https://github.com/spion/hashtable-latencies>.

Santeri Hiltunen has a nice [blog
post](https://blog.hilzu.moe/2016/06/26/studying-gc-latencies/) with
other measurements, in particular some information on tuning the Java
benchmark, and a Javascript benchmark with similar explanations, and
data on the importance of graph-of-pointers over contiguous data
structures to reduce latencies.

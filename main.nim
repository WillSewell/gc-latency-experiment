# Compile and run with 'nim c -r -d:useRealtimeGC -d:release main.nim'
# See here for more info: http://forum.nim-lang.org/t/2646

import strutils

include "$lib/system/timers"

const
  windowSize = 200000
  msgCount   = 1000000

type
  Msg = seq[byte]
  Buffer = seq[Msg]

var worst: Nanos

proc mkMessage(n: int): Msg =
  result = newSeq[byte](1024)
  for i in 0 .. <result.len:
    result[i] = byte(n)

proc pushMsg0(b: var Buffer, highID: int) =
  # warmup:
  let m = mkMessage(highID)
  shallowCopy(b[highID mod windowSize], m)

proc pushMsg1(b: var Buffer, highID: int) =
  # with benchmarking:
  let start = getTicks()

  let m = mkMessage(highID)
  shallowCopy(b[highID mod windowSize], m)

  let elapsed = getTicks() - start
  if elapsed > worst:
    worst = elapsed

proc main() =
  # Don't use GC_disable() and GC_step(). Instead use GC_setMaxPause().
  when defined(disableMS):
    GC_disableMarkAndSweep()
  GC_setMaxPause(300)

  var b = newSeq[Msg](windowSize)
  # we need to warmup Nim's memory allocator so that not most
  # of the time is spent in mmap()... Hopefully later versions of Nim
  # will be smarter and allocate larger pieces from the OS:
  for i in 0 .. <msgCount:
    pushMsg0(b, i)

  # now after warmup, we can measure things:
  for i in 0 .. <msgCount:
    pushMsg1(b, i)

  echo(worst.float / 1_000_000.0, " ms")

when isMainModule:
  main()

let time () = Mtime_clock.counter ()

let from counter = Mtime_clock.count counter

let to_nsecs = Mtime.Span.to_ns
let to_secs = Mtime.Span.to_s

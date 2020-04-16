#### How to use:
1. Create an `Event t (Job e a)` where `Job e a` is a newtype wrapper over `IO (Either e a)`.
2. Decide on an "initial rate" `(Int, Picos)` where the `Int` is "jobs per tick", and the `Picos` is the interval length between ticks in picoseconds. Interval boundaries start from midnight, and the ticks start at the next interval start.
3. Decide on optional rate constraints `Seq (Int, Picos)` which are a sequence of (jobs per tick, interval length) pairs that the throughput cannot exceed.
4. Decide on a rate governor function `(Int, Picos) -> Either e a -> (Int, Picos)` which is called to possibly modify the job rate depending on the result of a job, and the current rate.
5. Call `rateLimit` with the above parameters.

#### What happens:
A tick event stream is created that drives the rate limiter. Each tick we batch however many jobs decided by the parameters above and run them asynchronously, each in their own thread. If the jobs in the batch finish successfully they are output in FIFO order. If some of the jobs in a batch don't finish successfully they are retried until they succeed. Typically you'd use the rate governor function to decide how to slow the rate in response to failures (and raise the rate in response to successes). If any of the rate constraints are exceeded then the rate is lowered to 0 until the rate constraint intervals lengths are exceeded and the constraints reset.

#### Purpose:
I wanted to see how writing a rate limiter might look in FRP, and wanted to try Reflex. I also need a rate limiter for another project, hence some idiosyncratic design decisions.

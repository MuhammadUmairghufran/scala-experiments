// idea - we go from last to first and need to calculate everything a lot of times
// @annotation.tailrec - annotation throws error
def fib(n: Long): Long =
  if (n==0) 0
  else {
    if (n==1) 1
    else fib(n-1) + fib(n-2)
  }

fib(0)
fib(1)
fib(2)
fib(3)
fib(10)

// we go from first to last - we have tail recursive optimisation
// and do not recalculate every time
def fibi(n: Long): Long = {
  @annotation.tailrec
  def fibIter(left: Long, right: Long, counter: Long): Long = {
    if (n == counter) left
    else fibIter(left + right, left, counter + 1)
  }
  fibIter(0, 1, 0)
}

fibi(10)
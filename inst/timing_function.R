timed = function(ltime = proc.time()[3]) {
  ## use: t_name = t = timed(t)
  ##     this saves t_name and keeps t for next timed(t)
  ## returns two components:
  ##     seconds from start of process
  ##     seconds difference from first element of ltime
  time = proc.time()[3]
  c(time, time - ltime[1])
}


t_code1 = t_code2 = NULL
t0 = timed()

x <- rnorm(5e7)
t_code1 = (t1 = timed(t0))[2]
t_code1


t_total = timed(t0)[2]

cat(" Section  time\n",
         "  code1", t_code1, "\n",
         "  code2", t_code2, "\n",
         
         "  total", t_total, "\n",
         "\n", all.rank=TRUE)

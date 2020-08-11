import time

# Usage:
#    t = Timer()
#    with t:
#       <some-computation>
#    # Prints time, at the end

class Timer:

  def __init__(self):
    # monotonic = fractional seconds, clock cannot go backwards, unaffected by system clock updates
    self.monotonic_start    = None
    self.monotonic_end      = None
    # perf_counter = includes sleep
    self.perf_counter_start = None
    self.perf_counter_end   = None
    # process_time = does not include sleep
    self.process_time_start = None
    self.process_time_end   = None

  def __enter__(self):
    self.monotonic_start    = time.monotonic()
    self.perf_counter_start = time.perf_counter()
    self.process_time_start = time.process_time()

  def __exit__(self, a, b, c):
    self.monotonic_end    = time.monotonic()
    self.perf_counter_end = time.perf_counter()
    self.process_time_end = time.process_time()
    self.print_time()

  def print_time(self):
    # Subclasses may override
    d1 = self.monotonic_end    - self.monotonic_start
    d2 = self.perf_counter_end - self.perf_counter_start
    d3 = self.process_time_end - self.process_time_start
    print(d3)
    # print("monotonic: %s  perf. counter: %s  process time: %s" % (d1, d2, d3))

import os
import gc as _gc
import timeit
try:
	import resource
except ModuleNotFoundError:
	resource = None
import time
import numpy as np
import pandas as pd
try:
	import psutil
except:
	psutil = None

#       nano micro milli    kilo mega giga tera peta exa  zeta yotta
tiers = ['n', 'µ', 'm', '', 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y']


def si_units(x, kind='B', f="{:.3g} {}{}"):
	tier = 3
	shift = 1024 if kind=='B' else 1000
	if x > 0:
		while x > 1024 and tier < len(tiers):
			x /= shift
			tier += 1
		while x < 1 and tier >= 0:
			x *= shift
			tier -= 1
	return f.format(x,tiers[tier],kind)


class MemoryUsage:

	def __init__(self):
		self.memory_history = [0,]
		self.max_memory_history = [0,]
		self.pid = os.getpid()  # the current process identifier, to track memory usage
		if psutil is None:
			raise ModuleNotFoundError("pstil")
		self.check()

	def check(self, silent=False, gc=False, time_checkpoint=None):
		if gc:
			_gc.collect()
		now_memory = psutil.Process(self.pid).memory_info().rss
		if resource:
			max_memory = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
			marginal_max = max_memory - self.max_memory_history[-1]
		else:
			max_memory = None
			marginal_max = None
		marginal_usage = now_memory - self.memory_history[-1]
		if time_checkpoint:
			time_note = si_units(time.time()-time_checkpoint, kind='s') + ": "
		else:
			time_note = ""
		if not silent:
			print(f"{time_note}Net {si_units(marginal_usage)}, Total {si_units(now_memory)}")
		self.memory_history.append(now_memory)
		self.max_memory_history.append(max_memory)

	def __enter__(self):
		_gc.collect()
		_gc.disable()
		self._context_start_time = time.time()
		self.check(silent=True, gc=False)

	def __exit__(self, exc_type, exc_val, exc_tb):
		self.check(time_checkpoint=self._context_start_time)
		_gc.enable()
		_gc.collect()


resource_usage = MemoryUsage()


def start_memory_tracing():
	'''More intensive memory tracing'''
	import tracemalloc
	tracemalloc.start()


def stop_memory_tracing():
	import tracemalloc
	tracemalloc.stop()


def ping_memory_tracing():
	import tracemalloc
	current, peak = tracemalloc.get_traced_memory()
	print(f"Current memory usage is {si_units(current)}; Peak was {si_units(peak)}")


def timing(stmt, setup='pass', repeat=10, globals=None, quiet=False):
	t = timeit.Timer(
		stmt,
		setup=setup,
		globals=globals,
	)
	n, duration = t.autorange()
	timings = np.asarray([duration]+t.repeat(repeat-1, n)) / n
	_mean = si_units(np.mean(timings), 's')
	_std = si_units(np.std(timings), 's')
	_min = si_units(min(timings), 's')
	_max = si_units(max(timings), 's')
	if not quiet:
		if n > 1:
			print(f"{_mean} ± {_std} per loop (mean ± std of {repeat} runs, {n} loops each), {_min} to {_max}")
		else:
			print(f"{_mean} ± {_std} per run (mean ± std of {repeat} runs), {_min} to {_max}")
	return timings


def search_path(*paths):
	"""Search and return the first path that exists."""
	from pathlib import Path
	for pth in paths:
		if os.path.exists(pth):
			return Path(pth)
		pth = os.path.expanduser(pth)
		if os.path.exists(pth):
			return Path(pth)
		pth = os.path.expandvars(pth)
		if os.path.exists(pth):
			return Path(pth)


from contextlib import contextmanager

@contextmanager
def profiler():
	from pyinstrument import Profiler
	profiler = Profiler()
	profiler.start()
	yield
	profiler.stop()
	print(profiler.output_text(unicode=True, color=True))

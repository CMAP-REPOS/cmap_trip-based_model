import cmap_modedest

log = cmap_modedest.log_to_stderr(level=10)


def L(*args):
	"""Short logging function for estimation"""
	if len(args) == 1 and isinstance(args[0], str) and args[0][0] == "#":
		log.info(args[0])
	else:
		s = "\n".join(str(i) for i in args)
		s = "\n"+s
		log.info(s.replace("\n", "\n    "))

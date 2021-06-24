import os
import logging
from logging.handlers import RotatingFileHandler
LOGGER_NAME = 'CMAP'
LOG_FORMAT = '[{elapsedTime}] {name:s}:{levelname:s}: {message:s}'
WORKERLOG_FORMAT = '[{asctime}][{elapsedTime}] {name:s}:{levelname:s}: {message:s}'

def format_elapsed_time(duration_milliseconds):
    hours, rem = divmod(duration_milliseconds/1000, 3600)
    minutes, seconds = divmod(rem, 60)
    if hours:
        return ("{:0>2}:{:0>2}:{:05.2f}".format(int(hours),int(minutes),seconds))
    else:
        return ("{:0>2}:{:05.2f}".format(int(minutes),seconds))


class ElapsedTimeFormatter(logging.Formatter):
    def format(self, record):
        record.elapsedTime = format_elapsed_time(record.relativeCreated)
        return super(ElapsedTimeFormatter, self).format(record)

def getLogger(name=None):
    return logging.getLogger(name or LOGGER_NAME)

def getSubLogger(subname):
    return logging.getLogger(f"{LOGGER_NAME}.{subname}")

def log_to_stderr(level=30, log_dir=None):
    """
    Turn on logging and add a handler which prints to stderr

    Parameters
    ----------
    level : int
        minimum level of the messages that will be logged
    """
    import time
    logger = logging.getLogger(LOGGER_NAME)

    # avoid creation of multiple stream handlers for logging to console
    for entry in logger.handlers:
        if isinstance(entry, logging.StreamHandler) and (entry.formatter._fmt == LOG_FORMAT):
            return logger

    formatter = ElapsedTimeFormatter(LOG_FORMAT, style='{')
    handler = logging.StreamHandler()
    handler.setLevel(level)
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    if log_dir is not None:
        # also log to a file
        filehandler = RotatingFileHandler(
            filename=os.path.join(log_dir, f"{time.strftime('cmap_trip_log_master_%Y%m%d_%H%M%S')}.log"),
            mode='a',
            maxBytes=1_000_000,
            backupCount=10,
        )
        filehandler.setLevel(level)
        filehandler.setFormatter(formatter)
        logger.addHandler(filehandler)
    logger.propagate = False
    logger.setLevel(level)

    return logger


def log_df(df, logger=None, level=20, verbose=0, indent="    "):
    from io import StringIO
    if logger is None or isinstance(logger, str):
        from logging import getLogger
        logger = getLogger(logger)
    with StringIO() as buffer:
        df.info(verbose=verbose, buf=buffer)
        info = buffer.getvalue()
        if indent:
            import textwrap
            info = textwrap.indent(info, indent, lambda line: True)
        logger.log(level, info)


def get_worker_log(log_dir, level=20):
    """
    Turn on logging and add a handler which writes to a process-specific log file

    Parameters
    ----------
    level : int
        minimum level of the messages that will be logged
    """

    os.makedirs(log_dir, exist_ok=True)

    logger = logging.getLogger(LOGGER_NAME)

    # avoid creation of multiple stream handlers for logging to worker log
    for entry in logger.handlers:
        if isinstance(entry, RotatingFileHandler) and (entry.formatter._fmt == WORKERLOG_FORMAT):
            return logger

    formatter = ElapsedTimeFormatter(WORKERLOG_FORMAT, style='{')
    handler = RotatingFileHandler(
        filename=os.path.join(log_dir, f"cmap_trip_log_{os.getpid()}.log"),
        mode='a',
        maxBytes=1_000_000,
        backupCount=10,
    )
    handler.setLevel(level)
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    logger.propagate = False
    logger.setLevel(level)

    return logger

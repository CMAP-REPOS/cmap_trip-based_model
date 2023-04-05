# Mode, Destination, and Time of Day Technical Guide

## Functional Flow

When you invoke the `cmap_modedest` tool on the command line, a Python session
is started and the `run` function (see `cmap_modedest/__main__.py`) is called.

- `run()` in `cmap_modedest/__main__.py`
    
    This is the main control loop function.  It processes command line arguments
    and then launches `choice_simulator_trips_many` as configured.

- `choice_simulator_trips_many()` in `cmap_modedest/application.py`
  
    This is the workhorse function, it breaks TAZ's into batches, starts multiple
    parallel processes to handle trip simulation on the batches, and aggregates
    the results.  The other principal functions that this function calls include
    `choice_simulator_trips`, `compute_deadhead_trip_table`, and `assemble_trips`.

- `choice_simulator_trips()` in `cmap_modedest/application.py`
  
    This is the single-process function to model trips by mode/dest/time, called
    once for each job started by `choice_simulator_trips_many`.

- `compute_deadhead_trip_table()` in `cmap_modedest/application.py`
      
    This is where deadhead trips are created.

- `assemble_trips()` in `cmap_modedest/application.py`

    This is where all the various parquet trip-list files are assembled into
    a common dask DataFrame for processing.

- `choice_simulator_initialize` in `cmap_modedest/application.py`
    
    This function checks if the mode+destination choice models have already been
    loaded into memory for this process.  If they have not, it then checks if
    a pickled version of the choice models has been saved to disk, and loads that
    if it is available. Otherwise, it initializes the choice models from scratch
    using the `model_builder` function.

- `model_builder` in `cmap_modedest/choice_model.py`

    This function creates the structure of the model and sets parameter
    values within that structure if given.  

- `choice_simulator_prob` in `cmap_modedest/application.py`

    Here, the choice model is used to compute the probabilities for the mode and 
    destintion choices.




## Statefulness

The program avoids creating a global "state" to facilitate passing the state to 
subprocesses that actually execute jobs.  Instead, a common DataHandler object 
is used to pass state (i.e. the names of the input files, configuration options, 
etc) from the main process to subprocessed.

The main state object is a `DataHandler` object. This class is defined in 
`cmap_modedest/data_handlers/__init__.py`. An instance of the DataHandler is passed
around between functions, generally as the first argument to each function that 
needs it, as an argument named `dh`.


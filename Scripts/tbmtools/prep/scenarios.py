def create_transit_scenario(network_file, mode_file, link_shape_file, scenario_id,
                            scenario_title, modeller):
    """
    Create a transit scenario from transaction files.

    Creates a scenario with transit modes, nodes, links, and link shape
    added from a mode transaction file, a base network transaction file,
    and a link shape transaction file. Overwrites an existing scenario.

    Parameters
    ----------
        network_file : str
            Path to base network transaction file.
        mode_file : str
            Path to mode transaction file.
        link_shape_file : str
            Path to link shape transaction file.
        scenario_id : int
            Scenario number for new scenario.
        scenario_title : str
            Title for new scenario.
        modeller : inro.modeller.Modeller
            Modeller instance of active Emme project.

    Returns
    -------
    """
    # Construct Modeller tools.
    create_scenario = modeller.tool('inro.emme.data.scenario.create_scenario')
    process_mode_file = modeller.tool('inro.emme.data.network.mode.mode_transaction')
    process_network_file = modeller.tool('inro.emme.data.network.base.base_network_transaction')
    process_link_shape_file = modeller.tool('inro.emme.data.network.base.link_shape_transaction')
    # Create the scenario.
    create_scenario(scenario_id,
                    scenario_title,
                    overwrite=True,
                    set_as_primary=True)
    # Add modes.
    process_mode_file(transaction_file=mode_file)
    # Add nodes and links.
    process_network_file(transaction_file=network_file)
    # Add link shape.
    process_link_shape_file(transaction_file=link_shape_file, revert_on_error=False)
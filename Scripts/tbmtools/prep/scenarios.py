def create_transit_scenario(network_file, mode_file, link_shape_file, scenario_id, scenario_title, modeller):
    """
    Creates a scenario with transit modes, nodes, and links added from a mode
    transaction file and a base network transaction file.
    """
    # Construct Modeller tools.
    create_scenario = modeller.tool('inro.emme.data.scenario.create_scenario')
    process_mode_file = modeller.tool('inro.emme.data.network.mode.mode_transaction')
    process_network_file = modeller.tool('inro.emme.data.network.base.base_network_transaction')
    process_link_shape_file = modeller.tool('inro.emme.data.network.base.link_shape_transaction')
    # Create the scenario.
    create_scenario(scenario_id,
                    scenario_title,
                    set_as_primary=True)
    # Add modes.
    process_mode_file(transaction_file=mode_file)
    # Add nodes and links.
    process_network_file(transaction_file=network_file)
    # Add link shape.
    process_link_shape_file(transaction_file=link_shape_file,
                            revert_on_error=False)


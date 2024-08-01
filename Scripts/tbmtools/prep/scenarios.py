def create_transit_scenario(network_file, mode_file, link_shape_file, scenario_id,
                            scenario_title, modeller, turn_file=None):
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
    process_turn_file = modeller.tool('inro.emme.data.network.turn.turn_transaction')
    # Create the scenario.
    create_scenario(scenario_id,
                    scenario_title,
                    overwrite=True,
                    set_as_primary=True)
    # Add modes.
    if isinstance(mode_file, list):
        for file in mode_file:
            process_mode_file(transaction_file=str(file))
    else:
        process_mode_file(transaction_file=str(mode_file))
    # Add nodes and links.
    if isinstance(network_file, list):
        for file in network_file:
            process_network_file(transaction_file=str(file))
        process_turn_file(transaction_file=str(turn_file),
                          revert_on_error = False)
    else:
        process_network_file(transaction_file=str(network_file))
    # Add link shape.
    process_link_shape_file(transaction_file=str(link_shape_file), revert_on_error=False)

def configure_gtfs_scenario(scenario_id, modeller):
    """
    Add GTFS network fields and extra attributes to a transit scenario.
    """
    # Define GTFS network fields and extra attributes.
    transit_line_fields = [('#route_name', 'route name'),
                           ('#trip_id', 'trip id'),
                           ('#agency_name', 'agency name'),
                           ('#trip_headsign', 'trip destination headsign'),
                           ('#emme_name', 'default line id')]
    transit_line_attributes = [('@bikes_allowed', 'bicycles allowed 0=unknown 1=yes 2=no'),
                               ('@direction', 'trip direction 0=outbound 1=inbound'),
                               ('@first_departure', 'first departure in mins from midnight'),
                               ('@wheelchair_access', 'wheelchair access 0=unknown 1=yes 2=no')]
    transit_segment_fields = [('#stop_name', 'stop name'),
                              ('#emme_line_name', 'default line id'),
                              ('#i_fare_zone', 'fare zone at inode'),
                              ('#j_fare_zone', 'fare zone at jnode')]
    transit_segment_attributes = [('@stop_time', 'inter-service stop time'),
                                  ('@stop_count', 'number of stops on segment'),
                                  ('@split_time', 'proportional travel time on segment'),
                                  ('@zfare', 'zone fare')]
    # Construct Modeller tools.
    create_net_field = modeller.tool('inro.emme.data.network_field.create_network_field')
    create_attribute = modeller.tool('inro.emme.data.extra_attribute.create_extra_attribute')
    # Add GTFS network fields to transit lines.
    for fld in transit_line_fields:
        fld_name = fld[0]
        fld_desc = fld[1]
        create_net_field(network_field_type='TRANSIT_LINE',
                         network_field_atype='STRING',
                         network_field_name=fld_name,
                         network_field_description=fld_desc,
                         overwrite=True,
                         scenario=modeller.emmebank.scenario(scenario_id))
    # Add GTFS network fields to transit segments.
    for fld in transit_segment_fields:
        fld_name = fld[0]
        fld_desc = fld[1]
        create_net_field(network_field_type='TRANSIT_SEGMENT',
                         network_field_atype='STRING',
                         network_field_name=fld_name,
                         network_field_description=fld_desc,
                         overwrite=True,
                         scenario=modeller.emmebank.scenario(scenario_id))
    # Add GTFS extra attributes to transit lines.
    for attr in transit_line_attributes:
        attr_name = attr[0]
        attr_desc = attr[1]
        create_attribute(extra_attribute_type='TRANSIT_LINE',
                         extra_attribute_name=attr_name,
                         extra_attribute_description=attr_desc,
                         overwrite=True,
                         scenario=modeller.emmebank.scenario(scenario_id))
    # Add GTFS extra attributes to transit segments.
    for attr in transit_segment_attributes:
        attr_name = attr[0]
        attr_desc = attr[1]
        create_attribute(extra_attribute_type='TRANSIT_SEGMENT',
                         extra_attribute_name=attr_name,
                         extra_attribute_description=attr_desc,
                         overwrite=True,
                         scenario=modeller.emmebank.scenario(scenario_id))
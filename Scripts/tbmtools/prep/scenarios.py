def build_gtfs_base_network(highway_modes, highway_nodes, highway_links, turns,
                            transit_modes, rail_network, link_shape, vehicles,
                            scenario, modeller):
    """ Build a base network for loading GTFS information.

    Adds modes, nodes, links, turns, link shape, and transit vehicles to
    a scenario from mode transaction files, base network transaction
    files, a turn transaction file, a link shape transaction file, and a
    transit vehicle transaction file.

    Parameters
    ----------
        highway_modes : str or Path object
            Path to highway mode transaction file.
        highway_nodes : str or Path object
            Path to highway node base network transaction file.
        highway_links : str or Path object
            Path to highway link base network transaction file.
        turns : str or Path object
            Path to turn transaction file.
        transit_modes : str or Path object
            Path to transit mode transaction file.
        rail_network : str or Path object
            Path to rail base network transaction file.
        link_shape : str or Path object
            Path to link shape transaction file.
        transit_vehicles : str or Path object
            Path to transit vehicle transaction file.
        scenario : inro.emme.database.scenario.Scenario
            Scenario to contain the built network.
        modeller : inro.modeller.Modeller
            Modeller instance of active Emme project.
    """
    # Construct Modeller tools.
    process_mode_file = modeller.tool('inro.emme.data.network.mode.mode_transaction')
    process_network_file = modeller.tool('inro.emme.data.network.base.base_network_transaction')
    process_link_shape_file = modeller.tool('inro.emme.data.network.base.link_shape_transaction')
    process_turn_file = modeller.tool('inro.emme.data.network.turn.turn_transaction')
    process_vehicle_file = modeller.tool('inro.emme.data.network.transit.vehicle_transaction')
    # Build highway.
    process_mode_file(str(highway_modes),
                      scenario=scenario)
    process_network_file(str(highway_nodes),
                         scenario=scenario)
    process_network_file(str(highway_links),
                         scenario=scenario)
    process_turn_file(str(turns),
                      revert_on_error = False,
                      scenario=scenario)
    # Remove truck modes - they unnecessary in this network and conflict with
    # auxiliary transit modes.
    truck_modes = ['T', 'h', 'm', 'l', 'b']
    network = scenario.get_network()
    for m in truck_modes:
        network.delete_mode(m, cascade=True)
    scenario.publish_network(network)
    # Add bus modes to highway links.
    process_mode_file(str(transit_modes),
                      scenario=scenario)
    network = scenario.get_network()
    for link in network.links():
        if network.mode('A') in link.modes:
            if link.volume_delay_func in [1]:
                bus_modes = ['B', 'E', 'P', 'Q', 'L']
            elif link.volume_delay_func in [2, 3, 4, 5, 7, 8]:
                bus_modes = ['E', 'Q']
            else:
                bus_modes = []
        for m in bus_modes:
            link.modes |= set([network.mode(m)])  # Adds the mode to the link.
    scenario.publish_network(network)
    # Build rail.
    process_network_file(str(rail_network),
                         scenario=scenario)
    # Add link shape.
    process_link_shape_file(str(link_shape),
                            revert_on_error=False,
                            scenario=scenario)
    # Add transit vehicles.
    process_vehicle_file(str(vehicles),
                         scenario=scenario)


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

def configure_gtfs_schema(scenario, trip_aggregation, modeller):
    """ Configure a scenario's schema to store GTFS information.

    Creates network fields and extra attributes in transit line and
    transit segment domains for storing imported GTFS data.

    Parameters
    ----------
    scenario : inro.emme.database.scenario.Scenario
        Scenario to contain the GTFS data.
    modeller : inro.modeller.Modeller
        Modeller instance of active Emme project.
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
    if trip_aggregation:
        transit_line_attributes.extend([('@hdw_nt1', 'headway 12am-6am'),
                                        ('@hdw_am', 'headway 6am-9am'),
                                        ('@hdw_md', 'headway 9am-4pm'),
                                        ('@hdw_pm', 'headway 4pm-6pm'),
                                        ('@hdw_nt2', 'headway 6pm-12am')])
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
                         scenario=scenario)
    # Add GTFS network fields to transit segments.
    for fld in transit_segment_fields:
        fld_name = fld[0]
        fld_desc = fld[1]
        create_net_field(network_field_type='TRANSIT_SEGMENT',
                         network_field_atype='STRING',
                         network_field_name=fld_name,
                         network_field_description=fld_desc,
                         overwrite=True,
                         scenario=scenario)
    # Add GTFS extra attributes to transit lines.
    for attr in transit_line_attributes:
        attr_name = attr[0]
        attr_desc = attr[1]
        create_attribute(extra_attribute_type='TRANSIT_LINE',
                         extra_attribute_name=attr_name,
                         extra_attribute_description=attr_desc,
                         overwrite=True,
                         scenario=scenario)
    # Add GTFS extra attributes to transit segments.
    for attr in transit_segment_attributes:
        attr_name = attr[0]
        attr_desc = attr[1]
        create_attribute(extra_attribute_type='TRANSIT_SEGMENT',
                         extra_attribute_name=attr_name,
                         extra_attribute_description=attr_desc,
                         overwrite=True,
                         scenario=scenario)
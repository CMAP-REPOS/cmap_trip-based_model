from tbmtools.transit import config


def build_base_network(rail_network_file, scenario):
    """ Build a base network for loading GTFS information.

    Adds modes, nodes, links, turns, link shape, and transit vehicles to
    a scenario from mode transaction files, base network transaction
    files, a turn transaction file, a link shape transaction file, and a
    transit vehicle transaction file.

    Parameters
    ----------
        rail_network : str or Path object
            Path to rail base network transaction file.
        scenario : inro.emme.database.scenario.Scenario
            Scenario to contain the built network.
    """

    # Construct Modeller tools.
    mode_transaction = config.MODELLER.tool('inro.emme.data.network.mode.mode_transaction')
    base_network_transaction = config.MODELLER.tool('inro.emme.data.network.base.base_network_transaction')
    link_shape_transaction = config.MODELLER.tool('inro.emme.data.network.base.link_shape_transaction')
    turn_transaction = config.MODELLER.tool('inro.emme.data.network.turn.turn_transaction')
    vehicle_transaction = config.MODELLER.tool('inro.emme.data.network.transit.vehicle_transaction')
    
    # Build highway.
    mode_transaction(str(config.DB_DIR.joinpath('data', 'modes.in')),
                     scenario=scenario)
    base_network_transaction(str(config.highway_nodes),
                             scenario=scenario)
    base_network_transaction(str(config.highway_links),
                             scenario=scenario)
    turn_transaction(str(config.DB_DIR.joinpath('data', 'turnp07202016.txt')),
                     revert_on_error = False,
                     scenario=scenario)
    
    # Remove truck modes -- they're unnecessary in this network and
    # conflict with auxiliary transit modes.
    truck_modes = ['T', 'h', 'm', 'l', 'b']
    network = scenario.get_network()
    for m in truck_modes:
        network.delete_mode(m, cascade=True)
    scenario.publish_network(network)
    
    # Add bus modes to highway links.
    with open(config.transit_modes, "r") as f:
        content = f.read()
    modified_content = content.replace("init", "")
    modified_transit_modes = config.PROJ_DIR.joinpath('output', 'tranmodes.txt')
    with open(modified_transit_modes, 'w') as f:
        f.write(modified_content)
    mode_transaction(str(modified_transit_modes),
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
            link.modes |= {network.mode(m)}  # Adds the mode to the link.
    scenario.publish_network(network)
    
    # Build rail.
    base_network_transaction(str(rail_network_file),
                             scenario=scenario)
    
    # Add link shape.
    link_shape_transaction(str(config.link_shape),
                           revert_on_error=False,
                           scenario=scenario)
    
    # Add transit vehicles.
    vehicle_transaction(str(config.transit_vehicles),
                        scenario=scenario)


def configure_schema(scenario):
    """ Configure a scenario's schema to store GTFS information.

    Creates network fields and extra attributes in transit line and
    transit segment domains for storing imported GTFS data.

    Parameters
    ----------
    scenario : inro.emme.database.scenario.Scenario
        Scenario to contain the GTFS data.
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
                               ('@wheelchair_access', 'wheelchair access 0=unknown 1=yes 2=no'),
                               ('@hdw_nt1', 'headway 12am-6am'),
                               ('@hdw_am', 'headway 6am-9am'),
                               ('@hdw_md', 'headway 9am-4pm'),
                               ('@hdw_pm', 'headway 4pm-6pm'),
                               ('@hdw_nt2', 'headway 6pm-12am')]
    transit_segment_fields = [('#stop_name', 'stop name'),
                              ('#emme_line_name', 'default line id'),
                              ('#i_fare_zone', 'fare zone at inode'),
                              ('#j_fare_zone', 'fare zone at jnode')]
    transit_segment_attributes = [('@stop_time', 'inter-service stop time'),
                                  ('@stop_count', 'number of stops on segment'),
                                  ('@split_time', 'proportional travel time on segment'),
                                  ('@zfare', 'zone fare')]

    # Construct Modeller tools.
    create_network_field = config.MODELLER.tool('inro.emme.data.network_field.create_network_field')
    create_extra_attribute = config.MODELLER.tool('inro.emme.data.extra_attribute.create_extra_attribute')

    # Add GTFS network fields to transit lines.
    for fld in transit_line_fields:
        fld_name = fld[0]
        fld_desc = fld[1]
        create_network_field(network_field_type='TRANSIT_LINE',
                             network_field_atype='STRING',
                             network_field_name=fld_name,
                             network_field_description=fld_desc,
                             overwrite=True,
                             scenario=scenario)

    # Add GTFS network fields to transit segments.
    for fld in transit_segment_fields:
        fld_name = fld[0]
        fld_desc = fld[1]
        create_network_field(network_field_type='TRANSIT_SEGMENT',
                             network_field_atype='STRING',
                             network_field_name=fld_name,
                             network_field_description=fld_desc,
                             overwrite=True,
                             scenario=scenario)

    # Add GTFS extra attributes to transit lines.
    for attr in transit_line_attributes:
        attr_name = attr[0]
        attr_desc = attr[1]
        create_extra_attribute(extra_attribute_type='TRANSIT_LINE',
                               extra_attribute_name=attr_name,
                               extra_attribute_description=attr_desc,
                               overwrite=True,
                               scenario=scenario)

    # Add GTFS extra attributes to transit segments.
    for attr in transit_segment_attributes:
        attr_name = attr[0]
        attr_desc = attr[1]
        create_extra_attribute(extra_attribute_type='TRANSIT_SEGMENT',
                               extra_attribute_name=attr_name,
                               extra_attribute_description=attr_desc,
                               overwrite=True,
                               scenario=scenario)


def modify_database(emmebank):
    """Adjust database dimensions to accommodate GTFS data."""
    change_database_dimensions = config.MODELLER.tool('inro.emme.data.database.change_database_dimensions')
    dims = emmebank.dimensions
    new_dims = False
    if dims['transit_lines'] < 40000:
        dims['transit_lines'] = 40000
        new_dims = True
    if dims['transit_segments'] < 2000000:
        dims['transit_segments'] = 2000000
        new_dims = True
    if dims['extra_attribute_values'] < 20000000:
        dims['extra_attribute_values'] = 20000000
        new_dims = True
    if new_dims:
        change_database_dimensions(emmebank_dimensions=dims,
                                   keep_backup=False)
                                   
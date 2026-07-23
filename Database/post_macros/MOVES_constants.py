### Defines constants for the createMOVESinputfile.py file

SOURCE_TYPES = [
    11, # Motorcycle
    21, # Passenger Car
    31, # Passenger Truck
    32, # Light Commercial Truck
    41, # Other Buses
    42, # Transit Bus
    43, # School Bus
    51, # Refuse Truck
    52, # Single Unit Short-haul Truck
    53, # Single Unit Long-haul Truck
    54, # Motor Homes
    61, # Combination Short-haul Truck
    62  # Combination Long-haul Truck
    ]

ROAD_TYPES = [
    2, # Rural Restricted Access
    3, # Rural Unrestricted Access
    4, # Urban Restricted Access
    5  # Urban Unrestricted Access
    ] # note: ROAD_TYPE 1 is off-network, and not included in all tabs except hourly VMT fraction

SPEED_BINS = [
    1, # <2.5 mph
    2, # 2.5 - 7.5 mph
    3, # 7.5 - 12.5 mph
    4, # 12.5 - 17.5 mph
    5, # 17.5 - 22.5 mph
    6, # 22.5 - 27.5 mph
    7, # 27.5 - 32.5 mph
    8, # 32.5 - 37.5 mph
    9, # 37.5 - 42.5 mph
    10, # 42.5 - 47.5 mph 
    11, # 47.5 - 52.5 mph
    12, # 52.5 - 57.5 mph
    13, # 57.5 - 62.5 mph
    14, # 62.5 - 67.5 mph
    15, # 67.5 - 72.5 mph
    16 # >=72.5
]

HOURS = list(range(1,25)) # range end is exclusive

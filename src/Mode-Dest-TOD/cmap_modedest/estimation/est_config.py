from ..util import search_path

## CONFIG ##

n_sampled_dests = 25
# The number of destination to sample during estimation
# This has no effect on application, where there is no sampling used.

EstimationDir = search_path(
	"~/Cambridge Systematics/PROJ CMAP Trip-Based - General/Estimation",
	"~/OneDrive/Cambridge Systematics/PROJ CMAP Trip-Based - General/Estimation",
	"other/possible/path/to/Estimation",
)
# Add local paths to the "Estimation" directory here.
# The contents of this directory itself are not to be published on GitHub

EstimationSkimsDir = search_path(
	"~/LocalGit/cmap-trip-model/Database/emmemat",
	"~/Git/cmap-trip-model/Database/emmemat",
)

############


mode_complete = {
	101:'Walk',
	102:'My own bike',
	103:'Divvy bike',
	104:'Zagster bike',
	201:'Motorcycle/moped',
	202:'Auto / van / truck (as the driver) ',
	203:'Auto / van / truck (as the passenger) ',
	301:'Carpool/vanpool',
	401:'School bus',
	500:'Rail and Bus',
	501:'Bus (CTA, PACE, Huskie Line, Indiana)',
	502:'Dial-a-Ride',
	503:'Call-n-Ride',
	504:'Paratransit',
	505:'Train (CTA, METRA, South Shore Line)',
	506:'Local transit (NIRPC region)',
	509:'Transit (specific mode not reported or imputed)',
	601:'Private shuttle bus',
	701:'Taxi',
	702:'Private limo',
	703:'Private car',
	704:'Uber/Lyft',
	705:'Via/Uber Pool/Lyft Line (shared ride)',
	801:'Airplane',
	997:'[$MODE2_O]',
}

mode_modeled = {
	101: None, # 'Walk',
	102: None, # 'My own bike',
	103: None, # 'Divvy bike',
	104: None, # 'Zagster bike',
	201: 'AUTO',
	202: 'AUTO',
	203: 'AUTO',
	301: 'AUTO',
	401: None, # 'School bus',
	500: 'TRANSIT',
	501: 'TRANSIT',
	502: None, # 'Dial-a-Ride',
	503: None, # 'Call-n-Ride',
	504: None, # 'Paratransit',
	505: 'TRANSIT',
	506: 'TRANSIT',
	509: 'TRANSIT',
	601: None, # 'Private shuttle bus',
	701: 'TNC', # 'Taxi',
	702: None, # 'Private limo',
	703: None, # 'Private car',
	704: 'TNC', # 'Uber/Lyft',
	705: 'TNC', # 'Via/Uber Pool/Lyft Line (shared ride)',
	801: None, # Airplane
	997: None, # Fill-in-the-blank
}


mode_modeled5 = {
	101: None, # 'Walk',
	102: None, # 'My own bike',
	103: None, # 'Divvy bike',
	104: None, # 'Zagster bike',
	201: 'AUTO',
	202: 'AUTO',
	203: 'AUTO',
	301: 'AUTO',
	401: None, # 'School bus',
	500: 'TRANSIT',
	501: 'TRANSIT',
	502: None, # 'Dial-a-Ride',
	503: None, # 'Call-n-Ride',
	504: None, # 'Paratransit',
	505: 'TRANSIT',
	506: 'TRANSIT',
	509: 'TRANSIT',
	601: None, # 'Private shuttle bus',
	701: 'TAXI', # 'Taxi',
	702: None, # 'Private limo',
	703: None, # 'Private car',
	704: 'TNC1', # 'Uber/Lyft',
	705: 'TNC2', # 'Via/Uber Pool/Lyft Line (shared ride)',
	801: None, # Airplane
	997: None, # Fill-in-the-blank
}


mode_modeled7 = {
	101: 'WALK', # 'Walk',
	102: 'BIKE', # 'My own bike',
	103: 'BIKE', # 'Divvy bike',
	104: 'BIKE', # 'Zagster bike',
	201: 'AUTO',
	202: 'AUTO',
	203: 'AUTO',
	301: 'AUTO',
	401: None, # 'School bus',
	500: 'TRANSIT',
	501: 'TRANSIT',
	502: None, # 'Dial-a-Ride',
	503: None, # 'Call-n-Ride',
	504: None, # 'Paratransit',
	505: 'TRANSIT',
	506: 'TRANSIT',
	509: 'TRANSIT',
	601: None, # 'Private shuttle bus',
	701: 'TAXI', # 'Taxi',
	702: None, # 'Private limo',
	703: None, # 'Private car',
	704: 'TNC1', # 'Uber/Lyft',
	705: 'TNC2', # 'Via/Uber Pool/Lyft Line (shared ride)',
	801: None, # Airplane
	997: None, # Fill-in-the-blank
}
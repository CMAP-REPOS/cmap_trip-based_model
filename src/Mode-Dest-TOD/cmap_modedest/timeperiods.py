

# 1. The ten hour late evening-early morning off-peak period (8:00 p.m. to 6:00 a.m.);
# 2. The shoulder hour preceding the AM peak hour (6:00 to 7:00 a.m.);
# 3. The AM peak two hours (7:00 to 9:00 a.m.);
# 4. The shoulder hour following the AM peak period (9:00 to 10:00 a.m);
# 5. A five hour midday period (10:00 a.m. to 2:00 p.m.);
# 6. The two hour shoulder period preceding the PM peak period (2:00 to 4:00 p.m.);
# 7. The PM peak two hours (4:00 to 6:00 p.m.), and;
# 8. The two hour shoulder period following the PM peak period (6:00 to 8:00 p.m.).

timeperiod_names = [
	'NIGHT',
	'AM_PRE',
	'AM_PEAK',
	'AM_POST',
	'MIDDAY',
	'PM_PRE',
	'PM_PEAK',
	'PM_POST',
]

timeperiods = {i:name for i, name in enumerate(timeperiod_names)}

hours_by_timeperiod = {
	0:   0,
	1:   0,
	2:   0,
	3:   0,
	4:   0,
	5:   0,
	6:   1,
	7:   2,
	8:   2,
	9:   3,
	10:  4,
	11:  4,
	12:  4,
	13:  4,
	14:  4,
	15:  5,
	16:  5,
	17:  6,
	18:  6,
	19:  7,
	20:  7,
	21:  0,
	22:  0,
	23:  0,
}


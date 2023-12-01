import sys
from tbmtools.results import trip_roster

def main():
    projdir = sys.argv[1]
    outdir = sys.argv[2]
    trip_roster_path = trip_roster.export(projdir, outdir)
    print(trip_roster_path)

if __name__ == '__main__':
    main()
import subprocess
import textwrap

from .config import PROJ_DIR


def build_networks():
    print("Creating time-of-day highway and transit networks...")
    script_path = PROJ_DIR.joinpath('Database', 'prep_macros', 'initialize_scenarios.py')
    command = ['uv', 'run', str(script_path)]
    subprocess.run(command, check=True)
    print('Complete.')


def initialize_database(val):
    print("Cleaning up databank...")
    report_path = PROJ_DIR.joinpath('Database', 'cleanup.rpt')
    if report_path.exists():
        report_path.unlink()
    script_path = PROJ_DIR.joinpath('Database','useful_macros', 'cleanup_for_rerun.py')
    command = ['uv', 'run', str(script_path), str(val)]
    subprocess.run(command,
                   check=True,
                   stdout=open(report_path, 'w'))
    reports_path = PROJ_DIR.joinpath('Database', 'reports')
    if reports_path.exists():
        reports_path.unlink()
    print("Complete.")


def verify_transit_builds(val):
    print("Checking for transit network input errors...")
    trnscen = int(val) + 21
    maxscen = int(val) + 27
    build_transit_error = PROJ_DIR.joinpath('Database', 'report', 'build_transit.error')
    if build_transit_error.exists():
        build_transit_error.unlink()
    while trnscen <= maxscen:
        script_path = PROJ_DIR.joinpath('Database','prep_macros', 'build_transit_error_check.py')
        command = ['uv', 'run', str(script_path), str(trnscen)]
        subprocess.run(command,
                       cwd=PROJ_DIR.joinpath('Database'),
                       check=True)
        if build_transit_error.exists():
            print(f"Errors found in transit network {trnscen}.")
            print(f"Review report/build_{trnscen}transit.rpt and rebuild transit network, then rerun.")
            input("Press Enter to exit...")
            return
        trnscen += 2
    print("No transit network errors found.")
    print("Complete.")


def main(settings):
    # Configure
    ver = settings.get('model_version')
    val = settings.get('scenario_code')
    transactFilePath = settings.get('transactionFilePath')
    print(textwrap.dedent(f"""
                              --- Model Run Settings ---
                          Conformity version = {ver}
                          Scenario = {val}
                          Location of network transaction files = {transactFilePath}
                          """))
    ok = ''
    while ok not in ['y', 'n']:
        ok = input(f"Build time-of-day networks for scenario {val}? (y/n): ").strip().lower()
    if ok == 'n':
        print("Model run canceled by user.")
        return
    # Run
    initialize_database(val)
    build_networks()
    verify_transit_builds(val)
    print("Networks built successfully!")


if __name__ == "__main__":
    main()
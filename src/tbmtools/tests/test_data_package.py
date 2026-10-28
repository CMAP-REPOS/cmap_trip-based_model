import filecmp
from itertools import islice, zip_longest
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
import zipfile

sys.path.append(str(Path(__file__).parents[1].joinpath('src')))

from tbmtools import project as tbm
from tbmtools.matrix import person_trip, skim, vehicle_trip
from tbmtools.network import highway, transit


class TestDataPackage(unittest.TestCase):

    def test_vehicle_trip_matrices(self):
        with tempfile.TemporaryDirectory() as out_dir:
            expected_trips_dir = Path(out_dir).joinpath('expected_trips')
            with zipfile.ZipFile(expected_dir.joinpath('trips_c25q2_700.zip'), 'r') as z:
                z.extractall(expected_trips_dir)
            test_trips_dir = vehicle_trip.export_auto(Path(out_dir), 700, modeller)
            # Compare the directories.
            comp = filecmp.dircmp(expected_trips_dir,
                                  test_trips_dir,
                                  ignore=[f.name for f in expected_trips_dir.iterdir() if f.name not in [f'mf{n}.csv' for n in range(4, 11)]])
            self.assertEqual(comp.left_only, [], 
                             f'Files only in expected: {comp.left_only}')
            self.assertEqual(comp.right_only, [], 
                             f'Files only in test: {comp.right_only}')
            self.assertEqual(comp.diff_files, [], 
                             f'Files with different data: {comp.diff_files}')
            
    def test_transit_skim_matrices(self):
        with tempfile.TemporaryDirectory() as out_dir:
            expected_skims_dir = Path(out_dir).joinpath('expected_skims')
            with zipfile.ZipFile(expected_dir.joinpath('skims_c25q2_700.zip'), 'r') as z:
                z.extractall(expected_skims_dir)
            test_skims_dir = skim.export_transit(Path(out_dir), 700, modeller)
            # Compare the directories.
            comp = filecmp.dircmp(expected_skims_dir,
                                  test_skims_dir,
                                  ignore=[f.name for f in expected_skims_dir.iterdir() if f.name not in [f'mf{n}.csv' for n in [822, 823, 838, 830, 828, 837, 922, 923, 938, 930, 928, 937]]])
            self.assertEqual(comp.left_only, [], 
                             f'Files only in expected: {comp.left_only}')
            self.assertEqual(comp.right_only, [], 
                             f'Files only in test: {comp.right_only}')
            self.assertEqual(comp.diff_files, [], 
                             f'Files with different data: {comp.diff_files}')
            
    def test_highway_skim_matrices(self):
        with tempfile.TemporaryDirectory() as out_dir:
            expected_skims_dir = Path(out_dir).joinpath('expected_skims')
            with zipfile.ZipFile(expected_dir.joinpath('skims_c25q2_700.zip'), 'r') as z:
                z.extractall(expected_skims_dir)
            test_skims_dir = skim.export_highway(Path(out_dir), 700, modeller)
            # Compare the directories.
            comp = filecmp.dircmp(expected_skims_dir,
                                  test_skims_dir,
                                  ignore=[f.name for f in expected_skims_dir.iterdir() if f.name not in [f'mf{n}.csv' for n in range(44, 48)]])
            self.assertEqual(comp.left_only, [], 
                             f'Files only in expected: {comp.left_only}')
            self.assertEqual(comp.right_only, [], 
                             f'Files only in test: {comp.right_only}')
            self.assertEqual(comp.diff_files, [], 
                             f'Files with different data: {comp.diff_files}')
            
    def test_trip_roster(self):
        with tempfile.TemporaryDirectory() as out_dir:
            expected_trip_roster_file = Path(expected_dir).joinpath('trip_roster_c25q2_700.csv')
            test_trip_roster_file = person_trip.export_trip_roster(Path(modeller.desktop.project_file_name()).parent, Path(out_dir), 'trip_roster')
            # Compare the directories.
            comp = filecmp.cmp(expected_trip_roster_file,
                               test_trip_roster_file,
                               shallow=False)
            self.assertTrue(comp)

    def test_auto_person_trip_matrices(self):
        with tempfile.TemporaryDirectory() as out_dir:
            expected_trips_dir = Path(out_dir).joinpath('expected_trips')
            expected_hovtrips_dir = Path(out_dir).joinpath('expected_hov_trips')
            with zipfile.ZipFile(expected_dir.joinpath('trips_c25q2_700.zip'), 'r') as z1:
                z1.extractall(expected_trips_dir)
            with zipfile.ZipFile(expected_dir.joinpath('hovtrips_c25q2_700.zip'), 'r') as z2:
                z2.extractall(expected_hovtrips_dir)
            test_trip_roster_file = person_trip.export_trip_roster(Path(modeller.desktop.project_file_name()).parent, Path(out_dir), 'trip_roster')
            test_trips_dir, test_hovtrips_dir = person_trip.export_auto(Path(modeller.desktop.project_file_name()).parent, Path(out_dir), test_trip_roster_file)
            # Compare the directories.
            comp1 = filecmp.dircmp(expected_trips_dir,
                                   test_trips_dir,
                                   ignore=[f'mf{n}.csv' for n in [4, 5, 6, 7, 8, 9, 10, 38, 39, 40, 41, 42, 43]] + ['hov_trips'])
            self.assertEqual(comp1.left_only, [], 
                             f'Files only in expected: {comp1.left_only}')
            self.assertEqual(comp1.right_only, [], 
                             f'Files only in test: {comp1.right_only}')
            self.assertEqual(comp1.diff_files, [], 
                             f'Files with different data: {comp1.diff_files}')
            comp2 = filecmp.dircmp(expected_hovtrips_dir,
                                   test_hovtrips_dir)
            self.assertEqual(comp2.left_only, [], 
                             f'Files only in expected: {comp2.left_only}')
            self.assertEqual(comp2.right_only, [], 
                             f'Files only in test: {comp2.right_only}')
            self.assertEqual(comp2.diff_files, [], 
                             f'Files with different data: {comp2.diff_files}')
            
    def test_transit_person_trip_matrices(self):
        with tempfile.TemporaryDirectory() as out_dir:
            expected_trips_dir = Path(out_dir).joinpath('expected_trips')
            with zipfile.ZipFile(expected_dir.joinpath('trips_c25q2_700.zip'), 'r') as z:
                z.extractall(expected_trips_dir)
            test_trips_dir = person_trip.export_transit(Path(modeller.desktop.project_file_name()).parent, Path(out_dir), 700, modeller)
            # Compare the directories.
            comp = filecmp.dircmp(expected_trips_dir,
                                  test_trips_dir,
                                  ignore=[f.name for f in expected_trips_dir.iterdir() if f.name not in [f'mf{n}.csv' for n in range(38, 44)]])
            self.assertEqual(comp.left_only, [], 
                             f'Files only in expected: {comp.left_only}')
            self.assertEqual(comp.right_only, [], 
                             f'Files only in test: {comp.right_only}')
            self.assertEqual(comp.diff_files, [], 
                             f'Files with different data: {comp.diff_files}')
            
    def test_transit_network_transaction_files(self):
        with tempfile.TemporaryDirectory() as out_dir:
            expected_transit_network_dir = Path(out_dir).joinpath('expected_transit_network')
            with zipfile.ZipFile(expected_dir.joinpath('emmenet_transit_c25q2_700.zip'), 'r') as z:
                z.extractall(expected_transit_network_dir)
            test_transit_network_dir = transit.export_all(Path(out_dir), 700, modeller)[0]
            # Compare the directories.
            comp = filecmp.dircmp(expected_transit_network_dir,
                                  test_transit_network_dir,
                                  ignore=['transit_op-700', 'transit_pk-700'])
            self.assertEqual(comp.left_only, [], 
                             f'Files only in expected: {comp.left_only}')
            self.assertEqual(comp.right_only, [], 
                             f'Files only in test: {comp.right_only}')
            if comp.diff_files:
                line_diff_files = []
                for file in comp.diff_files:
                    if diff_skip_lines(expected_transit_network_dir.joinpath(file), test_transit_network_dir.joinpath(file), lines_to_skip=4):
                        line_diff_files.append(file)
                comp.diff_files = line_diff_files
            self.assertEqual(comp.diff_files, [], 
                             f'Files with different data: {comp.diff_files}')
            
    def test_higway_network_transaction_files(self):
        with tempfile.TemporaryDirectory() as out_dir:
            expected_highway_network_dir = Path(out_dir).joinpath('expected_highway_network')
            with zipfile.ZipFile(expected_dir.joinpath('emmenet_highway_c25q2_700.zip'), 'r') as z:
                z.extractall(expected_highway_network_dir)
            test_highway_network_dir = highway.export_all(Path(out_dir), 700, modeller)[0]
            # Compare the directories.
            comp = filecmp.dircmp(expected_highway_network_dir,
                                  test_highway_network_dir)
            self.assertEqual(comp.left_only, [], 
                             f'Files only in expected: {comp.left_only}')
            self.assertEqual(comp.right_only, [], 
                             f'Files only in test: {comp.right_only}')
            if comp.diff_files:
                line_diff_files = []
                for file in comp.diff_files:
                    if diff_skip_lines(expected_highway_network_dir.joinpath(file), test_highway_network_dir.joinpath(file), lines_to_skip=4):
                        line_diff_files.append(file)
                comp.diff_files = line_diff_files
            self.assertEqual(comp.diff_files, [], 
                             f'Files with different data: {comp.diff_files}')


def diff_skip_lines(file1, file2, lines_to_skip=1):
    line_diff = False  # Files are identical
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        # Skip the specified number of lines
        skipped_f1 = islice(f1, lines_to_skip, None)
        skipped_f2 = islice(f2, lines_to_skip, None)
        with open(Path(__file__).parent.joinpath(f'diff_{file1.name}'), 'w') as f3:
            f3.write('expected, test\n')
            # Compare remaining lines side-by-side
            for line1, line2 in zip_longest(skipped_f1, skipped_f2):
                if line1 != line2:
                    f3.write(f'{repr(line1)}, {repr(line2)}\n')
                    line_diff = True  # Files differ
        if not line_diff:
            Path(__file__).parent.joinpath(f'diff_{file1.name}').unlink()

    return line_diff


def setUpModule():
    # Connect to testing project.
    data_dir = Path(__file__).parent.joinpath('data')
    global modeller
    modeller = tbm.connect(data_dir)
    # Extract expected test data.
    expected_file = sorted(data_dir.glob('*.zip'))[0]
    global expected_dir
    expected_dir = data_dir.joinpath('expected')
    expected_dir.mkdir()
    with zipfile.ZipFile(expected_file, 'r') as z:
        z.extractall(expected_dir)


def tearDownModule():
    # Delete expected test data directory.
    shutil.rmtree(expected_dir)


if __name__ == '__main__':
    unittest.main()

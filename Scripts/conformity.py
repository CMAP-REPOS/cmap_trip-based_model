"""
Module for conformity analysis modeling.

conformity.py
03/23/2021
N. Ferguson

Functions:
    rename_project() - Changes project titles and names
"""

# Import modules from standard library
import os
import glob

# Import modules from Emme API
import inro.emme.desktop.app as _app
import inro.emme.database.emmebank as _emmebank

# Define year codes
analysis_years = {
  '100': '2015',
  '200': '2020',
  '300': '2025',
  '400': '2030',
  '500': '2035',
  '600': '2040',
  '700': '2050'
}

# Use the directory of this file as a base for relative paths
module_dir = os.path.dirname(__file__)

def rename_project(cycle_code, year_code):
    """
    Renames database, project, and project file components of the
    containing Emme project with the specified conformity cycle code and
    analysis year code.
    """

    # Get year from year code
    year = analysis_years[year_code]
    
    # Find the project file and the database
    project_dir = os.path.join(module_dir, '..')
    emp_files = glob.glob(os.path.join(project_dir, '*.emp'))
    project_file = emp_files[0]
    db = os.path.join(project_dir, 'Database', 'emmebank')

    # Rename the database
    with _emmebank.Emmebank(db) as my_emmebank:
        print('Renaming ' + my_emmebank.title + ' database to...')
        my_emmebank.title = '{} Scenario {} [{}]'.format(cycle_code, year_code, year)
        print(my_emmebank.title + '\n')

    # Use the project file to open Desktop
    my_desktop = _app.start_dedicated(
      project=project_file,
      visible=False,
      user_initials='INRO'
    )
    
    # Rename the project
    print('Renaming ' + my_desktop.project.name + ' project to...')
    my_desktop.project.name = '{}_{}'.format(cycle_code, year_code)
    print(my_desktop.project.name + '\n')

    # Save the project changes and close Desktop
    my_desktop.project.save()
    my_desktop.close()

    # Rename the project files
    print('Renaming project files...')
    new_emp_name = '{}_{}.emp'.format(cycle_code, year_code)
    new_prj_name = new_emp_name + '.prj'
    os.rename(project_file, os.path.join(project_dir, new_emp_name))
    os.rename(project_file + '.prj', os.path.join(project_dir, new_prj_name))

    print('Done.')
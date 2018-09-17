import fnmatch
import os

import pandas as pd

#Root directory: path to where the flygram analysis folder is located.
root_dir = '/Users/sophiasong/Documents/KaunLab/flygram/flygram_analysis/'

def convert_excel_to_csv(excel_directory, csv_directory):
    """
    Convert excel files to csv files.
    Params:
        excel_directory: Path of directory containing flygram excel files.
            NOTE: The header column of the excel file must not have capitalization or spaces
        csv_directory: Path of directory to store converted csvs
    """
    # For each Excel files in the excel directory
    for file in fnmatch.filter(os.listdir(excel_directory), '*.xls'):
        # Read the excel file
        data_xls = pd.read_excel(excel_directory+file, 'Sheet1', index_col=None)
        # Convert the excel file to a csv and replace spaces in the name with underscore
        data_xls.to_csv('{}/{}.csv'.format(csv_directory, file.replace(" ", "_").replace('.xls', "")), encoding='utf-8', index=False)

def get_group_names(directory):
    group_list = []
    for file in fnmatch.filter(os.listdir(directory), '*.csv'):

        file_name = file.split("_")[0]
        group_list.append(file_name)
    return group_list

def get_group_df(group):

    column_list = ['time', 'group', 'activity']

    baseline_df = pd.DataFrame(columns=column_list)
    ethanol_df = pd.DataFrame(columns=column_list)
    recovery_df = pd.DataFrame(columns=column_list)

    print (group)
    df = pd.read_csv(root_dir + 'csv_flygram_data/' + genotype + '/' + group + '_alldata.csv')

    time_series = pd.Series(data=list(map(lambda x: int(x.split(',')[0].lstrip('(')), df.iloc[:,0])), name='time_elapsed')
    df.update(time_series)
    baseline_activity = df.iloc[14, 1:]

    baseline_df['activity'] = baseline_activity
    baseline_df['time'] = 'baseline'
    baseline_df['group'] = group

    ethanol_activity = df.iloc[59, 1:]
    ethanol_df['activity'] = ethanol_activity
    ethanol_df['time'] = 'ethanol'
    ethanol_df['group'] = group

    recovery_activity = df.iloc[109, 1:]
    recovery_df['activity'] = recovery_activity
    recovery_df['time'] = 'recovery'
    recovery_df['group'] = group

    group_df = pd.concat([baseline_df, ethanol_df, recovery_df])
    return group, group_df

if __name__=="__main__":

    subfolders = os.walk(root_dir + 'excel_flygram_data/')
    subfolders = [i for i in subfolders if (root_dir + 'excel_flygram_data/') not in i]
    genotypes = list(map(lambda x: os.path.basename(os.path.normpath(x)), [x[0] for x in subfolders]))
    for genotype in genotypes:

        convert_excel_to_csv(root_dir + 'excel_flygram_data/' + genotype + '/', root_dir + 'csv_flygram_data/' + genotype + '/')
        group_list = get_group_names(root_dir + 'csv_flygram_data/' + genotype + '/')

        total_df = pd.DataFrame()
        group_name_1 = ''
        for group in group_list:
            group_name, group_df = get_group_df(group)
            total_df = pd.concat([total_df, group_df])

        total_df.to_csv(root_dir + 'processed_flygram_data/' + genotype + '/' + 'processed_{file_name}.csv'.format(file_name=genotype), index=False)

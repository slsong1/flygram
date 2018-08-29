import fnmatch
import os

import pandas as pd
import scipy.stats as stats
import statsmodels.formula.api as smf
from statsmodels.stats.multicomp import pairwise_tukeyhsd

root_dir = '/Users/sophiasong/Documents/KaunLab/flygram/flygram_analysis/'

def convert_excel_to_csv(excel_directory, csv_directory):
    for file in fnmatch.filter(os.listdir(excel_directory), '*.xls'):
        data_xls = pd.read_excel(excel_directory+file, 'Sheet1', index_col=None)
        data_xls.to_csv('{}/{}.csv'.format(csv_directory, file.replace(" ", "_").replace('.xls', "")), encoding='utf-8', index=False)

def get_groups(csv_directory):
    group_list = []
    for file in fnmatch.filter(os.listdir(csv_directory), '*.csv'):
        group_list.append(file.split("_")[0])

    group_list.append('time')

    return group_list

def get_groups(directory):
    group_name_list = []
    for file in fnmatch.filter(os.listdir(directory), '*.csv'):
        file_name = file.split("_")[0]
        group_name = ''.join([i for i in file_name if i not in "ywshi."])
        group_name_list.append(group_name)
    return group_name_list

def get_timepoint(group, directory, index_start, index_stop):

    column_list = ['time', 'group', 'activity']
    total_df = pd.DataFrame(columns = column_list)

    for file in fnmatch.filter(os.listdir(directory), '*.csv'):
        file_name = file.split("_")[0]
        if group in file_name:
            df = pd.read_csv(root_dir + 'csv_flygram_data/'+file)

            df_time = df.iloc[index_start:index_stop, 0]
            time_series = list(map(lambda x: int(x.split(',')[1].rstrip(']')), df_time))

            timepoint = pd.DataFrame()

            timepoint['activity'] = df.iloc[index_start:index_stop, 1:].mean(axis=1)
            timepoint['group']=file_name
            timepoint['time'] = time_series

            total_df = total_df.append(timepoint, ignore_index=True, sort=False)
    total_df = total_df[['time', 'group', 'activity']]
    return total_df

def get_anova_stats(df):

    groups = list(df.group.unique())
    print (groups)

    result = stats.f_oneway(df[df.group == groups[0]]['activity'],
                   df[df.group == groups[1]]['activity'],
                   df[df.group == groups[2]]['activity'])
    print (result)
    p_value = result.pvalue
    return result, p_value

def get_single_anova_stats(df):
    md = smf.mixedlm('activity ~ group + time', data = df, groups=df['group']).fit()
    print(md.summary())

def tukey_test(df):
    tukey = pairwise_tukeyhsd(endog=df['activity'],
                              groups=df['group'],
                              alpha=0.05)
    print(tukey.summary())

if __name__=="__main__":

    #TODO: Add functionality to take in multiple excel files in the directories

    convert_excel_to_csv('/Users/sophiasong/Documents/KaunLab/flygram/flygram_analysis/excel_flygram_data/', '/Users/sophiasong/Documents/KaunLab/flygram/flygram_analysis/csv_flygram_data/')
    groups = get_groups(root_dir + 'csv_flygram_data/')
    groups = set(groups)
    for group in groups:

        baseline_df = get_timepoint(group, root_dir + 'csv_flygram_data/', 0, 30)

        baseline_df.to_csv(root_dir + 'processed_flygram_data/{file_name}_{timepoint}.csv'.format(file_name=group, timepoint='baseline'), index=False)

        ethanol_df = get_timepoint(group, root_dir + 'csv_flygram_data/', 29, 90)
        ethanol_df.to_csv(
            root_dir + 'processed_flygram_data/{file_name}_{timepoint}.csv'.format(file_name=group,
                                                                                    timepoint='ethanol'), index=False)
        recovery_df = get_timepoint(group, root_dir + 'csv_flygram_data/', 89, -1)
        recovery_df.to_csv(
            root_dir + 'processed_flygram_data/{file_name}_{timepoint}.csv'.format(file_name=group,
                                                                                    timepoint='recovery'), index=False)
        print ("baseline stats")
        get_anova_stats(baseline_df)
        print ("ethanol stats")
        get_anova_stats(ethanol_df)
        print ("recovery stats")
        get_anova_stats(recovery_df)

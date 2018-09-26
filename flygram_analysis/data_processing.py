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
    """

    :param directory:
    :return:
    """
    group_list = []
    for file in fnmatch.filter(os.listdir(directory), '*.csv'):

        file_name = file.split("_")[0]
        group_list.append(file_name)
    return group_list

def get_group_df(group, genotype):
    """

    :param group:
    :param genotype:
    :return:
    """
    print (genotype)
    inf = genotype.split("_")
    info = "_" + inf[1] + "_" + inf[2]
    genotype_name = genotype.split("_")[0]
    if ('yw.' + genotype_name) in group:
        factor = 1
    if ('yw.shi' + genotype_name) in group:
        factor = 2
    if ('shi.' + genotype_name) in group:
        factor = 3

    total_df = pd.DataFrame()

    df = pd.read_csv(root_dir + 'csv_flygram_data/' + genotype + '/' + group + info + '_alldata.csv')

    df = df.iloc[:, 1:]

    total_df = df.transpose()
    cols = list(total_df.columns.values)
    total_df['group'] = pd.Series(group, index=total_df.index)
    total_df['factor'] = pd.Series(factor, index=total_df.index)


    total_df = total_df[['group', 'factor'] + cols]

    return group, total_df

def sort_list(genotype, ls):
    """

    :param genotype:
    :param ls:
    :return:
    """
    genotype_name = genotype.split("_")[0]
    print (ls)
    print ("sorting")
    new_list = [0, 0, 0]
    for item in ls:
        if ('yw.'+genotype_name) in item:
            new_list.insert(0, item)
        if ('yw.shi' + genotype_name) in item:
            new_list.insert(1, item)
        if ('shi.'+genotype_name) in item:
            new_list.insert(2, item)

    new_list = list(filter(lambda a: a != 0, new_list))
    return new_list


if __name__=="__main__":

    subfolders = os.walk(root_dir + 'excel_flygram_data/')
    subfolders = [i for i in subfolders if (root_dir + 'excel_flygram_data/') not in i]
    genotypes = list(map(lambda x: os.path.basename(os.path.normpath(x)), [x[0] for x in subfolders]))

    for genotype in genotypes:
        print (genotype)
        convert_excel_to_csv(root_dir + 'excel_flygram_data/' + genotype + '/', root_dir + 'csv_flygram_data/' + genotype + '/')
        group_list = get_group_names(root_dir + 'csv_flygram_data/' + genotype + '/')
        print (group_list)
        sorted= sort_list(genotype, group_list)

        total_df = pd.DataFrame()
        group_name_1 = ''

        for group in sorted:
            print ("group: {}".format(group))
            group_name, group_df = get_group_df(group, genotype)
            total_df = pd.concat([total_df, group_df])

        header = pd.DataFrame(pd.Series(range(0, total_df.shape[1] * 10, 10)), list(total_df.columns.values)).transpose()
        total_df = header.append(total_df)
        total_df.to_csv(root_dir + 'processed_flygram_data/' + genotype + '/' + 'processed_{file_name}.csv'.format(file_name=genotype), header=False, index=False)

#TODO: Path manager to make directories and automatically combine them

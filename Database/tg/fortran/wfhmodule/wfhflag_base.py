# coding=utf-8

# this flags tbm people as usualwfh or tc14
import pandas as pd
import numpy as np
import sys
import os

# pathways
savedir = sys.argv[2]
assert os.path.exists(savedir) == True, "savedir not valid"
os.chdir(sys.argv[1])
synpoppath = "synthetic_persons.zip"
synhhpath = "synthetic_households.zip"
popsynhhpath = savedir + "/POPSYN_HH.csv"
incdistpath = "incdist.csv"
indmixpath = "indusmix.csv"
indpxwalkpath = "indp_naics.csv"

# save additional output files?
savefiles = sys.argv[3]

# major parameters - source: mdt + nirpc survey (which is higher than PUMS data...)
# percent of all workers
usualwfhpct = float(sys.argv[4])
tc14pct = float(sys.argv[5])

# set seedvalue
seedvalue = 2
np.random.seed(seed=seedvalue)

# place industries in one of two income distributions types
# high is based on industries that had >60% of workers in income group 4
# low is based on other industries
lowlist = ['11', '21', '44-45', '48-49', '56', '61', '62', '71', '72', '81']
highlist = ['22', '23', '31-33', '42', '51', '52', '53', '54', '55', '92']

# setup education weights for usualwfh (low to high)
eduw = [0.177, 0.246, 0.576]
# setup education weights for usualwfh (low to high, fine they don't sum to 1)
eduwtc = [0.049, 0.114, 0.720]

# set distribution of tc14 into the 4 days
tcportions = [0.575, 0.254, 0.112, 0.059]

print('setting up for wfhflag script...')
# read in files
dfpop = pd.read_csv(synpoppath)
dfhh = pd.read_csv(synhhpath, dtype={'MV': object})
indpxwalk = pd.read_csv(indpxwalkpath)
indmix = pd.read_csv(indmixpath)
incdist = pd.read_csv(incdistpath)

# industry mix - source: mdt + nirpc survey (rescaled after removing industries -7, -8, and 97 to sum to 1)
indmix1 = indmix[indmix['cat'] == 'usualwfh'][['indus', 'pct']]
indmix2 = indmix[indmix['cat'] == 'tc14'][['indus', 'pct']]

# merge naics2, income4, edu, get workers
dfpop = dfpop.merge(indpxwalk, on='INDP', how='left')
dfpop = dfpop.merge(dfhh[['household_id', 'HINCP19']], on='household_id', how='left')
dfpop['inccat4'] = pd.cut(dfpop.HINCP19, bins=[-99999, 30000, 60000, 100000, 2000000], right=False, labels=[1, 2, 3, 4])

workers = dfpop[dfpop['JWTR'] != 'bb'].copy()
workers.JWTR = workers.JWTR.astype(float).astype(int)
workers.SCHL = workers.SCHL.astype(float).astype(int)
workers.loc[workers.SCHL.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17]), 'edu'] = 1
workers.loc[workers.SCHL.isin([18, 19, 20]), 'edu'] = 2
workers.loc[workers.SCHL.isin([21, 22, 23, 24]), 'edu'] = 3
workers.loc[:, 'selected'] = 0

# overall targets
targetwfh = round(len(workers) * usualwfhpct, 0)
targettc14 = round(len(workers) * tc14pct, 0)

workers = workers[workers['ESR'] != '4']

# functions
def portionout(df, pctcol, total, colname):
    """
    divides up a total based on a percent column into a new column (no decimals)
    :param df: input dataframe
    :param pctcol: name of column that has percentages
    :param total: can be name of column that has totals, or a numeric total that the percentages will be applied to
    :param colname: the name of the result column
    :return: dataframe with targetdec, targetint, decimal, and colname columns
    """
    df = df.copy()
    indcounts = workers[workers.selected == 0].groupby('naics2').agg({'household_id':'count'}).reset_index()
    indcounts.rename({'household_id':'sectortotal'}, axis=1, inplace=True)


    if isinstance(total, int) or isinstance(total, float):
        df['targetdec'] = df[pctcol] * total

        df2 = df.merge(indcounts, left_on='indus', right_on='naics2')
        df2['testtot'] = df2['sectortotal'] - df2['targetdec']

        if len(df2[df2.testtot < 0]) == 0:
            pass
        else:
            # don't go all the way up to max to prevent same issue further down the line
            df2.loc[df2.testtot < 0, 'newpct'] = (df2.sectortotal - 10) / total
            df2.loc[df2.testtot < 0, 'pctdiff'] = df2[pctcol] - df2.newpct
            todistribute = df2.pctdiff.sum()
            # normalize unchanged pcts to one to distribute out the missing pct
            newdenom = 1 - df2.loc[df2.testtot < 0][pctcol].sum()
            df2.loc[df2.testtot >=0, 'pctfordist'] = df2[pctcol] / newdenom
            df2.loc[df2.testtot >= 0, 'newpct'] = (df2.pctfordist * todistribute) + df2[pctcol]
            df2[pctcol] = df2['newpct']
            df2['targetdec'] = df2[pctcol] * total

        df2['targetint'] = df2['targetdec'].astype('int')
        df2['decimal'] = df2['targetdec'] - df2['targetint']
        leftover = total - df2['targetint'].sum()
        df2.sort_values('decimal', ascending=False, inplace=True)
        df2.reset_index(drop=True, inplace=True)
        df2.loc[df2.index < leftover, 'targetint'] = df2.targetint + 1
        assert (total - df2.targetint.sum()) == 0, "Targets do not sum to total"
        df2.rename({'targetint': colname}, axis=1, inplace=True)
        return df2

    elif isinstance(total, str):
        df['targetdec'] = df[pctcol] * df[total]

        df2 = df.merge(indcounts, left_on='indus', right_on='naics2')
        df2['testtot'] = df2['sectortotal'] - df2['targetdec']

        if len(df2[df2.testtot < 0]) == 0:
            pass
        else:
            df2.loc[df2.testtot < 0, 'newpct'] = (df2.sectortotal - 10) / df[total]
            df2.loc[df2.testtot < 0, 'pctdiff'] = df2[pctcol] - df2.newpct
            todistribute = df2.pctdiff.sum()
            df2.loc[df2.testtot >= 0, 'newpct'] = df2[pctcol] * (1 + todistribute)
            df2[pctcol] = df2['newpct']
            df2['targetdec'] = df2[pctcol] * df2[total]

        df2['targetint'] = df2['targetdec'].astype('int')
        df2['decimal'] = df2['targetdec'] - df2['targetint']

        totallist = [x for x in df2[total].unique()]
        piecelist = []
        for i in totallist:
            selection = df2[df2[total] == i].copy()
            leftover = i - selection['targetint'].sum()
            if leftover != 0:
                selection.sort_values('decimal', ascending=False, inplace=True)
                selection.reset_index(drop=True, inplace=True)
                selection.loc[selection.index < leftover, 'targetint'] = selection.targetint + 1
                assert (i - selection.targetint.sum()) == 0, "Targets do not sum to total"
            piecelist.append(selection)
        df2 = pd.concat(piecelist)
        df2.rename({'targetint': colname}, axis=1, inplace=True)
        return df2


def settargets(indmix, overalltarget):
    """
    use portionout function to set industry/income targets for group of interest (usualwfh or tc14)
    :param indmix: input df with distribution of people in group of interest into industries
    :param overalltarget: total target value for people in group of interest
    :return:
    """
    indmixfinal = portionout(indmix, 'pct', overalltarget, 'indint')

    # income targets
    indmixfinal.loc[indmixfinal['indus'].isin(lowlist), 'incdist'] = 'low'
    indmixfinal.loc[indmixfinal['indus'].isin(highlist), 'incdist'] = 'high'
    targets = indmixfinal[['indus', 'pct', 'indint', 'incdist']].merge(incdist, on='incdist', how='left')
    finaltargets = portionout(targets, 'pct_y', 'indint', 'indincint')
    finaltargets.reset_index(drop=True, inplace=True)

    return finaltargets
    

def jwtr11flag(targetdf, workerdf, flagvalue):
    """
    flag workers in workerdf with jwtr11 with the flagvalue according to counts in targetdf
    :param targetdf: guiding dataframe with targets values for each category
    :param workerdf: worker df with selected flag column
    :param flagvalue: the value to set the selected column to
    :return:workerdf
    """
    for i, row in targetdf.iterrows():
        ind = row['indus']
        inc4 = row['hhinc4']
        target = row['indincint']
        options = workerdf[(workerdf['naics2'] == ind) & (workerdf['selected'] == 0) & (
                workerdf['inccat4'] == inc4) & (workerdf['JWTR'] == 11)]
        if len(options) > target:
            ilist = [x for x in options.sample(target, random_state=seedvalue).index.values]
            workerdf.loc[workerdf.index.isin(ilist), 'selected'] = flagvalue
        else:
            ilist2 = [x for x in options.index.values]
            workerdf.loc[workerdf.index.isin(ilist2), 'selected'] = flagvalue

    return workerdf



def preproundtwo(workerdf, flagvalue, eduweights, targets):
    """
    prepare for roundtwo assignment by calculating numbers still needed and adding eduwgt column
    :param workerdf: worker df with selected flag column
    :param flagvalue: value in the 'selected' column to evaluate
    :param eduweights: weights to apply based on edu field for use in sampling
    :return: b, workerdf.  b is the new guiding df that lists how many are still needed in each category
    """
    result = workerdf[workerdf['selected'] == flagvalue].groupby(['naics2', 'inccat4']). \
        household_id.count().reset_index()

    # calculate number still needed in each category
    comparison = targets.merge(result, left_on=['indus', 'hhinc4'],
                               right_on=['naics2', 'inccat4'], how='left').reset_index()
    comparison.household_id.fillna(0, inplace=True)
    comparison['needed'] = comparison['indincint'] - comparison['household_id']
    b = comparison[comparison['needed'] > 0]
    workerdf = workerdf.merge(comparison[['indus', 'hhinc4', 'needed']],
                              left_on=['naics2', 'inccat4'], right_on=['indus', 'hhinc4'], how='left')
    workerdf.loc[workerdf['edu'] == 1, 'eduwgt'] = eduweights[0]
    workerdf.loc[workerdf['edu'] == 2, 'eduwgt'] = eduweights[1]
    workerdf.loc[workerdf['edu'] == 3, 'eduwgt'] = eduweights[2]

    return b, workerdf


def roundtwo(dfb, workerdf, flagvalue):
    """
    finish flagging workers to meet target values (outside of jwtr == 11) using eduwgt column
    :param dfb:  guiding dataframe with targets values for each category
    :param workerdf: worker df with selected flag column
    :param flagvalue: value to set 'selected' column to
    :return: workerdf
    """
    dolater = {}

    for i, row in dfb.iterrows():
        ind = row['indus']
        inc4 = row['hhinc4']
        target = int(row['needed'])
        options = workerdf[(workerdf['naics2'] == ind) & (workerdf['selected'] == 0) & (
                workerdf['inccat4'] == inc4)]

        if len(options) >= target:
            ilist = [x for x in options.sample(target, weights='eduwgt', random_state=seedvalue).index.values]
            workerdf.loc[workerdf.index.isin(ilist), 'selected'] = flagvalue
        elif len(options) < target:
            dolater[i] = (ind,inc4,target)
        
        
    if len(dolater) > 0:
        donow = pd.DataFrame.from_dict(dolater, orient='index', columns=['ind','inc','target'])
        for i,row in donow.iterrows():
            ind = row['ind']
            inc4 = row['inc']
            target = int(row['target'])
            options = workerdf[(workerdf['naics2'] == ind) & (workerdf['selected'] == 0) & (
                    workerdf['inccat4'] == inc4)]
            ilist = [x for x in options.index.values]
            stillneed = target - len(ilist)
            options2 = workerdf[(workerdf['naics2'] == ind) & (workerdf['selected'] == 0) & (workerdf.inccat4 != inc4)]
            ilist.extend([x for x in options2.sample(stillneed).index.values])

            workerdf.loc[workerdf.index.isin(ilist), 'selected'] = flagvalue

    return workerdf


##########################################
# part one: usualwfh
##########################################

# industry targets
finaltargets1 = settargets(indmix1, targetwfh)

print('selecting usualwfh from JWTR 11...')
# round one
workers = jwtr11flag(finaltargets1, workers, 1)

# round two
b1, workers = preproundtwo(workers, 1, eduw, finaltargets1)

print('selecting usualwfh from any...')
workers = roundtwo(b1, workers, 1)

current = workers.selected.sum()
remaining = int(targetwfh - current)
print('checking overall usualwfh target met...')
assert remaining == 0, "overall usualwfh target not met"
workers = workers.drop(['indus', 'hhinc4', 'needed', 'eduwgt'], axis=1)

##########################################
# part two: tc14
##########################################

# industry targets
finaltargets2 = settargets(indmix2, targettc14)

print('selecting tc14 from JWTR 11...')
# round one
workers = jwtr11flag(finaltargets2, workers, 2)

# round two
b2, workers = preproundtwo(workers, 2, eduwtc, finaltargets2)

print('selecting tc14 from any...')
workers = roundtwo(b2, workers, 2)

current = len(workers[workers['selected'] == 2])
remaining = int(targettc14 - current)
print('checking overall tc14 target met...')
assert remaining == 0, "overall tc14 target not met"

##########################################
# assign tc14 wfh status for given day
##########################################
print('assigning wfh status for tc14...')
workers['tc'] = 0

tc1 = targettc14 * tcportions[0]
tc2 = targettc14 * tcportions[1]
tc3 = targettc14 * tcportions[2]
tc4 = targettc14 * tcportions[3]

count = 4
for t in [tc4, tc3, tc2]:
    options = workers[(workers['selected'] == 2) & (workers['tc'] == 0)]
    it = int(t)
    ilist = [x for x in options.sample(it, random_state=seedvalue).index.values]
    workers.loc[workers.index.isin(ilist), 'tc'] = count
    count -= 1

workers.loc[(workers['selected'] == 2) & (workers['tc'] == 0), 'tc'] = 1

workers['random'] = np.random.random(size=len(workers))
workers.loc[(workers['selected'] == 2) & (workers['random'] < (workers['tc'] / 5)), 'working'] = 1
workers.working.fillna(0, inplace=True)

##########################################
# format and save
##########################################
print('preparing and saving HH_WFH_STATUS.csv...')
popsynhh = pd.read_csv(popsynhhpath, names=['sz', 'hhtype', 'vehicles',
                                               'serialno', 'stpuma5', 'rowcol', 'adults', 'workers',
                                               'children', 'iq', 'age', 'hhvtype', 'income'])
workers.loc[workers['selected'] == 1, 'usualwfh'] = 1
workers.loc[(workers['selected'] == 2) & (workers['working'] == 1), 'tc14'] = 1
workers.loc[(workers['selected'] == 2) & (workers['working'] == 0), 'tc14nw'] = 1
workers.usualwfh.fillna(0, inplace=True)
workers.tc14.fillna(0, inplace=True)
workers.tc14nw.fillna(0, inplace=True)
hhsummary = workers.groupby('household_id').agg({'usualwfh':'sum','tc14':'sum','tc14nw':'max'}).reset_index()
hhsummary['wfhworkers'] = hhsummary['tc14'] + hhsummary['usualwfh']
hhsummary.loc[(hhsummary['tc14'] > 0) & (hhsummary['usualwfh'] > 0), 'finalflag'] = 1
hhsummary.loc[(hhsummary['tc14'] > 0) & (hhsummary['usualwfh'] == 0), 'finalflag'] = 2
hhsummary.loc[(hhsummary['tc14'] == 0) & (hhsummary['usualwfh'] > 0), 'finalflag'] = 1
final1 = dfhh[['household_id','SERIALNO']].merge(hhsummary[['household_id','finalflag','wfhworkers','tc14nw']],on='household_id', how='left')
final1.finalflag.fillna(0, inplace=True)
try:
    final1['SERIALNO'] = final1['SERIALNO'].str.replace('HU','99')
except AttributeError:
    pass
final1sort = final1.sort_values('SERIALNO').reset_index()

pssort = popsynhh.sort_values('serialno').reset_index()
final1sort['newindex'] = pssort['index']
final1sort['sn2'] = pssort['serialno']
final1sort.sort_values('newindex', inplace=True)
final1sort.set_index('newindex', inplace=True)
final1sort['SERIALNO'] = final1sort.SERIALNO.astype('int64')
final1sort['diffcheck'] = final1sort['SERIALNO'] - final1sort['sn2']
assert (final1sort.diffcheck == 0).all(), "file not aligned with popsyn_hh"
final1sort.wfhworkers.fillna(0, inplace=True)
final1sort.tc14nw.fillna(0, inplace=True)
final1sort['finalflag'] = final1sort.finalflag.astype('int')
final1sort['wfhworkers'] = final1sort.wfhworkers.astype('int')
final1sort['tc14nw'] = final1sort.tc14nw.astype('int')
final1sort[['SERIALNO', 'finalflag','wfhworkers','tc14nw']].to_csv(savedir + "/HH_WFH_STATUS.CSV", index=False, header=False)

##########################################
# save additional files
##########################################

if savefiles == "Y":
    print('saving additional files...')
    workers.to_csv("workers.csv", index=False)
    finaltargets1.to_csv("finaltargets_usualwfh.csv", index=False)
    finaltargets2.to_csv("finaltargets_tc14.csv", index=False)
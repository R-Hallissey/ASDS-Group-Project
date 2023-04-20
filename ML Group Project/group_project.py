# -*- coding: utf-8 -*-
"""
Created on Mon Mar 20 11:01:56 2023

@author: User
"""

# Packages
import numpy as np
import pandas as pd


# Import Data
Q32018 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20180701_20181001.csv")
Q42018 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20181001_20190101.csv")
Q12019 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20190101_20190401.csv")
Q22019 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20190401_20190701.csv")
Q32019 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20190701_20191001.csv")
Q42019 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20191001_20200101.csv")
Q12020 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20200101_20200401.csv")
Q22020 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20200101_20200401.csv")
Q32020 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20200401_20200701.csv")
Q42020 = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/dublinbikes_20200701_20201001.csv")

# Concatenate 
data = pd.concat([Q32018, Q42018, Q12019, Q22019, Q32019, Q42019, Q12020, Q22020,
                 Q32020, Q42020])
# Save data
dat = data.to_csv('C:/Users/User/Downloads/Machine Learning/Group Project/data.csv', index=False)

dat = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/data.csv")


# Remove redundent columns 
# data.columns
# data.head()
#  data['ADDRESS']
# data['NAME']

# data = data.drop(columns=['STATION ID', 'ADDRESS', 'LATITUDE', 'LONGITUDE', ])


# Save trimmed data
data.to_csv('data2.csv', index=False)

# 
# Reset index
#data.reset_index(inplace=True)

# Inspect dataset
#dat = data

# Reset index
dat.reset_index(inplace=True)

# Inspect dataset
print(dat.head())
dat.info()

# Create bike usage variable
dat['BIKE USAGE'] = dat['AVAILABLE BIKES'] - dat['AVAILABLE BIKES'].shift(-1)

# Inspect dataset
print(dat.head())

# Create "Bike Usage (Prev. Wk)" and "Bike Usage (Prev. Month)" variables

dat['BIKE USAGE (PREV WK)'] = dat['BIKE USAGE'].shift(-2016)
dat['BIKE USAGE (PREV MTH)'] = dat['BIKE USAGE'].shift(-8064)

# Inspect dataset
print(dat.head()) 

# Add datetime column
dat['DATETIME'] = pd.to_datetime(dat.TIME, format = "%Y-%m-%d %H:%M:%S")

# Add date column
dat['DATE'] = dat['DATETIME'].dt.date

# Add "Day of Week" and "Month of Year" columns
dat['DAY OF WEEK'] = dat['DATETIME'].dt.dayofweek
dat['MONTH OF YEAR'] = dat['DATETIME'].dt.month

# Inspect dataset
print(dat.head()) 

# Save data set with dans additonal columns 
dat.to_csv('data3.csv', index=False)


# Further column removal
dat.columns
dat2 = dat

dat = dat.drop(columns=['DATE', 'TIME', 'AVAILABLE BIKES', 'BIKE STANDS',
                        'AVAILABLE BIKE STANDS', 'LAST UPDATED', 'ADDRESS'])

dat = pd.read_csv("C:/Users/User/Downloads/Machine Learning/Group Project/data3.csv")

dat['NAME'].unique()

dat.columns['NAME']
#  Yorks Street East Mask
mask_station = '52'
YSE_ID = '52'
masked_YSE = dat[dat['STATION ID'] == YSE_ID]


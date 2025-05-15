clear 
close all
clc

%load data
load('all_data_Mudrik.mat')
%save releavnt data to csv
xlswrite('pc_exp3.xlsx',pc_exp3)
xlswrite('pc_exp4.xlsx',pc_exp4)

function [ RawDataAllSubs ] = load_files( path )
% load matlab files in a folder and combine into a single matrix

DataList = dir(fullfile(path, '*.mat'));

RawDataAllSubs = [];
for i = 1:size(DataList,1)
    filename = DataList(i).name;
    DataTable = load(fullfile(path, filename));
    DataMat = table2array(DataTable.Data_Table);
    RawDataAllSubs = [RawDataAllSubs ; DataMat]; % add it to the general mat.
end

end


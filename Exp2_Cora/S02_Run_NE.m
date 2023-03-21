folder ='../NR/Network_Enhancement/';  
addpath(genpath(folder)); 


data_place='./Data/Network.csv';
Data=importdata(data_place);
output_w=Network_Enhancement(Data);
filename='./Data/Network_NE.csv';
dlmwrite(filename,output_w);
  
output_w2=Network_Enhancement(output_w);
filename='./Data/Network_NE2.csv';
dlmwrite(filename,output_w2);

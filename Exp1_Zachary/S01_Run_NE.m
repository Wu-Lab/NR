folder ='../NR/Network_Enhancement/';  
addpath(genpath(folder)); 


data_place='./Data/Zachary.txt';
Data=importdata(data_place);
output_w=Network_Enhancement(data);
filename='./Data/zachary_NE.csv';
dlmwrite(filename,output_w);
  
output_w2=Network_Enhancement(output_w);
filename='./Data/zachary_NE2.csv';
dlmwrite(filename,output_w2);

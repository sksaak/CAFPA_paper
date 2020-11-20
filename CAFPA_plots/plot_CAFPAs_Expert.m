% plot CAFPAs in typical representation 
% 
% "Copyright 2016 Mareike Buhl ", 
%  Mareike Buhl, v1 17.12.19, mareike.buhl@uol.de
%
% slightly adjusted by Samira Saak (2020) 


clear all; 
close all; 
clc; 

addpath('C:\Users\Samira\Desktop\CAFPA_pub\analysis\CAFPA_PLOT\functions\'); 
addpath('C:\Users\Samira\Desktop\CAFPA_pub\analysis\EV\data\clustering');
path_save = 'C:\Users\Samira\Desktop\CAFPA_pub\analysis\EV\plots\CAFPA_PLOT\clustering\';

for c = 4:10 
load(['CAFPA_expert_' num2str(c) '.mat']);

for i = 1:c

    if i == 1
        cafpas = expert_1;
    elseif i == 2
        cafpas = expert_2;
    elseif i == 3 
        cafpas = expert_3;
    elseif i == 4
        cafpas = expert_4;
    elseif i == 5
        cafpas = expert_5;
    elseif i == 6 
        cafpas = expert_6;
    elseif i == 7
        cafpas = expert_7;
    elseif i == 8
        cafpas = expert_8;
    elseif i == 9 
        cafpas = expert_9;
    elseif i == 10
        cafpas = expert_10;
    end 
        

%% mean and standard deviation [1x10]
cafpas_value = nanmean(cafpas,1);       
cafpas_errorbar = nanstd(cafpas,1); 

% plot
[fh, axh] = plot_trafficlight_table_tl(cafpas_value,cafpas_errorbar); 


% add names of CAFPAs
cafpa_names_lc = {'C_{A1}','C_{A2}','C_{A3}','C_{A4}','C_{U1}','C_{U2}','C_{B}','C_{N}','C_{C}','C_{E}'};
order = [7,8,9,10,5,6,3,4,1,2];
for ca = 1:10
    text(axh(order(ca)),0.05,0.85,cafpa_names_lc{ca},'FontSize',10);
    
    if ca == 9
        text(axh(order(ca)),0.05,0.2,['{\itN} = ' num2str(size(cafpas,1))],'FontSize',8);
    end
end


% save figure
if c == 4
     saveas(gcf, [path_save, 'Expert_4\Expert_clust_' num2str(i) '.png'])
elseif c == 5 
     saveas(gcf, [path_save, 'Expert_5\Expert_clust_' num2str(i) '.png'])
elseif c == 6
     saveas(gcf, [path_save, 'Expert_6\Expert_clust_' num2str(i) '.png'])
elseif c == 7
     saveas(gcf, [path_save, 'Expert_7\Expert_clust_' num2str(i) '.png'])
elseif c == 8
    saveas(gcf, [path_save, 'Expert_8\Expert_clust_' num2str(i) '.png'])
elseif c == 9
    saveas(gcf, [path_save, 'Expert_9\Expert_clust_' num2str(i) '.png'])
elseif c == 10 
    saveas(gcf, [path_save, 'Expert_10\Expert_clust_' num2str(i) '.png'])
end 

close all;
clear cafpas cafpas_value cafpas_errorbar

end 

end 
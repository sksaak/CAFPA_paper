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


for c = 2:6

load(['CAFPA_Lasso_dem_' num2str(c) '.mat']);


for i = 1:c
    
    if i == 1
        cafpas = lasso_1;
    elseif i == 2
        cafpas = lasso_2;
    elseif i == 3 
        cafpas = lasso_3;
    elseif i == 4
        cafpas = lasso_4;
    elseif i == 5
        cafpas = lasso_5;
    elseif i == 6
        cafpas = lasso_6;
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
if c == 2
    saveas(gcf, [path_save, 'Lasso_2\Lasso_clust_' num2str(i) '.png'])
elseif c == 3
     saveas(gcf, [path_save, 'Lasso_3\Lasso_clust_' num2str(i) '.png'])
elseif c == 4
     saveas(gcf, [path_save, 'Lasso_4\Lasso_clust_' num2str(i) '.png'])
elseif c == 5 
     saveas(gcf, [path_save, 'Lasso_5\Lasso_clust_' num2str(i) '.png'])
elseif c == 6
     saveas(gcf, [path_save, 'Lasso_6\Lasso_clust_' num2str(i) '.png'])
end 




close all;
clear cafpas cafpas_value cafpas_errorbar


end 

end 
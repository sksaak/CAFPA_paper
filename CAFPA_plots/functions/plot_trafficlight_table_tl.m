% function plot_trafficlight_table_tl() plots CAFPA results in trafficlight 
% representation as it was used in the expert survey
% 
% (v1 MB 29.09.16)
% v2 MB 22.05.18 (adapted for plotting results of cafpa-survey-2: added stdprob for continuous CAFPAs)
% 29.05.18: added option for median and interquartile ranges 
% v3 MB 17.12.19: adapted/reorganized for master thesis Samira Saak 
% 
% input: 
% meanprob      vector of mean probabilities
% num_lights    number of traffic light (typically 1)
% stdprob       (varargin{1}) vector of stds of probabilities
% 
% 
% output: 
% fh,h          figure handles 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [fh, h] = plot_trafficlight_table_tl(meanprob,varargin)

%% Plotting details
width = 10;
height = 12; 
plnumx = 4;  % ncols
plnumy = 4;  % nrows

% margins
plytopmargin = 0.1; % relative to 1
plybottommargin = 0.18; 
plxleftmargin = 0.02;
plxrightmargin = 0.02;

% distance between the small figures
plxdist = 0.00/plnumx;
plydist = 0.00/plnumy;

% calculate the size of the figures
plxsize = (1 - plxleftmargin - plxrightmargin - (plxdist*(plnumx-1)))/plnumx;
plysize = (1 - plytopmargin - plybottommargin - (plydist*(plnumy-1)))/plnumy;


%% ordering of meanprob according to subplot order
order = [9,10,7,8,5,6,1,2,3,4]; 
meanprob_sp = meanprob(order); meanprob = meanprob_sp; 
meanprob(isnan(meanprob)) = 0.5; 

% check if standard deviation/interquartiles are given 
std_flag = 0; 
quart_flag = 0; 
if ~isempty(varargin)
    stdprob = varargin{1}; 
    stdprob_sp = stdprob(:,order); stdprob = stdprob_sp'; 
    if size(stdprob,2) == 1; 
        std_flag = 1; % plot mean and std 
    elseif size(stdprob,2) == 2; 
        quart_flag = 1;  % plot median and interquartiles
    end
end

%% plotting 
fh = figure; 
h = []; 
map = calc_colormap(rgb('FireBrick'),rgb('Gold'),rgb('DarkGreen')); map = map'; 

for actnumy = 1:3
    for actnumx = 1:2
        k = actnumx+2*(actnumy-1);
        h_tmp = subplot('position',[((actnumx-1)*(2*plxsize+plxdist) +plxleftmargin) ((actnumy-1)*(plysize+plydist) + plybottommargin) 2*plxsize plysize]);
        hold on
        axis([0 2 0 1]);
        map_idx = ceil(meanprob(k)*100);
        if map_idx == 0
            map_idx = 1;
        end
        % fill box with respective traffic light color:
        color = map(map_idx,:);
        s = fill([0 2 2 0],[1 1 0 0],color);
        % inset colormap:
        if 1
            ax3 = axes('position',[1.46*plxsize + ((actnumx-1)*(2*plxsize+plxdist) +plxleftmargin) 0.22*plysize + ((actnumy-1)*(plysize+plydist) + plybottommargin) 0.99*0.5*plxsize 0.99*0.5*0.5*plysize]);
            if std_flag
                plot_colormap_p(h_tmp,map,meanprob(k),ax3,stdprob(k));
            elseif quart_flag
                plot_colormap_p(h_tmp,map,meanprob(k),ax3,stdprob(k,:));
            else
                plot_colormap_p(h_tmp,map,meanprob(k),ax3);
            end
        end
        
        box(h_tmp,'on');
        set(h_tmp, 'XTick', []);
        set(h_tmp, 'YTick', []);
        h = [h,h_tmp];
        
    end
end


actnumy = 4;
for actnumx = 1:4
    k = actnumx+6;
    h_tmp = subplot('position',[((actnumx-1)*(plxsize+plxdist) +plxleftmargin) ((actnumy-1)*(plysize+plydist) + plybottommargin) plxsize plysize]);
    hold on;
    
    axis([0 1 0 1]);
    map_idx = ceil(meanprob(k)*100);
    if map_idx == 0
        map_idx = 1;
    end
    % fill box with respective traffic light color:
    color = map(map_idx,:);
    s = fill([0 1 1 0],[1 1 0 0],color);
    % inset colormap:
    if 1
        ax3 = axes('position',[0.46*plxsize + ((actnumx-1)*(plxsize+plxdist) +plxleftmargin) 0.22*plysize + ((actnumy-1)*(plysize+plydist) + plybottommargin) 0.99*0.5*plxsize 0.99*0.5*0.5*plysize]);
        if std_flag
            plot_colormap_p(h_tmp,map,meanprob(k),ax3,stdprob(k));
        elseif quart_flag
            plot_colormap_p(h_tmp,map,meanprob(k),ax3,stdprob(k,:));
        else
            plot_colormap_p(h_tmp,map,meanprob(k),ax3);
        end
    end
    
    box(h_tmp,'on');
    set(h_tmp, 'XTick', []);
    set(h_tmp, 'YTick', []);
    h = [h,h_tmp];
end

orient tall
fh = gcf; % 12.02.19 - to access whole figure with figure handle, and not the last subfigure

set(gcf, 'PaperPositionMode', 'manual');
set(gcf, 'PaperUnits', 'centimeters');
set(gcf, 'PaperPosition', [0 0 width height]);

end 
 

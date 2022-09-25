function bcil_tsplot(ts,outpng,varargin)

% Create plots of time series data

ts=load(ts);
if length(varargin) ~= size(ts,2)
	display('ERROR: the number of varargin is not same as the number of columns in ts.');
	return;
end
h=figure;
fsl=16;
fsm=14;
fss=12;
lw=2;
Vol=1:size(ts,1);

set(h,'position',[10 10 1920 180])
sph1=subplot(1,1,1);
plot(Vol,ts,'linestyle','-','linewidth',lw-0.5);
xlim([0 size(ts,1)]);
xlabel('Volume','fontsize',fsm,'interpreter','latex');
ylabel('Signal [a.u.]','fontsize',fsm,'interpreter','latex');
set(sph1,'Color',[.95 .95 .95],'TickLabelInterpreter','latex');
title('\bf Timeseries','interpreter','latex','fontsize',fsl);
if length(varargin) > 0
	legend(varargin,'Location','northwest','FontSize',fss,'interpreter','latex');
end
set(gcf, 'color','w','InvertHardCopy', 'off');
print(gcf,'-dpng',outpng);


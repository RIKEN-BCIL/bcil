function bcil_motiongreyplot(motregabs,greyorig,greyclean,wbcommand,outpng)
%
% Create plots of motion regressors, FD, delta%Dvars, greyplots
%
% Requires path to: 
%    ${HCPPIPEDIR}/global/matlab/normalise.m
%    ${DVARSDIR}/MovPartextImport.m
%    ${DVARSDIR}/FDCalc.m
%    ${DVARSDIR}/DVARSCalc.m
%    ${DVARSDIR}/mis/MassAC.m
%

PracticalSigThr = 5;  			% Based HCP data
warning off;

mra=MovPartextImport(motregabs); 		% mra=load('Movement_Regressors.txt');
greyO=ciftiopen(greyorig, wbcommand);		% greyo=ciftiopen('AP_run-01_epi_Atlas.dtseries.nii','wb_command');
greyC=ciftiopen(greyclean, wbcommand);	% greyc=ciftiopen('AP_run-01_epi_Atlas_hp0_clean.dtseries.nii','wb_command');

DimG=size(greyO.cdata,1);
DimT=size(greyO.cdata,2);
xlimr=[1 DimT];
zlimgr=[-3 3];
ylimgr=[1 DimG];
hTime=(1:(DimT-1))+0.5;
Time=1:DimT;

fsl=16;	% font size large
fsm=14;	% font size medium
fss=12;	% font size small
lw=2;

[grot,DimLH]=system([ wbcommand, ' -file-information ', greyorig, ' -no-map-info | grep CortexLeft: | awk ''{print $2}''']); 
[grot,DimRH]=system([ wbcommand, ' -file-information ', greyorig, ' -no-map-info | grep CortexRight: | awk ''{print $2}''']); 
DimLH=str2num(DimLH);
DimRH=str2num(DimRH);
DimSC=DimG-DimLH-DimRH;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate FD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[FDts,FD_Stat]=FDCalc(mra,[1,1,1,0,0,0]);	% first 3 colums are rotaions in degree, followed by 3 translations in mm
% write FD and FD stats
FDtxt=fopen(strrep(motregabs,'.txt','_FD.txt'),'w');
fprintf(FDtxt,'%6.6f\n',FDts);
fclose(FDtxt);
rowNames = {'Min';'Q1';'Q2';'Q3';'Max';'IQR'};
Val = [ prctile(FDts,0); prctile(FDts,25); prctile(FDts,50); prctile(FDts,75); prctile(FDts,100); prctile(FDts,75)-prctile(FDts,25) ];
T = table(Val, 'RowNames', rowNames);
T.Val=num2str(T.Val,'%1.5f\n');
writetable(T,strrep(motregabs,'.txt','_FD_stats.txt'),'Delimiter','\t','WriteRowNames',true,'WriteVariableName',false);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate DVARS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[DVARSO,DVARS_StatO]=DVARSCalc(greyO.cdata,'scale',1/100,'TransPower',1/3,'RDVARS','verbose',1);
[DVARSC,DVARS_StatC]=DVARSCalc(greyC.cdata,'scale',1/100,'TransPower',1/3,'RDVARS','verbose',1);

% save DVARS (D-var) and Delta percent D-var
DVARStxt=fopen(strrep(greyorig,'.dtseries.nii','_DVARS.txt'),'w');
fprintf(DVARStxt,'%6.6f\n',DVARSO);
fclose(DVARStxt);
DVARStxt=fopen(strrep(greyclean,'.dtseries.nii','_DVARS.txt'),'w');
fprintf(DVARStxt,'%6.6f\n',DVARSC);
fclose(DVARStxt);
DVARStxt=fopen(strrep(greyorig,'.dtseries.nii','_DVARS_DeltapDvar.txt'),'w');
fprintf(DVARStxt,'%6.6f\n',DVARS_StatO.DeltapDvar);
fclose(DVARStxt);
DVARStxt=fopen(strrep(greyclean,'.dtseries.nii','_DVARS_DeltapDvar.txt'),'w');
fprintf(DVARStxt,'%6.6f\n',DVARS_StatC.DeltapDvar);
fclose(DVARStxt);

% save outlier regressors
idx = find(DVARS_StatO.pvals<0.05./(DimT-1) & DVARS_StatO.DeltapDvar>PracticalSigThr);
DVARSreg = zeros(DimT,1);
DVARSreg(idx)   = 1;
DVARSreg(idx+1) = 1;
DVARStxt=fopen(strrep(greyorig,'.dtseries.nii','_DVARS_outliers.txt'),'w');
fprintf(DVARStxt,'%d\n',DVARSreg);
fclose(DVARStxt);
idx = find(DVARS_StatC.pvals<0.05./(DimT-1) & DVARS_StatC.DeltapDvar>PracticalSigThr);
DVARSreg = zeros(DimT,1);
DVARSreg(idx)   = 1;
DVARSreg(idx+1) = 1;
DVARStxt=fopen(strrep(greyclean,'.dtseries.nii','_DVARS_outliers.txt'),'w');
fprintf(DVARStxt,'%d\n',DVARSreg);
fclose(DVARStxt);

% DSE decomposition
[VO,DSE_StatO]=DSEvars(greyO.cdata,'scale',1/100,'saveDSEtable',strrep(greyorig,'.dtseries.nii','_DSE_stats.csv'));
[VC,DSE_StatC]=DSEvars(greyC.cdata,'scale',1/100,'saveDSEtable',strrep(greyclean,'.dtseries.nii','_DSE_stats.csv'));

% DSE decomposition for CIFTI

% Autocorrelation coefficient
greyo=normalise(greyO.cdata,2);
greyc=normalise(greyC.cdata,2);
outgrey=greyO;
outgrey.cdata=mean(outgrey.cdata,2);
outgrey.cdata=MassAC(greyo',1)';
ciftisavereset(outgrey,strrep(greyorig,'.dtseries.nii','_ACcoeff.dscalar.nii'),wbcommand);
accoefftxt=fopen(strrep(greyorig,'.dtseries.nii','_ACcoeff_median.txt'),'w');
fprintf(accoefftxt,'%1.5f\n',mean(outgrey.cdata));
fclose(accoefftxt);
outgrey.cdata=MassAC(greyc',1)';
ciftisavereset(outgrey,strrep(greyclean,'.dtseries.nii','_ACcoeff.dscalar.nii'),wbcommand);
accoefftxt=fopen(strrep(greyclean,'.dtseries.nii','_ACcoeff_median.txt'),'w');
fprintf(accoefftxt,'%1.5f\n',mean(outgrey.cdata));
fclose(accoefftxt);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create figure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
h=figure;
set(h,'position',[10 10 1920 1080]);
set(h,'visible','on');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot FD, D (rotation) and D (translation)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sph1=subplot(6,1,1);
yyaxis left
plot(hTime,FDts,'color','k','linestyle','-','linewidth',lw-0.5);
set(sph1,'Color',[.95 .95 .95],'TickLabelInterpreter','latex')
title('\bf FD, D (rotation) and D (translation)','interpreter','latex','fontsize',fsl);
ylabel('FD (mm)','color','k','fontsize',fsm,'interpreter','latex');

if max(FDts)>0.6
        ylim([0 max(FDts)+0.1]);
else
        ylim([0 0.6]);
end
    
yyaxis right
plot(Time,FD_Stat.AbsRot,'color',[1,0,0,0.3],'linestyle','-','linewidth',lw-0.9);
hold on 
plot(Time,FD_Stat.AbsTrans,'color',[0,0,1,0.3],'linestyle','-','linewidth',lw-0.9);
hold off
axis tight
ylabel('D (mm)','fontsize',fsm,'interpreter','latex');
lh1=legend({'FD','D (rotation)','D (translation)'},'orientation','horizontal','location','best','FontSize',fss,'interpreter','latex');
set(lh1, 'Box', 'off' ) ;
xticklabels({});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot %delta D-vars motion and distortion corrected 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sph2=subplot(6,1,2);
plot(hTime,DVARS_StatO.DeltapDvar,'color','k','linestyle','-','linewidth',lw-0.5); 
set(sph2,'Color',[.95 .95 .95],'TickLabelInterpreter','latex')
title('\bf $\Delta\%D$-var (motion and distortion corrected)','interpreter','latex','fontsize',fsl);
ylabel('Greyordinates','FontSize',fsm,'interpreter','latex');
mx_cntrd_g_ts = max(DVARS_StatO.DeltapDvar); mn_cntrd_g_ts = min(DVARS_StatO.DeltapDvar);
stps = abs(round(diff([mx_cntrd_g_ts mn_cntrd_g_ts])./4.5));
Ytcks = round(mn_cntrd_g_ts:stps:mx_cntrd_g_ts);
ylabel('$\Delta\%D$-var','fontsize',fss,'interpreter','latex')
set(sph2,'ygrid','on','xlim',[1 DimT-1],'ycolor','k','yTick',Ytcks,'ylim',[mn_cntrd_g_ts mx_cntrd_g_ts])

% plot significant DVARS statistically (Bonferroni corrected p<0.05) and practically (Delta %D-var > 5%)
Idx = find(DVARS_StatO.pvals<0.05./(DimT-1) & DVARS_StatO.DeltapDvar>PracticalSigThr);
stpjmp  = 1;
Lcol    = [.6 .6 .6];
yyll = ylim;
ph=[];
for ii=1:numel(Idx)
    xtmp=[Idx(ii)-stpjmp   Idx(ii)-stpjmp   Idx(ii)+stpjmp  Idx(ii)+stpjmp];
    ytmp=[yyll(1)               yyll(2)         yyll(2)        yyll(1)    ];
    ph(ii)=patch(xtmp,ytmp,Lcol,'FaceAlpha',0.3,'edgecolor','none','LineStyle','none');
    clear *tmp
end
lh2=legend({'$\Delta\%D$-var (motion and distortion-corrected)' 'Statistically and practically significant'},'orientation','horizontal','location','best','FontSize',fss,'interpreter','latex');
set( lh2, 'Box', 'off' ) ;
xticklabels({});

% greyplot
sph3=subplot(6,1,3);
imagesc(greyo,zlimgr);colormap(gray);xlim(xlimr);ylim(ylimgr);
title('\bf Timeseries of greyordinates (motion and distortion corrected)','interpreter','latex','fontsize',fsl);
ylabel('Greyordinates','FontSize',fsm,'interpreter','latex');
set(sph3,'TickLabelInterpreter','latex')
stx=0; endx=DimT; sty=0; endy=DimLH;
v=[stx sty; stx endy; endx endy; endx sty];
f=[1 2 3 4];
patch('Faces',f,'Vertices',v,'FaceColor','c','FaceAlpha',0.1,'LineStyle','none');

sty=endy+1; endy=DimLH+DimRH;
v=[stx sty; stx endy; endx endy; endx sty];
f=[1 2 3 4];
patch('Faces',f,'Vertices',v,'FaceColor','y','FaceAlpha',0.1,'LineStyle','none');

sty=endy+1; endy=DimG;
v=[stx sty; stx endy; endx endy; endx sty]; 
f=[1 2 3 4];
patch('Faces',f,'Vertices',v,'FaceColor','r','FaceAlpha',0.1,'LineStyle','none');
xticklabels({});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot deltaDvars cleaned
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sph4=subplot(6,1,4);
%subplot(6,1,2);
plot(hTime,DVARS_StatC.DeltapDvar,'color','k','linestyle','-','linewidth',lw-0.5); 
set(sph4,'Color',[.95 .95 .95],'TickLabelInterpreter','latex')
title('\bf $\Delta\%D$-var (ICA-FIX cleaned)','interpreter','latex','fontsize',fsl);
ylabel('Greyordinates','FontSize',fsm,'interpreter','latex');
mx_cntrd_g_ts = max(DVARS_StatC.DeltapDvar); mn_cntrd_g_ts = min(DVARS_StatC.DeltapDvar);
stps = abs(round(diff([mx_cntrd_g_ts mn_cntrd_g_ts])./4.5));
Ytcks = round(mn_cntrd_g_ts:stps:mx_cntrd_g_ts);
ylabel('$\Delta\%D$-var','fontsize',fss,'interpreter','latex')
set(sph4,'ygrid','on','xlim',[1 DimT-1],'ycolor','k','yTick',Ytcks,'ylim',[mn_cntrd_g_ts mx_cntrd_g_ts])

% plot significant DVARS statistically (Bonferroni corrected p<0.05) and practically (Delta %D-var > 5%)
Idx = find(DVARS_StatC.pvals<0.05./(DimT-1) & DVARS_StatC.DeltapDvar>PracticalSigThr);
stpjmp  = 1;
Lcol    = [.6 .6 .6];
yyll=ylim;
ph=[];
for ii=1:numel(Idx)
    xtmp=[Idx(ii)-stpjmp   Idx(ii)-stpjmp   Idx(ii)+stpjmp  Idx(ii)+stpjmp];
    ytmp=[yyll(1)               yyll(2)         yyll(2)        yyll(1)    ];
    ph(ii)=patch(xtmp,ytmp,Lcol,'FaceAlpha',0.3,'LineStyle','none');
    clear *tmp
end
lh4=legend({'$\Delta\%D$-var (ICA-FIX cleaned)' 'Statistically and practically significant'},'orientation','horizontal','location','best','FontSize',fss,'interpreter','latex');
set( lh4, 'Box', 'off' ) ;
xticklabels({});

% greyplot
sph5=subplot(6,1,5);
imagesc(greyc,zlimgr);colormap(gray);xlim(xlimr);ylim(ylimgr);
title('\bf Timeseries of greyordinates (ICA-FIX cleaned)','interpreter','latex','fontsize',fsl);
ylabel('Greyordinates','FontSize',fsm,'interpreter','latex');
set(sph5,'TickLabelInterpreter','latex')
stx=0; endx=DimT; sty=0; endy=DimLH;
v=[stx sty; stx endy; endx endy; endx sty];
f=[1 2 3 4];
patch('Faces',f,'Vertices',v,'FaceColor','c','FaceAlpha',0.1,'LineStyle','none');

sty=endy+1; endy=DimLH+DimRH;
v=[stx sty; stx endy; endx endy; endx sty];
f=[1 2 3 4];
patch('Faces',f,'Vertices',v,'FaceColor','y','FaceAlpha',0.1,'LineStyle','none');

sty=endy+1; endy=DimG;
v=[stx sty; stx endy; endx endy; endx sty]; 
f=[1 2 3 4];
patch('Faces',f,'Vertices',v,'FaceColor','r','FaceAlpha',0.1,'LineStyle','none');
xticklabels({});

% plot mean signals
sph5=subplot(6,1,6);
plot(Time,mean(greyO.cdata)','color','b','linestyle','-','linewidth',lw-0.5); 
hold on 
plot(Time,mean(greyC.cdata)','color','r','linestyle','-','linewidth',lw-0.5);
hold off
set(sph5,'Color',[.95 .95 .95],'xlim',xlimr,'TickLabelInterpreter','latex');
title('\bf Timeseries of mean signals','interpreter','latex','fontsize',fsl);
ylabel('Signal [a.u.]','FontSize',fsm,'interpreter','latex');
lh5=legend({'motion and distortion-corrected' 'ICA-FIX cleaned'},'orientation','horizontal','location','best','FontSize',fss,'interpreter','latex');
set( lh5, 'Box', 'off' ) ;
xlabel('Volume','FontSize',fsm,'interpreter','latex');

set(gcf, 'color','w','InvertHardCopy', 'off');
print(gcf,'-dpng',outpng);

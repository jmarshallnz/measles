% MeasFS.m Calculate measles final size for DHBs
%__________________________________________________________________________
clear all
close all
%__________________________________________________________________________
disp(' _______________________MeasFS______________________________________')
%__________________________________________________________________________
% the final size equation
R0 = 12.8;
x0max = 0.3;
Nx0 = 200;
P = (1:Nx0)*x0max/(Nx0+1);
x0 = P./(1-exp(-R0.*P));
Rv = R0*x0;
%__________________________________________________________________________
% DHB: 1 Auckland, 2 Bay of Plenty, 3 Canterbury, 4 Capital and Coast,
% DHB:  5 Counties Manukau, 6 Hawke's Bay, 7 Hutt Valley, 8 Lakes,
% DHB:  9 MidCentral, 10 Nelson Marlborough, 11 Northland
% DHB:  12 South Canterbury, 13 Southern, 14 Tairawhiti, 15 Taranaki,
% DHB:  16 Waikato, 17 Wairarapa, 18 Waitemata, 19 West Coast, 20 Whanganui.
%
Pop = [374772,149289,432990,240162,365838,114345,114621,66942,132129,...
    119109,104913,50718,263439,24705,88458,271716,33120,438735,...
    28410,47787];
Naive = [52010,20679,51357,32625,55544,15602,15198,10558,17328,13059,...
    14921,5238,31607,4769,11473,39402,3932,58350,3197,6075];
%RvDHB = 1+2*rand(size(1:20));
RvDHB = R0*Naive./Pop;
PDHB = interp1(Rv,P,RvDHB);
% The number of cases per DHB
FSDHB = PDHB.*Naive;
% The number of vaccinations short
VDHB = Naive-Pop/R0;
%__________________________________________________________________________
% Export results
for i=1:20
    Z(i,1) = Pop(i);
    Z(i,2) = Naive(i);
    Z(i,3) = FSDHB(i);
    Z(i,4) = VDHB(i);
end
%
filename = 'measvac.xls';
xlswrite(filename,Z)
%__________________________________________________________________________
% Plot results
bar(Pop,'b')
hold on
bar(Naive,'r')
hold off
figure
plot(Rv,P,'b','linewidth',2)
axis([0.5,ceil(Rv(Nx0)),-x0max/10,x0max])
hold on
plot([0,ceil(Rv(Nx0))],[0,0],'k','linewidth',2)
plot(RvDHB(1)*[1,1],[0,PDHB(1)],'-o',...
    'MarkerEdgeColor','k','MarkerFaceColor','g', 'MarkerSize',10)
plot(RvDHB(2)*[1,1],[0,PDHB(2)],'-o',...
    'MarkerEdgeColor','k','MarkerFaceColor','c', 'MarkerSize',10)
plot(RvDHB(3)*[1,1],[0,PDHB(3)],'-o',...
    'MarkerEdgeColor','k','MarkerFaceColor','m', 'MarkerSize',10)
plot(RvDHB(4)*[1,1],[0,PDHB(4)],'-o',...
    'MarkerEdgeColor','k','MarkerFaceColor','b', 'MarkerSize',10)
plot(RvDHB(5)*[1,1],[0,PDHB(5)],'-o',...
    'MarkerEdgeColor','k','MarkerFaceColor','r', 'MarkerSize',10)
hold off
figure
plot(Rv,P,'b','linewidth',2)
axis([0.5,ceil(Rv(Nx0)),-x0max/10,x0max])
hold on
plot([0,ceil(Rv(Nx0))],[0,0],'k','linewidth',2)
plot(RvDHB(6)*[1,1],[0,PDHB(6)],'-s',...
    'MarkerEdgeColor','k','MarkerFaceColor','g', 'MarkerSize',10)
plot(RvDHB(7)*[1,1],[0,PDHB(7)],'-s',...
    'MarkerEdgeColor','k','MarkerFaceColor','c', 'MarkerSize',10)
plot(RvDHB(8)*[1,1],[0,PDHB(8)],'-s',...
    'MarkerEdgeColor','k','MarkerFaceColor','m', 'MarkerSize',10)
plot(RvDHB(9)*[1,1],[0,PDHB(9)],'-s',...
    'MarkerEdgeColor','k','MarkerFaceColor','b', 'MarkerSize',10)
plot(RvDHB(10)*[1,1],[0,PDHB(10)],'-s',...
    'MarkerEdgeColor','k','MarkerFaceColor','r', 'MarkerSize',10)
hold off
figure
plot(Rv,P,'b','linewidth',2)
axis([0.5,ceil(Rv(Nx0)),-x0max/10,x0max])
hold on
plot([0,ceil(Rv(Nx0))],[0,0],'k','linewidth',2)
plot(RvDHB(11)*[1,1],[0,PDHB(11)],'-d',...
    'MarkerEdgeColor','k','MarkerFaceColor','g', 'MarkerSize',10)
plot(RvDHB(12)*[1,1],[0,PDHB(12)],'-d',...
    'MarkerEdgeColor','k','MarkerFaceColor','c', 'MarkerSize',10)
plot(RvDHB(13)*[1,1],[0,PDHB(13)],'-d',...
    'MarkerEdgeColor','k','MarkerFaceColor','m', 'MarkerSize',10)
plot(RvDHB(14)*[1,1],[0,PDHB(14)],'-d',...
    'MarkerEdgeColor','k','MarkerFaceColor','b', 'MarkerSize',10)
plot(RvDHB(15)*[1,1],[0,PDHB(15)],'-d',...
    'MarkerEdgeColor','k','MarkerFaceColor','r', 'MarkerSize',10)
hold off
figure
plot(Rv,P,'b','linewidth',2)
axis([0.5,ceil(Rv(Nx0)),-x0max/10,x0max])
hold on
plot([0,ceil(Rv(Nx0))],[0,0],'k','linewidth',2)
plot(RvDHB(16)*[1,1],[0,PDHB(16)],'-^',...
    'MarkerEdgeColor','k','MarkerFaceColor','g', 'MarkerSize',10)
plot(RvDHB(17)*[1,1],[0,PDHB(17)],'-^',...
    'MarkerEdgeColor','k','MarkerFaceColor','c', 'MarkerSize',10)
plot(RvDHB(18)*[1,1],[0,PDHB(18)],'-^',...
    'MarkerEdgeColor','k','MarkerFaceColor','m', 'MarkerSize',10)
plot(RvDHB(19)*[1,1],[0,PDHB(19)],'-^',...
    'MarkerEdgeColor','k','MarkerFaceColor','b', 'MarkerSize',10)
plot(RvDHB(20)*[1,1],[0,PDHB(20)],'-^',...
    'MarkerEdgeColor','k','MarkerFaceColor','r', 'MarkerSize',10)
hold off
%__________________________________________________________________________
%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|
%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>|
%__________________________________________________________________________

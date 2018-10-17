clear all
load('OJ.mat')
load('OJ_idx.mat')

t = (OJy' - mean(OJy))/std(OJy);
y = OJx';


len1 = length(idx1);
len2 = length(idx2);
figure
hold on
plot(1:len1, t(idx1), 'ko')
plot((len1+1):(len1+len2), t(idx2), 'ro')

figure
hold on
plot(y(:, idx1), 'k')
plot(y(:, idx2), 'r')
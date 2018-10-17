clear all
load('facedata_norm.mat')
d = Y(:, 1);

%%
d_4096 = reshape(d, [64, 64]);
figure
imagesc(d_4096);
colormap gray

%%
d_1024 = imresize(d_4096, [32, 32]);
figure
imagesc(d_1024);
colormap gray

%% 
d_484 = imresize(d_4096, [22, 22]);
figure
imagesc(d_484);
colormap gray

%%
d_225 = imresize(d_4096, [15, 15]);
figure
imagesc(d_484);
colormap gray


%%
d_100 = imresize(d_4096, [10, 10]);
figure
imagesc(d_100);
colormap gray
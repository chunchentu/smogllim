clear all
load('facedata_norm.mat')
t = t_norm;
y = Y;

% load('face_1024.mat')
% t = dataT;
% y = dataY;
if ismac
   addpath('../Utils') 
end

K = 20;
Lw = 9;

flag_retrain = 1;
%%
if flag_retrain
    fprintf(1, 'Re train the model\n');
    
    verb = 1;

    rng(1234)
    N = size(y, 2);
    idx = kmeans([t;y]', K);
    in_r = full(sparse(1:N,idx,1,N,K,N));
    cstr.Sigma = 'i*';


    [th, r, ll] = gllim(t, y, K,'Lw',Lw,'cstr',cstr,'maxiter',100,'verb', verb);
    save(sprintf('face_K%d_Lw%d.mat', K, Lw), '-v7.3')
else
    fprintf(1, 'Load from the pre-trained model\n');
    load(sprintf('face_K%d_Lw%d.mat', K, Lw));
end

%%
[~, cluster_assignment] = max(r, [], 2);
for target_cluster = 1:K
    cluster_member = find(cluster_assignment == target_cluster);
    figure
    plotmatrix(t(:, cluster_member)')
    xlim([-1, 1])
    title(sprintf('Cluster %d', target_cluster))
end

% 2 5 6
%%
target_cluster = 2;
cluster_member = find(cluster_assignment == target_cluster);
figure
plotmatrix(t(:, cluster_member)')
xlim([-1, 1])
title(sprintf('Cluster %d', target_cluster))

%%
figure

for i = 1:length(cluster_member)
   temp_img = reshape(y(:, cluster_member(i)), 64, 64);
   subplot(5, 6, i)
   imagesc(temp_img)
   colormap gray
   if i >= length(cluster_member)
       break
   end
    
    
end



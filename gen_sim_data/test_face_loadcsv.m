clear all
load('facedata_norm.mat')
t = t_norm;
y = Y;

M = csvread('faceinfo.csv', 1, 1);
gllim_clust = M(:, 8);
test_train = M(:, 9);
train_idx = find(test_train==1);
test_idx = find(test_train==0);

if ismac
    addpath('../Utils') 
end

train_y = y(:, train_idx);
train_t = t(:, train_idx);
test_y = y(:, test_idx);
test_t = t(:, test_idx);

%% check cluster 7
target_cluster = 7;
cluster_member = find(gllim_clust == target_cluster);
figure
plotmatrix(t(:, cluster_member)')
xlim([-1, 1])
title(sprintf('Cluster %d', target_cluster))

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
%%
K = length(unique(gllim_clust));
Lw = 9;

flag_retrain = 1;
%%
if flag_retrain
    fprintf(1, 'Re train the model\n');
    
    verb = 1;

    rng(1234)
    N = size(train_y, 2);
    idx = gllim_clust(train_idx);
    in_r = full(sparse(1:N,idx,1,N,K,N));
    cstr.Sigma = 'i*';


    [th, r, ll] = gllim(train_t, train_y, K,'Lw',Lw,'cstr',cstr,'maxiter', 100,'verb', verb, 'in_r', in_r);
    save(sprintf('face_K%d_Lw%d.mat', K, Lw), '-v7.3')
else
    fprintf(1, 'Load from the pre-trained model\n');
    load(sprintf('face_K%d_Lw%d.mat', K, Lw));
end

%%
[~, temp_assignment] = max(r, [], 2);
N = size(y, 2);
cluster_assignment = zeros(N, 1);
cluster_assignment(train_idx) = temp_assignment;
for target_cluster = 1:K
    cluster_member = find(cluster_assignment == target_cluster);
    figure
    plotmatrix(t(:, cluster_member)')
    xlim([-1, 1])
    title(sprintf('Cluster %d', target_cluster))
end

% 2 5 6
%%
target_cluster = 7;
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

%% save for R
save(sprintf('face_cluster%d.mat', target_cluster), 'cluster_member')
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

load('R_subcluster.mat');

%%
subcluster_idx = [subcluster_1; subcluster_2];
subset_y = y(:, subcluster_idx);
subset_t = t(:, subcluster_idx);
subcluster_r = [ones(length(subcluster_1), 1); 2*ones(length(subcluster_2), 1)];


save('cluster_seed_data.mat', 'subset_y', 'subset_t', 'subcluster_r')
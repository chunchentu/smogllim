clear all
load('facedata_norm.mat')
t = t_norm;
y = Y;

% load('face_1024.mat')
% t = dataT;
% y = dataY;

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
figure
for i = 1:length(subcluster_1)
   temp_img = reshape(y(:, subcluster_1(i)), 64, 64);
   subplot(5, 6, i)
   imagesc(temp_img)
   colormap gray
   if i >= length(subcluster_1)
       break
   end
end

figure
for i = 1:length(subcluster_2)
   temp_img = reshape(y(:, subcluster_2(i)), 64, 64);
   subplot(5, 6, i)
   imagesc(temp_img)
   colormap gray
   if i >= length(subcluster_2)
       break
   end
end

%%
subcluster_idx = [subcluster_1; subcluster_2];
subset_y = y(:, subcluster_idx);
subset_t = t(:, subcluster_idx);
subcluster_r = [ones(length(subcluster_1), 1); 2*ones(length(subcluster_2), 1)];
%% estimate gllim parameters
K = 2;
Lw = 9;
rng(1234)
verb = 1;

N = size(subset_y, 2);
idx = gllim_clust(train_idx);
in_r = subcluster_r;
cstr.Sigma = 'i*';

[th, r, ll] = gllim(subset_t, subset_y, K,'Lw',Lw,'cstr',cstr,'maxiter', 100,'verb', verb, 'in_r', in_r);

%%
save('subcluster_retrain.mat', '-v7.3')


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

% %% try on reshape
% target_size = 15;
% subset_N = size(subset_y, 2);
% temp_img = zeros([target_size^2, subset_N]);
% 
% for i = 1:subset_N
%    cur_img = reshape(subset_y(:, i), [64, 64]);
%    resize_img = imresize(cur_img, [target_size, target_size]);
%    temp_img(:, i) = reshape(resize_img, [target_size^2, 1]);
% end
% 
% %% check
% figure
% imagesc(reshape(temp_img(:, 10), [target_size, target_size]))
% colormap gray

%%

% K = 2;
% Lw = 9;
% rng(1234)
% verb = 1;
% 
% N = size(subset_y, 2);
% idx = gllim_clust(train_idx);
% in_r = subcluster_r;
% cstr.Sigma = 'i*';
% 
% [th, r, ll] = gllim(subset_t, temp_img, K,'Lw',Lw, ...
%     'cstr',cstr,'maxiter', 100,'verb', verb, 'in_r', in_r);
clear all
target_size = 10;
load(fullfile('sim_data', sprintf('size%d.mat', target_size)))
t = [subcluster1_t, subcluster2_t];
y = [subcluster1_y, subcluster2_y];

if ismac
    addpath('../Utils') 
end

%% estimate gllim parameters
K = 2;
Lw = 9;
rng(1234)
verb = 1;

N = size(y, 2);
cstr.Sigma = 'i';

[th, r, ll] = gllim(t, y, K,'Lw',Lw,'cstr',cstr,'maxiter', 100,'verb', verb);

%%
[~, cluster_assignment] = max(r, [], 2);
N = size(y, 2);
for target_cluster = 1:K
    cluster_member = find(cluster_assignment == target_cluster);
    figure
    plotmatrix(t(:, cluster_member)')
    xlim([-1, 1])
    title(sprintf('Cluster %d', target_cluster))
end

%%
[pred, r] = gllim_inverse_map(y, th, 0);
pred = pred(1:3, :);
pred_se = sum((pred - t).^2, 1);
pred_mse = mean(pred_se);
fprintf(1, 'Train MSE: %.4g\n', pred_mse);

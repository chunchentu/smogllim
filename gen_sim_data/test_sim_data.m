clear all
load('sim_data.mat')
t = overall_t;
y = overall_y;

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

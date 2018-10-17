clear all
close all

load('OJ.mat')
t = (OJy' - mean(OJy))/std(OJy);
y = OJx';
if ismac
   addpath('../Utils') 
end

flag_retrain = 1;
%%
if flag_retrain
    fprintf(1, 'Re train the model\n');
    K = 10;
    Lw = 9;
    verb = 0;

    rng(9487)
    N = size(y, 2);
    idx = kmeans([t;y]', K);
    in_r = full(sparse(1:N,idx,1,N,K,N));
    cstr.Sigma = 'i*';


    [th, r, ll] = gllim(t, y, K,'Lw',Lw,'cstr',cstr,'maxiter',100,'verb', verb, 'in_r', in_r);
    save('OJ_K25_Lw9.mat')
else
    fprint(1, 'Load from the pre-trained model\n');
    load('OJ_K25_Lw9.mat');
end

%%
[~, cluster_assignment] = max(r, [], 2);
target_cluster = 9;
cluster_member = find(cluster_assignment == target_cluster);
figure
subplot(2,1,1)
plot(y(:, cluster_member))
subplot(2,1,2)
plot(cluster_member, t(:, cluster_member), 'o')

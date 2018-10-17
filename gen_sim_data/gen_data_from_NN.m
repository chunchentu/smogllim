clear all
load('facedata_norm.mat')
dict_t = t_norm;
dict_y = Y;


load('cluster_seed_data.mat')
t = subset_t;
y = subset_y;
NN_num = 30;

sub_cluster_idx = zeros(NN_num, 2);


cluster_assignment = subcluster_r;

for target_cluster = 1:2
    target_member = find(cluster_assignment == target_cluster);

%     target_y = subset_y(:, target_member);
%     target_mean = mean(target_y, 2);
%     target_se = sum(bsxfun(@minus, dict_y, target_mean).^2, 1);
%     [~, sort_idx] = sort(target_se);
%     sub_cluster_idx(:, target_cluster) = sort_idx(1:NN_num);
    
    
    target_t = subset_t(:, target_member);
    target_mean = mean(target_t, 2);

    target_se = sum(bsxfun(@minus, dict_t, target_mean).^2, 1);
    [~, sort_idx] = sort(target_se);
    sub_cluster_idx(:, target_cluster) = sort_idx(1:NN_num);
    
    figure
    for i=1:NN_num
        subplot(5, 5, i)
        temp_idx = sort_idx(i);
        temp_img = reshape(dict_y(:, temp_idx), [64, 64]);
        imagesc(temp_img);
        colormap gray
        title(sprintf('%d', temp_idx))
        if i >= NN_num || i >= 25
            break
        end

    end

end

t1 = dict_t(:, sub_cluster_idx(:, 1));
t2 = dict_t(:, sub_cluster_idx(:, 2));
figure
hold on
plot(t1(1, :), t1(2, :), 'kx')
plot(t2(1, :), t2(2, :), 'bo')

figure
hold on
plot(t1(2, :), t1(3, :), 'kx')
plot(t2(2, :), t2(3, :), 'bo')

figure
hold on
plot(t1(1, :), t1(3, :), 'kx')
plot(t2(1, :), t2(3, :), 'bo')

% check interasection
intersect(sub_cluster_idx(:, 1), sub_cluster_idx(:, 2))
save('sub_clsuter_nn.mat', 'sub_cluster_idx')


%%
D = size(dict_y, 1);
img_size = [10:5:30, 32];
img_size_len = length(img_size);
save_prefix = 'sim_data';
imgClust1 = dict_y(:, sub_cluster_idx(:, 1));
imgClust2 = dict_y(:, sub_cluster_idx(:, 2));
subcluster1_t = dict_t(:, sub_cluster_idx(:, 1));
subcluster2_t = dict_t(:, sub_cluster_idx(:, 2));
for img_size_idx = 1:img_size_len
    target_size = img_size(img_size_idx);
    fprintf(1, 'Resizing image to: %d\n', target_size);
    
    % cluster 1
    subcluster1_y = zeros(target_size^2, NN_num);
    for i = 1:NN_num
       temp_y =  reshape(imgClust1(:, i), [64, 64]);
       temp_y =  imresize(temp_y, [target_size, target_size]);
       subcluster1_y(:, i) = temp_y(:);
    end
    
    % cluster 2
    subcluster2_y = zeros(target_size^2, NN_num);
    for i = 1:NN_num
       temp_y =  reshape(imgClust2(:, i), [64, 64]);
       temp_y =  imresize(temp_y, [target_size, target_size]);
       subcluster2_y(:, i) = temp_y(:);
    end
    save_file_name = fullfile(save_prefix, sprintf('size%d.mat', target_size));
    save(save_file_name, 'subcluster1_y', 'subcluster1_t', 'subcluster2_y', 'subcluster2_t')
end


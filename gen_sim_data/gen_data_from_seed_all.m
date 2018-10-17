clear all
load('cluster_seed_data.mat')
t = subset_t;
y = subset_y;
cluster_assignment = subcluster_r;
for cvID = 1:10
    overall_t = [];
    overall_y = [];
    cluster_assignment = [];

    for target_cluster = 1:2
        target_member = find(cluster_assignment == target_cluster);

        target_y = subset_y(:, target_member);
        target_t = subset_t(:, target_member);


        %%
        Lw = 9;
        gen_group_N = 5;
        round_num = 5;
        gen_N = gen_group_N * round_num;
    %     [gen_t, gen_w, gen_y] = gen_face_instance_randSelect(target_t, target_y, gen_group_N, Lw, round_num);
        [gen_t, gen_w, gen_y] = gen_face_instance(target_t, target_y, gen_N, Lw, cvID);

        if target_cluster == 1
            overall_t = [overall_t, gen_t(:, 1:16)];
            overall_y = [overall_y, gen_y(:, 1:16)];
        else
            overall_t = [overall_t, gen_t(:, 1:17)];
            overall_y = [overall_y, gen_y(:, 1:17)];
        end

        %% checking
        figure
        plotmatrix(gen_t');

        figure
        for i=1:gen_N
            subplot(5, 5, i)
            temp_img = reshape(gen_y(:, i), [64, 64]);
            imagesc(temp_img);
            colormap gray
            if i >= gen_N
                break
            end

        end

        figure
        for i=1:size(subset_y, 2);
            subplot(5, 5, i)
            temp_img = reshape(subset_y(:, i), [64, 64]);
            imagesc(temp_img);
            colormap gray
            if i >= size(subset_y, 2)
                break
            end
        end
    end
    overall_t = [overall_t, subset_t];
    overall_y = [overall_y, subset_y];

    save('sim_data.mat', 'overall_t', 'overall_y')

end
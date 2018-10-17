function [gen_t, gen_w, gen_y, theta] = gen_face_instance_randSelect(t, y, gen_round_N, Lw, round_num)

    N = size(y, 2);
    select_num = ceil(N*0.3);
    gen_t = [];
    gen_w = [];
    gen_y = [];
    rng(1234)
    kmeans_num = 3;
    idx = kmeans(t', kmeans_num);
    for r = 1:round_num
        rand_index = randsample(1:N, select_num);
% %         kmeans_id = mod(r, kmeans_num) + 1;
%         rand_index = [rand_index, find(idx==kmeans_id)'];
%         rand_index = find(idx==kmeans_id);
        temp_gen_t = t(:, rand_index);
        temp_gen_y = y(:, rand_index);
        [temp_t, temp_w, temp_y] = gen_face_instance(temp_gen_t, temp_gen_y, gen_round_N, Lw, r);
        gen_t = [gen_t, temp_t];
        gen_y = [gen_y, temp_y];
        gen_w = [gen_w, temp_w];
    end    
end

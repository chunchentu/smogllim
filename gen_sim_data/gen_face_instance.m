function [gen_t, gen_w, gen_y, theta] = gen_face_instance(t, y, gen_N, Lw, s)
    rng(1234+s)
    [Lt, N] = size(t);
    D = size(y, 1);

    ct = mean(t,2);
    Gammat = cov(t');
    x = t;

    y_bar = mean(y, 2);
    y_star = bsxfun(@minus, y, y_bar);
    x_bar = mean(x, 2);
    x_star = bsxfun(@minus, x, x_bar);

    At = y_star*x_star'/(x_star*x_star');
    b = mean(y - At*x, 2);
    residual = y - bsxfun(@plus, At*x, b);
    C = residual * residual'/N;
    
   [U,Lambda]=eigs(C,Lw);
   sigma2k=(trace(C)-trace(Lambda))./(D-Lw) + 1e-8;
   if sigma2k < eps
       sigma2k = eps; 
    end
   Sigma=sigma2k*eye(D);
   Aw=U*real(sqrt(Lambda-sigma2k*eye(Lw)));
   Gammaw = Lambda;

    
    A = [At, Aw];
    %% generating data
    A = [At, Aw];
    %% generating data
%     gen_t = mvnrnd(ct, Gammat, gen_N)'; % Nx1
%     gen_w = mvnrnd(zeros(Lw, 1), eye(Lw), gen_N)';
%     gen_x = [gen_t; gen_w];


    gen_t = mvnrnd(ct, Gammat, gen_N)'; % Nx1
    gen_w = mvnrnd(zeros(Lw, 1), eye(Lw), gen_N)';
    gen_x = [gen_t; gen_w];
    gen_x = bsxfun(@minus, gen_x, mean(gen_x, 2));
    gen_x = gen_x';
    temp_C = cov(gen_x);
    [eig_Vec, eig_Val] = eig(temp_C);
    temp_diag = diag(eig_Val);
    temp_inv_dig = 1./temp_diag;
    temp_inv_dig(round(temp_diag, 8)==0) = 0;
    inv_sqrt_diag = diag(sqrt(temp_inv_dig));
    gen_x = gen_x * eig_Vec * inv_sqrt_diag;
    
    temp_C = [Gammat, zeros(Lt, Lw); zeros(Lw, Lt), Gammaw];
    [eig_Vec, eig_Val] = eig(temp_C);
    diag_Val = diag(eig_Val);
    diag_Val(diag_Val<eps) = eps;
    eig_Val = diag(diag_Val);
    gen_x = gen_x * sqrt(eig_Val) * eig_Vec';
    gen_x = gen_x';
    gen_y = bsxfun(@plus, A*gen_x, b) + mvnrnd(zeros(D, 1), Sigma, gen_N)';

    gen_y(gen_y<0)=0;
    gen_y(gen_y>1)=1;
    theta.ct = ct;
    theta.Gammat = Gammat;
    theta.At = At;
    theta.Aw = Aw;
    theta.A = A;
    theta.b = b;
    theta.Sigma = Sigma;
end


%% backup
% gen_t = mvnrnd(ct, Gammat, gen_N)'; % Nx1
% gen_w = mvnrnd(zeros(Lw, 1), eye(Lw), gen_N)';
% gen_x = [gen_t; gen_w];
% gen_x = bsxfun(@minus, gen_x, mean(gen_x, 2));
% gen_x = gen_x';
% temp_C = cov(gen_x);
% [eig_Vec, eig_Val] = eig(temp_C);
% temp_diag = diag(eig_Val);
% temp_inv_dig = 1./temp_diag;
% temp_inv_dig(round(temp_diag, 8)==0) = 0;
% inv_sqrt_diag = diag(sqrt(temp_inv_dig));
% gen_x = gen_x * eig_Vec * inv_sqrt_diag;
% 
% temp_C = [Gammat, zeros(Lt, Lw); zeros(Lw, Lt), Gammaw];
% [eig_Vec, eig_Val] = eig(temp_C);
% diag_Val = diag(eig_Val);
% diag_Val(diag_Val<eps) = eps;
% eig_Val = diag(diag_Val);
% gen_x = gen_x * sqrt(eig_Val) * eig_Vec';
% gen_x = gen_x';
% gen_y = bsxfun(@plus, A*gen_x, b) + mvnrnd(zeros(D, 1), Sigma, gen_N)';
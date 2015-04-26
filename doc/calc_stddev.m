for i=1:length(collisions)
    results(i,:) = [ones(1,collisions(i)) zeros(1,30000-collisions(i))];
end

SEM = std(results') / sqrt(length(results(1,:)));
ts = tinv([0.025  0.975],length(results(1,:))-1);      % T-Score
CI = [mean(results'); mean(results')] + ts'*SEM;                      % Confidence Intervals
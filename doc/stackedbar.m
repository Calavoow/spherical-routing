ring = csvread('ring.csv');
sphere = csvread('sphere.csv');

%ringX = ring(:, 1);
%ringY = ring(:, 2);
%ringZ = ring(:, 3:end-1);
%ringZ(ringZ == -1) = NaN;
%
%ringZ2 = ringZ;
%ringZ2(isnan(ringZ2)) = [];
%stackEls = unique(ringZ2);
%
%ZEls = zeros(size(ringZ,1), length(stackEls));
%for i=1:size(ringZ,1)
%   for j=1:length(stackEls)
%     count = sum(ringZ(i,:) == stackEls(j));
%     ZEls(i,j) = count / length(ringZ);
%   end
%end
%
%X = unique(ringX);
%Y = unique(ringY);
%Z = zeros(length(X), length(Y), length(stackEls));
%for i=1:length(X)
%  for j=1:length(Y)
%    for k=1:size(ZEls,2)
%      Z(i,j,k) = ZEls((i-1)*length(Y) + j,k);
%    end
%  end
%end
%
%labels = cellstr(num2str(X));
%
%fig = plotBarStackGroups(Z, labels);
%leg = strcat('Layer', {' '}, cellstr(num2str(stackEls)), {' '});
%legend(leg);
%legend("boxoff");
%xlabel('Number of nodes (N)');
%ylabel('Fraction of collisions');
%ylim([0,1]);
%set(gca, 'ygrid', 'on');
%print('stackedbar-ring.pdf', '-dpdf', '-color', '-S800,450');

sphereX = sphere(:, 1);
sphereY = sphere(:, 2);
sphereZ = sphere(:, 3:end-1);
sphereZ(sphereZ == -1) = NaN;

sphereZ2 = sphereZ;
sphereZ2(isnan(sphereZ2)) = [];
stackEls = unique(sphereZ2);

ZEls = zeros(size(sphereZ,1), length(stackEls));
for i=1:size(sphereZ,1)
   for j=1:length(stackEls)
     count = sum(sphereZ(i,:) == stackEls(j));
     ZEls(i,j) = count / length(sphereZ);
   end
end

X = unique(sphereX);
Y = unique(sphereY);
Z = zeros(length(X), length(Y), length(stackEls));
for i=1:length(X)
  for j=1:length(Y)
    for k=1:size(ZEls,2)
      Z(i,j,k) = ZEls((i-1)*length(Y) + j,k);
    end
  end
end

labels = cellstr(num2str(X));

plotBarStackGroups(Z, labels);
ylabel('Fraction of collisions');
xlabel('Number of nodes (N)');
leg = strcat('Layer', {' '}, cellstr(num2str(stackEls)));
%ylim([0,1]);
set(gca, 'ygrid', 'on');
legend(leg);
legend("boxoff");
text(1.05,0.7, ...
  {'2-10 concurrent','requests'}, ...
  'HorizontalAlignment', 'center'
);
%annotation('arrow', [0.13,0.19], [0.85,0.85], 'headwidth', 3.5);
print('stackedbar-sphere.pdf', '-dpdf', '-color', '-S800,450');
ring = csvread("ring.csv");
sphere = csvread("sphere.csv");

ringX = ring(:, 1);
ringY = ring(:, 2);
ringZ = ring(:, 3:end);
ringZ(ringZ == -1) = NaN;

ringZ2 = ringZ;
ringZ2(isnan(ringZ2)) = [];
stackEls = unique(ringZ2);

ZEls = zeros(size(ringZ,1), length(stackEls));
for i=1:size(ringZ,1)
   for j=1:length(stackEls)
     count = sum(ringZ(i,:) == stackEls(j));
     ZEls(i,j) = count;
   end
end

X = unique(ringX);
Y = unique(ringY);
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
xlabel('Number of nodes (N)');
ylabel('Number of collisions');
leg = strcat('Layer ', cellstr(num2str(stackEls)));
legend(leg);
title('Number of collisions for concurrent routing on a ring.');
print('stackedbar-ring.pdf', '-deps2');

sphereX = sphere(:, 1);
sphereY = sphere(:, 2);
sphereZ = sphere(:, 3:end);
sphereZ(sphereZ == -1) = NaN;

sphereZ2 = sphereZ;
sphereZ2(isnan(sphereZ2)) = [];
stackEls = unique(sphereZ2);

ZEls = zeros(size(sphereZ,1), length(stackEls));
for i=1:size(sphereZ,1)
   for j=1:length(stackEls)
     count = sum(sphereZ(i,:) == stackEls(j));
     ZEls(i,j) = count;
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
ylabel('Number of collisions');
xlabel('Number of nodes (N)');
leg = strcat('Layer ', cellstr(num2str(stackEls)));
legend(leg);
title('Number of collisions for concurrent routing on a sphere.');
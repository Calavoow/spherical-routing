data = load('collision_ico.csv');
samples = 10000;
X = (0:7)';
fraction = data / samples;

bar(X,fraction);
xlabel("Number of subdivision performed.");
ylabel("Collisions / samples");
print(["collision.pdf", "-dpdf"]);

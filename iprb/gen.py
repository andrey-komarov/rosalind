from random import *

population = [0, 0, 1, 1, 2, 2]

ans = 0
total = 0
for it in range(1000000):
    a = choice(list(range(len(population))))
    b = choice(list(range(len(population))))
    while a == b:
        a = choice(list(range(len(population))))
        b = choice(list(range(len(population))))
    a = population[a]
    b = population[b]
    total += 1
    if a == 0 and b == 0:
        continue
    if a == 2 or b == 2:
        ans += 1
        continue
    if a == 1 and b == 1:
        if randint(1, 4) != 1:
            ans += 1
        continue
    if randint(1, 2) == 1:
        continue
    ans += 1

print (ans / total)


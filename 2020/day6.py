from functools import reduce


def part1(d):
    count = 0
    for i in d:
        group =set()
        for j in i:
            if j != " ":
                group.add(j)
        count += len(group)
    return count

with open("input6.txt") as f:
    rawData = f.read().split('\n\n')
    data = []
    for d in rawData:
        j = d.replace("\n"," ")
        data.append(j)
    
print(part1(data))

print(sum([
    len(reduce(lambda set_1, set_2: set_1.intersection(set_2), [set(word) for word in g if word])) for g in
    [group.split('\n') for group in open('input6.txt').read().split('\n\n')]
    ]))
    
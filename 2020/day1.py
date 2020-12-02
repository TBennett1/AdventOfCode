def part1(d, target):
    lookup = {}
    for i,num in enumerate(d):
        if target - num in lookup:
            return (target-num)*num
        lookup[num] = i

def part2(d, target):
    for i in range(len(d)):
        newTarget = target-d[i]
        for j in range(i+1,len(d)):
            newNewTarget = newTarget - d[j]
            for k in range(j+1,len(d)):
                if d[k] == newNewTarget:
                    return d[i]*d[j]*d[k]

                
        

with open("input.txt") as f:
    rawData = f.read().split()
    data = []
    for x in rawData:
        data.append(int(x))

print(part1(data,2020))
print(part2(data,2020))
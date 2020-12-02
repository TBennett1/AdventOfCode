def part1(d):
    validPasswords = []
    for policy,password in d:
        letter = policy[-1]
        number = policy[:-1].split("-")
        low = int(number[0])
        high = int(number[1])
        if password.count(letter)<=high and password.count(letter)>=low:
            validPasswords.append(password)
    return len(validPasswords)
        
def part2(d):
    vaildPasswords = []
    for policy,password in d:
        letter=policy[-1]
        positions = policy[:-1].split("-")
        if (password[int(positions[0])-1] == letter and password[int(positions[1])-1] != letter) or \
            (password[int(positions[0])-1] != letter and password[int(positions[1])-1] == letter):
            vaildPasswords.append(password)

    return len(vaildPasswords)

with open("input2.txt") as f:
    rawData = f.read().split('\n')
    data = [x.split(": ") for x in rawData]

print(part1(data))
print(part2(data))
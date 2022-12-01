import re
neededFields = sorted(['byr','iyr','eyr','hgt','hcl','ecl','pid'])

def part1(data):
    count = 0
    for passport in data:
        fields = sorted(passport.keys())
        if fields == neededFields:
            count+=1
    return count

def part2(data):
    mandatory_field_validators = {
        'byr': lambda x: re.match('^\d{4}$', x) and 1920 <= int(x) <= 2002,
        'iyr': lambda x: re.match('^\d{4}$', x) and 2010 <= int(x) <= 2020,
        'eyr': lambda x: re.match('^\d{4}$', x) and 2020 <= int(x) <= 2030,
        'hgt': lambda x: (re.match('^\d{3}cm$', x) and 150 <= int(x.replace('cm', '')) <= 193) or (re.match('^\d{2}in$', x) and 59 <= int(x.replace('in', '')) <= 76),
        'hcl': lambda x: re.match('^#[0-9a-f]{6}$', x),
        'ecl': lambda x: x in ('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'),
        'pid': lambda x: re.match('^\d{9}$', x)
    }
    count=0 
    for passport in data:
        count += all(func(passport.get(key, '')) for key, func in mandatory_field_validators.items())
    return count


with open("input4.txt") as f:
    rawData = f.read().split('\n\n')
    passports=[]
    for data in rawData:
        data = data.replace('\n',' ').split(" ")
        dic={}
        for entry in data:
            x,y = entry.split(":")
            if x=='cid':
                continue
            dic[x]=y
        passports.append(dic)
    

print(part1(passports))
print(part2(passports))
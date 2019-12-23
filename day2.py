with open("day2Input.txt", "r") as f:
    c = f.read()
    line = c.split(",")


initMem = list(map(int, line))

def set_input(memory, inp):
    mem = memory[:]

    for pos,val in inp.items():
        mem[pos] = val
    return mem



def intCode(tape):
    mem = tape[:]
    ptr = 0

    while mem[ptr] != 99:
        cmd = mem[ptr:]
        opc = cmd[0]

        if opc == 1:
            a, b, c = cmd[1:4]
            mem[c] = mem[a] + mem[b]
            ptr += 4
        elif opc == 2:
            a, b, c = cmd[1:4]
            mem[c] = mem[a] * mem[b]
            ptr += 4
        else:
            raise Exception("run: unknown opcode")
    return mem[0]

#part 1
part_one = intCode(set_input(initMem, {1: 12, 2: 2}))
print(part_one)

# part 2
for noun, verb in ((a,b) for a in range(100) for b in range(100)):
    if 19690720 == intCode(set_input(initMem, {1: noun, 2: verb})):
        ans = 100*noun+verb
        print(ans)
        break

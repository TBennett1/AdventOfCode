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
        elif opc == 3:
            a = cmd[1]
            b = int(input("Enter Value to be stored"))
            mem[a] = b
            ptr += 2
        elif opc == 4:
            a = cmd[1]
            print(mem[a])
            ptr += 2
        else:
            raise Exception("run: unknown opcode")
    return mem[0]

def parse_data() -> list[str]:
    lines = []
    with open("input.txt") as file:
        for line in file:
            lines.append(line.rstrip())
    return lines


def solve_p1(program: list[str]) -> int:
    bit_mask = "XXXXXXXXXXXXXXXXXXX"
    mem = {}

    def apply_mask(_number: int, _mask: str) -> int:
        res = _number
        and_mask = int("".join(["1" if x == 'X' else "0" for x in _mask]), 2)
        res = res & and_mask
        or_mask = int("".join([x if x in "01" else "0" for x in _mask]), 2)
        res = res | or_mask
        return res

    for line in program:
        lvalue, rvalue = line.split(" = ")
        if lvalue == "mask":
            bit_mask = rvalue
        elif lvalue[:3] == "mem":
            address = lvalue[4:-1]
            result = apply_mask(int(rvalue), bit_mask)
            mem[address] = result

    return sum(mem.values())


print(solve_p1(parse_data()))

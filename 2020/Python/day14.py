import itertools


def parse_data() -> list[str]:
    lines = []
    with open("input.txt") as file:
        for line in file:
            lines.append(line.rstrip())
    return lines


def solve_p1(program: list[str]) -> int:
    bit_mask = ""
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


def solve_p2(program: list[str]) -> int:
    bit_mask = ""
    mem = {}

    def apply_mask(_address: str, _mask: str) -> list[str]:
        res = ['0' if x not in "01" else x for x in format(int(_address), "36b")]
        for i in range(len(_mask)):
            if _mask[i] == "1":
                res[i] = "1"
            elif _mask[i] == "X":
                res[i] = "X"
        return res

    def resolve_floating(_address: list[str]) -> list[int]:
        combinations = list(itertools.product(["0", "1"], repeat=_address.count("X")))
        res = []
        for combo in combinations:
            res.append("".join(_address))
            for c in combo:
                res[-1] = res[-1].replace('X', c, 1)
        return res

    for line in program:
        lvalue, rvalue = line.split(" = ")
        if lvalue == "mask":
            bit_mask = rvalue
        elif lvalue[:3] == "mem":
            address = lvalue[4:-1]
            result = apply_mask(address, bit_mask)
            floating_addresses = resolve_floating(result)
            for floating_address in floating_addresses:
                mem[floating_address] = int(rvalue)

    return sum(mem.values())


print(solve_p1(parse_data()))
print(solve_p2(parse_data()))

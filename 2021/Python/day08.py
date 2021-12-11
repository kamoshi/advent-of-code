from functools import reduce


def load() -> list[tuple[list[str], ...]]:
    with open('../.input/day08', 'r') as f:
        return [tuple(map(str.split, line.split('|'))) for line in f.read().splitlines()]


def solve1() -> int:
    return sum(
        len(list(filter(lambda x: len(x) in [2, 3, 4, 7], output)))
        for _, output in load()
    )


A = 0b0000001
B = 0b0000010
C = 0b0000100
D = 0b0001000
E = 0b0010000
F = 0b0100000
G = 0b1000000

n_to_s = {
    0: A | B | C | E | F | G,
    1: C | F,
    2: A | C | D | E | G,
    3: A | C | D | F | G,
    4: B | C | D | F,
    5: A | B | D | F | G,
    6: A | B | D | E | F | G,
    7: A | C | F,
    8: A | B | C | D | E | F | G,
    9: A | B | C | D | F | G,
}
    
s_to_n = {
    A | B | C | E | F | G: 0,
    C | F: 1,
    A | C | D | E | G: 2,
    A | C | D | F | G: 3,
    B | C | D | F: 4,
    A | B | D | F | G: 5,
    A | B | D | E | F | G: 6,
    A | C | F: 7,
    A | B | C | D | E | F | G: 8,
    A | B | C | D | F | G: 9,
}

possible = {
    2: n_to_s[1],
    3: n_to_s[7],
    4: n_to_s[4],
}


def negate(string: str):
    return list(filter(lambda x: x not in string, "abcdefg"))


def check_pattern(pattern: list[str], mapping: dict[str, int]):
    def check_legal(_str: str):
        return reduce(lambda a, c: a | mapping[c], _str, 0) in s_to_n
    return all(map(check_legal, pattern))


def find_mapping(pattern: list[str]):
    domains = {
        char: 0b1111111
        for char in 'abcdefg'
    }

    # remove obvious impossible choices
    for activation in pattern:
        length = len(activation)
        if length in possible:
            for char in activation:
                domains[char] &= possible[length]
            for char in negate(activation):
                domains[char] &= ~possible[length]

    # bruteforce search
    search: list[dict[str, int]] = [{}]
    for char, domain in domains.items():
        deeper = []
        for i in range(7):
            if domain & (1 << i):
                for s in search:
                    new = s.copy()
                    new[char] = (1 << i)
                    deeper.append(new)
        search = deeper

    for mapping in search:
        if check_pattern(pattern, mapping):
            return mapping


def string_to_int(string: str, mapping: dict) -> int:
    return s_to_n[reduce(lambda a, c: a | mapping[c], string, 0)]


def solve2() -> int:
    return sum(
        reduce(lambda a, c: a * 10 + string_to_int(c, find_mapping(pattern)), output, 0)
        for pattern, output in load()
    )


if __name__ == '__main__':
    print(solve1())  # 301
    print(solve2())  # 908067

from functools import reduce


def load() -> list[str]:
    with open('../.input/day10', 'r') as f:
        return list(map(str.strip, f.readlines()))


opening, closing = ['(', '[', '{', '<'], [')', ']', '}', '>']
scores = { ")": 3, "]": 57, "}": 1197, ">": 25137 }


def validate(line: str) -> int:
    stack = []
    for char in line:
        if char in opening:
            stack.append(char)
        else:
            if opening.index(stack.pop()) != closing.index(char):
                return scores[char]
    return 0


def solve1() -> int:
    return sum(map(validate, load()))


scores2 = { ")": 1, "]": 2, "}": 3, ">": 4 }
completion = { o: c for o, c in zip(opening, closing) }


def autocomplete(line: str) -> str:
    stack = []
    for char in line:
        if char in opening:
            stack.append(char)
        else:
            stack.pop()
    return list(map(lambda x: completion[x], stack))


def solve2() -> int:
    lines = list(filter(lambda line: validate(line) == 0, load()))
    return sorted([
        reduce(lambda acc, char: 5 * acc + scores2[char], reversed(line), 0)
        for line in map(autocomplete, lines)
    ])[len(lines) // 2]


if __name__ == '__main__':
    print(solve1())  # 345441
    print(solve2())  # 3235371166

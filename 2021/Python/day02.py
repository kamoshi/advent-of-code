from functools import reduce
import re
from typing import Tuple


pattern = re.compile('^([a-z]+) ([0-9]+)$')


def load_input() -> list[Tuple[str, int]]:
    with open('../.input/day02', 'r') as f:
        return [
            (match.group(1), int(match.group(2))) for match
            in (pattern.search(line.strip()) for line in f.readlines())
        ]


def solve1() -> int:
    numbers = load_input()
    instructions = {
        "forward": lambda arg, x, y: (x + arg, y),
        "up": lambda arg, x, y: (x, y + arg),
        "down": lambda arg, x, y: (x, y - arg),
    }

    x, y = reduce(lambda pos, item: instructions[item[0]](item[1], *pos), numbers, (0, 0))
    return abs(x) * abs(y)


def solve2() -> int:
    numbers = load_input()
    instructions = {
        "forward": lambda arg, x, y, a: (x + arg, y + a * arg, a),
        "up": lambda arg, x, y, a: (x, y, a - arg),
        "down": lambda arg, x, y, a: (x, y, a + arg),
    }

    x, y, _ = reduce(lambda pos, item: instructions[item[0]](item[1], *pos), numbers, (0, 0, 0))
    return abs(x) * abs(y)


if __name__ == '__main__':
    print(solve1())
    print(solve2())

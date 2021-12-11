from typing import Tuple


def load() -> list[Tuple[list[str], list[str]]]:
    with open('../.input/day08', 'r') as f:
        return [tuple(map(str.split, line.split('|'))) for line in f.read().splitlines()]


def solve1() -> int:
    return sum(
        len(list(filter(lambda x: len(x) in [2, 3, 4, 7], output)))
        for _, output in load()
    )


if __name__ == '__main__':
    print(solve1())  # 

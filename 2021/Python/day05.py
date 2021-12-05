import re
from itertools import repeat
from typing import Iterable, Tuple


pattern = re.compile(r'(\d+),(\d+) -> (\d+),(\d+)')


def loader() -> list[Tuple[Tuple[int, int], Tuple[int, int]]]:
    with open('../.input/day05', 'r') as f:
        return [
            ((int(x1), int(y1)), (int(x2), int(y2)))
            for x1, y1, x2, y2
            in pattern.findall(f.read())
        ]


def find_points_in_line(x1, y1, x2, y2, skip_diagonal: bool = False) -> Iterable[Tuple[int, int]]:
    if skip_diagonal and x1 != x2 and y1 != y2:
        return []
    if x2 < x1 and y2 < y1:
        return find_points_in_line(x2, y2, x1, y1)
    if x2 < x1:
        y = (y for y in range(y1, y2 + 1)) if y1 != y2 else repeat(y1)
        return zip(range(x1, x2 - 1, -1), y)
    if y2 < y1:
        x = (x for x in range(x1, x2 + 1)) if x1 != x2 else repeat(x1)
        return zip(x, range(y1, y2 - 1, -1))
    if x1 == x2:
        return zip(repeat(x1), range(min(y1, y2), max(y1, y2) + 1))
    if y1 == y2:
        return zip(range(min(x1, x2), max(x1, x2) + 1), repeat(y1))
    return zip(range(x1, x2 + 1), range(y1, y2 + 1))


def solve1(skip_diagonal=True) -> int:
    visited_pts = set()
    crossed_pts = set()
    for (start, end) in loader():
        for x, y in find_points_in_line(*start, *end, skip_diagonal):
            if (x, y) in visited_pts:
                crossed_pts.add((x, y))
            else:
                visited_pts.add((x, y))
    return len(crossed_pts)


def solve2() -> int:
    return solve1(skip_diagonal=False)
    

if __name__ == '__main__':
    print(solve1())  # 5690
    print(solve2())  # 17741

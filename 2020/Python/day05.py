from typing import Tuple


def parse_input() -> list[Tuple[str, int, int, int]]:

    def find_position(code: str) -> range:
        search_range = range(0, 2**len(code)-1)
        for char in code:
            spaces = search_range.stop - search_range.start + 1
            if char in ['F', 'L']:
                search_range = range(search_range.start, search_range.stop - spaces//2)
            elif char in ['B', 'R']:
                search_range = range(search_range.start + spaces//2, search_range.stop)
        return search_range

    passes = []
    with open("input.txt") as file:
        for line in file:
            line_ = line.rstrip()
            row = find_position(line_[:7]).start
            col = find_position(line_[7:]).start
            passes.append((line_, row, col, row*8+col))

    return passes


BOARDING_PASSES = parse_input()
SORTED_PASSES = sorted(BOARDING_PASSES, key=lambda t: t[3])


def solve_p1() -> int:
    return SORTED_PASSES[-1][3]


def solve_p2() -> int:
    for i in range(1, len(SORTED_PASSES)-2):
        prev_ = SORTED_PASSES[i-1][3]
        curr_ = SORTED_PASSES[i][3]
        if prev_+1 != curr_:
            return curr_-1
    return -1


print(solve_p1())
print(solve_p2())

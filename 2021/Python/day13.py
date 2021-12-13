import re

import numpy as np


pattern_dots = re.compile(r"([0-9]+),([0-9]+)")
pattern_folds = re.compile(r"fold along ([xy])=([0-9]+)")


def load():
    with open("../.input/day13") as f:
        dots, folds = f.read().split("\n\n")
    dots = [(int(x), int(y)) for x, y in pattern_dots.findall(dots)]
    matrix = np.zeros((1 + max(y for _, y in dots), 1 + max(x for x, _ in dots)), dtype=bool)
    for x, y in dots:
        matrix[y, x] = True
    return matrix, [(axis, int(offset)) for axis, offset in pattern_folds.findall(folds)]


def fold(matrix: np.ndarray, axis, offset) -> np.ndarray:
    if axis == "x":
        result = matrix[:, :offset]
        right = np.fliplr(matrix[:, offset+1:])
        if result.shape == right.shape:
            result |= right
        else:
            result[:, -right.shape[1]:] |= right
    else:
        result = matrix[:offset, :]
        bottom = np.flipud(matrix[offset+1:, :])
        if result.shape == bottom.shape:
            result |= bottom
        else:
            result[-bottom.shape[0]:, :] |= bottom
    return result


def solve1() -> int:
    dots, folds = load()
    return fold(dots, *folds[0]).sum()


def solve2() -> None:
    dots, folds = load()
    for axis, offset in folds:
        dots = fold(dots, axis, offset)
    [print("".join(line)) for line in np.where(dots, "#", " ")]


if __name__ == "__main__":
    print(solve1())  # 638
    solve2()  # CJCKEBAPB

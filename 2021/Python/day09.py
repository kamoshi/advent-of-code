from functools import reduce

import numpy as np
from numpy.lib.stride_tricks import as_strided


def load() -> np.ndarray:
    with open("../.input/day09") as f:
        lines = list(map(str.strip, f.readlines()))
    return np.array([int(num) for line in lines for num in line]).reshape((len(lines), -1))


def windows(target, shape=(3, 3), stride: int = 1):
    target = np.pad(target, 1, 'constant', constant_values=9)
    (t_h, t_w), (w_h, w_w) = target.shape, shape
    out_shape = (1 + (t_h - w_h) // stride, 1 + (t_w - w_w) // stride, w_h, w_w)
    out_strides = (target.strides[0] * stride, target.strides[1] * stride, target.strides[0], target.strides[1])
    return as_strided(target, shape=out_shape, strides=out_strides)


def solve1() -> int:
    hmap = load()
    return np.sum((hmap + 1) * (hmap == windows(hmap).min(axis=(2, 3))))


def find_basin(areas, visited, x, y) -> int:
    if visited[x, y]:
        return 0
    visited[x, y] = True
    area = 1
    for dx, dy in [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]:
        if (
            0 <= dx < areas.shape[0] and
            0 <= dy < areas.shape[1] and
            areas[dx, dy]
        ):
            area += find_basin(areas, visited, dx, dy)
    return area


def solve2() -> int:
    hmap = load()
    areas = hmap != 9
    starts = np.argwhere(hmap == windows(hmap).min(axis=(2, 3)))
    visited = np.zeros(hmap.shape, dtype=np.bool8)
    basins = [find_basin(areas, visited, *start) for start in starts]
    return reduce(lambda acc, num: acc * num, sorted(basins, reverse=True)[:3], 1)


if __name__ == "__main__":
    print(solve1())  # 522
    print(solve2())  # 916688

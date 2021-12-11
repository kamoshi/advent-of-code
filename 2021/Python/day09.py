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
    mins = windows(hmap).min(axis=(2, 3))
    return np.sum((hmap + 1) * (hmap == mins))


def solve2() -> int:
    print(load())


if __name__ == "__main__":
    print(solve1())  # 522
    print(solve2())  # None

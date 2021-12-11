import numpy as np
from numpy.lib.stride_tricks import as_strided


def load():
    with open('../.input/day11') as f:
        lines = list(map(str.strip, f.readlines()))
    return np.array([int(num) for line in lines for num in line]).reshape((-1, len(lines)))


def windows(target, shape=(3, 3), stride: int = 1):
    target = np.pad(target, 1, 'constant', constant_values=0)
    (t_h, t_w), (w_h, w_w) = target.shape, shape
    out_shape = (1 + (t_h - w_h) // stride, 1 + (t_w - w_w) // stride, w_h, w_w)
    out_strides = (target.strides[0] * stride, target.strides[1] * stride, target.strides[0], target.strides[1])
    return as_strided(target, shape=out_shape, strides=out_strides)


def step(input: np.ndarray):
    current = input + np.ones(input.shape, dtype=np.int32)
    flashed = np.zeros(input.shape, dtype=np.bool8)

    while np.any(now_flashed := ((current * ~flashed) > 9)):
        convolution = np.tensordot(
            windows(now_flashed.astype(np.int32)),
            np.array([[1, 1, 1], [1, 0, 1], [1, 1, 1]]),
            axes=((2, 3), (0, 1))
        )
        flashed |= now_flashed
        current = current + convolution * ~flashed

    return np.where(current > 9, 0, current), flashed


def solve1() -> int:
    input = load()
    flash_count = 0
    for _ in range(100):
        input, flashed = step(input)
        flash_count += np.sum(flashed)
    return flash_count


def solve2() -> int:
    input = load()

    steps = 0
    while (steps := steps + 1):
        input, flashed = step(input)
        if np.all(flashed):
            return steps


if __name__ == '__main__':
    print(solve1())  # 1669
    print(solve2())  # 351
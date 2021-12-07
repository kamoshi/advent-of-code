import numpy as np


def load() -> list[int]:
    with open('../.input/day07') as f:
        return [int(x) for x in f.readline().split(',')]


def solve1() -> int:
    numbers = load()
    median = sorted(numbers)[len(numbers) // 2]
    return sum(abs(x - median) for x in numbers)


def nth_sum(n: int) -> int:
    return n * (n + 1) // 2


def solve2() -> int:
    numbers = np.array(load())
    search_vector = np.arange(0, max(numbers) + 1)
    search_matrix = np.tile(search_vector, (len(numbers), 1)).T
    distance = abs(search_matrix - numbers)
    fuel_expended = nth_sum(distance)
    return min(fuel_expended.sum(axis=1))
    

if __name__ == '__main__':
    print(solve1())  # 356179
    print(solve2())  # 99788435

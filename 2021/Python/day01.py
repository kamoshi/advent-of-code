

def solve1() -> int:
    with open('../.input/day01', 'r') as f:
        numbers: list[int] = [int(line) for line in f.readlines()]
    return sum((n > p) for p, n in zip(numbers[:-1], numbers[1:]))


def solve2() -> int:
    with open('../.input/day01', 'r') as f:
        numbers: list[int] = [int(line) for line in f.readlines()]
    return sum((sum(n) > sum(p)) for p, n in zip(
        zip(numbers[:-3], numbers[1:-2], numbers[2:-1]),
        zip(numbers[1:-2], numbers[2:-1], numbers[3:])
    ))
    

if __name__ == '__main__':
    print(solve1())  # 1766
    print(solve2())  # 1797

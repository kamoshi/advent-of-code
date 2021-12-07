def load() -> list[int]:
    with open('../.input/day07') as f:
        return [int(x) for x in f.readline().split(',')]


def solve1() -> int:
    numbers = load()
    median = sorted(numbers)[len(numbers) // 2]
    return sum(abs(x - median) for x in numbers)

if __name__ == '__main__':
    print(solve1())  # 356179

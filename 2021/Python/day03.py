from typing import DefaultDict


def load_input() -> list[str]:
    with open('../.input/day03', 'r') as f:
        return [line.strip() for line in f.readlines()]


def solve1() -> int:
    input = load_input()
    cache = [{"0": 0, "1": 0} for _ in range(len(input[0]))]

    for binary in input:
        for i, c in enumerate(binary):
            cache[i][c] += 1
    
    gamma, epsilon = map(sum, zip(*[
        (2**i, 0) if c["1"] > c["0"] else (0, 2**i)
        for i, c in enumerate(reversed(cache))
    ]))
    return gamma * epsilon


def find_single(input: list[str], reverse: bool = False) -> int:
    cache = [{"0": 0, "1": 0} for _ in range(len(input[0]))]
    remaining = input[:]
    
    for binary in remaining:
        for i, digit in enumerate(binary):
            cache[i][digit] += 1
    
    for i, c in enumerate(cache):
        if len(remaining) == 1:
            break

        generator = (entry for entry in remaining)
        remaining = []
        winning_digit = ("0" if c["0"] <= c["1"] else "1") if reverse else ("1" if c["1"] >= c["0"] else "0")
        for binary in generator:
            if binary[i] == winning_digit:
                remaining.append(binary)
            else:
                for digit, cache_dict in zip(binary, cache):
                    cache_dict[digit] -= 1
    
    return remaining[0]


def solve2() -> int:
    input = load_input()
    oxygen, carbon = input[:], input[:]
    res_o = sum(2**int(i) for i, x in enumerate(reversed(find_single(oxygen, reverse=False))) if x == "1")
    res_c = sum(2**int(i) for i, x in enumerate(reversed(find_single(carbon, reverse=True))) if x == "1")
    return res_o * res_c


if __name__ == "__main__":
    print(solve1())
    print(solve2())

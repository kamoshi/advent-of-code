import re

input_pattern = re.compile(r"([A-Z]+) -> ([A-Z]+)")


def load():
    with open("../.input/day14", "r") as f:
        start, rest = f.read().split("\n\n")
    return start, input_pattern.findall(rest)


def step(start, rules):
    output = []
    for pair in zip(start, start[1:]):
        c1, c2 = pair
        output.append(c1)
        output.append(rules[c1 + c2])
    output.append(start[-1])
    return output


def find_counts(start: str, rules: dict[str, str], epochs: int) -> dict[str, int]:
    for _ in range(epochs):
        start = step(start, rules)

    chars: dict[str, int] = {}
    for c in start:
        chars[c] = chars.get(c, 0) + 1
    return chars


def solve1() -> int:
    start, rules = load()
    rules = {k: v for k, v in rules}
    chars = find_counts(start, rules, 10)
    values = list(sorted(chars.values(), reverse=True))
    return values[0] - values[-1]


def find_counts_optimized(start: str, rules: dict[str, str], epochs: int) -> dict[str, int]:
    chars: dict[str, int] = {}
    for c1, c2 in zip(start, start[1:]):
        chars[c1 + c2] = chars.get(c1 + c2, 0) + 1

    for _ in range(epochs):
        new_chars: dict[str, int] = {}
        for combo, count in chars.items():
            if combo in rules:
                insert = rules[combo]
                new_chars[combo[0] + insert] = new_chars.get(combo[0] + insert, 0) + chars[combo]
                new_chars[insert + combo[1]] = new_chars.get(insert + combo[1], 0) + chars[combo]
        chars = new_chars

    final_chars: dict[str, int] = {start[-1]: 1}
    for combo, count in chars.items():
        final_chars[combo[0]] = final_chars.get(combo[0], 0) + count
    return final_chars


def solve2():
    start, rules = load()
    rules = {k: v for k, v in rules}
    chars = find_counts_optimized(start, rules, 40)

    values = list(sorted(chars.values(), reverse=True))
    return values[0] - values[-1]


if __name__ == "__main__":
    print(solve1())
    print(solve2())

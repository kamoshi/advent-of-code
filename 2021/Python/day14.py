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
        output.append(rules[c1+c2])
    output.append(start[-1])
    return output


def solve1():
    start, rules = load()
    rules = {k: v for k, v in rules}

    for _ in range(10):
        start = step(start, rules)

    chars = {}
    for c in start:
        chars[c] = chars.get(c, 0) + 1

    values = list(sorted(chars.values(), reverse=True))
    return values[0] - values[-1]


if __name__ == "__main__":
    print(solve1())


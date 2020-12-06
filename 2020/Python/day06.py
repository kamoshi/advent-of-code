import functools


def parse_input() -> list[list[str]]:
    groups = [[]]

    with open("input.txt") as file:
        for line in file:
            line_ = line.rstrip()
            if len(line_) > 0:
                groups[-1].append(line_)
            else:
                if not groups[-1] == []:
                    groups.append([])

    return groups


def solve_p1(groups: list[list[str]]) -> int:

    def count(group: list[str]) -> int:
        chars = set()
        for line in group:
            for char in line:
                chars.add(char)
        return len(chars)

    return sum(map(count, groups))


def solve_p2(groups: list[list[str]]) -> int:

    def count_intersection(group: list[str]) -> int:
        sets = []
        for line in group:
            new_set = set()
            for char in line:
                new_set.add(char)
            sets.append(new_set)
        result_set = functools.reduce(set.intersection, sets)
        return len(result_set)

    return sum(map(count_intersection, groups))


GROUPS = parse_input()
print(solve_p1(GROUPS))
print(solve_p2(GROUPS))

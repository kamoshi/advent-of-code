import re

CHILD_PARSE = re.compile(r"(\d+) (.*)")
PARSED_DICT:  dict[str, list[(int, str)]] = {}

with open("input.txt") as file:
    for line in file:
        _line = line.strip()
        (a, b) = map(str.strip, _line.split("contain"))
        parent = a[:-1]
        c = list(map(str.strip, b.split(",")))
        children = []
        for child in c:
            cleaned = child.replace("bags", "bag").replace(".", "")
            children += CHILD_PARSE.findall(cleaned)
        PARSED_DICT[parent] = children


def solve_p1() -> int:

    def check_contain(start: str, goal: str) -> bool:
        if start in PARSED_DICT:
            for _, child in PARSED_DICT[start]:
                if child == goal or check_contain(child, goal):
                    return True
        return False

    def check_all(name: str) -> set:
        result = set()
        for bag in PARSED_DICT.keys():
            if check_contain(bag, name):
                result.add(bag)
        return result

    return len(check_all("shiny gold bag"))


def solve_p2() -> int:

    def count_contained(name: str) -> int:
        result = 1
        if name in PARSED_DICT:
            for number, child in PARSED_DICT[name]:
                result += int(number) * count_contained(child)
        return result

    return count_contained("shiny gold bag")-1  # sub 1 because the root counts as 0

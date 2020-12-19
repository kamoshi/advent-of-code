import re
from typing import Union
from functools import partial


def parse_data():
    rules = {}
    strings = []
    with open("input.txt") as file:
        for line in file:
            line = line.rstrip()

            if not line:
                continue

            if line[0].isdigit():
                rule_id, rule_or = line.split(": ")
                rule_id = int(rule_id)
                rule_or = rule_or.split(" | ")
                rule_entry = []
                if rule_or[0][0] == '"':
                    rules[rule_id] = rule_or[0][1]
                else:
                    for rule in rule_or:
                        rule_entry.append(list(map(int, rule.split(" "))))
                    rules[rule_id] = rule_entry
            else:
                strings.append(line)

    return rules, strings


def solve_p1(rules, strings) -> int:

    def convert_to_regex(rules, rule_idx: int) -> str:
        rule = rules[rule_idx]
        if isinstance(rule, str):
            return rule
        else:
            out = "("
            map_func = partial(convert_to_regex, rules)
            for rule_option in rule:
                out += "".join(map(map_func, rule_option)) + "|"
            return out[:-1] + ")"

    pattern_str = convert_to_regex(rules, 0)+"$"
    pattern = re.compile(pattern_str)
    matching = 0
    for string in strings:
        if pattern.match(string):
            matching += 1
    return matching


RULES, STRINGS = parse_data()
print(solve_p1(RULES, STRINGS))

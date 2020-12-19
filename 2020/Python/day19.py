import re


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

    def convert_to_regex(rule_idx: int) -> str:
        rule = rules[rule_idx]
        if isinstance(rule, str):
            return rule
        else:
            out = "("
            for rule_option in rule:
                out += "".join(map(convert_to_regex, rule_option)) + "|"
            return out[:-1] + ")"

    pattern_str = convert_to_regex(0)+"$"
    pattern = re.compile(pattern_str)
    matching = 0
    for string in strings:
        if pattern.match(string):
            matching += 1
    return matching


def solve_p2(rules, strings) -> int:

    def convert_to_regex2(rule_idx: int) -> str:
        rule = rules[rule_idx]
        if isinstance(rule, str):
            return rule
        else:
            out = "("
            if rule_idx not in [8, 11]:
                for rule_option in rule:
                    for index in rule_option:
                        out += convert_to_regex2(index)
                    out += "|"
                return out[:-1] + ")"
            elif rule_idx == 8:
                return out + convert_to_regex2(42) + "+)"
            elif rule_idx == 11:  # I'm not proud of this at all, but it worked
                return out + convert_to_regex2(42) + f"({convert_to_regex2(42)}({convert_to_regex2(42)}({convert_to_regex2(42)}{convert_to_regex2(31)})?{convert_to_regex2(31)})?{convert_to_regex2(31)})?" + convert_to_regex2(31) + ")"

    pattern_str = convert_to_regex2(0) + "$"
    pattern = re.compile(pattern_str)
    matching = 0
    for string in strings:
        if pattern.match(string):
            matching += 1
    return matching


RULES, STRINGS = parse_data()
print(solve_p1(RULES, STRINGS))
print(solve_p2(RULES, STRINGS))




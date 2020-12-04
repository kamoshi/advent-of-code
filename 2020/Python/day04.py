import re


passports = [[]]


with open("input.txt") as file:
    for line in file:
        line_ = line.rstrip()
        if len(line_) > 2:
            passports[-1].append(line_)
        else:
            if not passports[-1] == []:
                passports.append([])


parsed_passports = []


for passport in passports:
    tags = []
    for line in passport:
        tags += line.split()
    pass_dict = {}
    for tag in tags:
        (name, value) = tag.split(":")
        pass_dict[name] = value
    parsed_passports.append(pass_dict)


def solve_p1() -> int:

    def test_passport(passport):
        for tag_name in ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]:
            if tag_name not in passport:
                return False
        return True

    counted = 0
    for passport in parsed_passports:
        if test_passport(passport):
            counted += 1
    return counted


def solve_p2() -> int:

    TAGS_REGEX = [
        ("byr", re.compile(r"(19[2-9]\d)|(200[12])")),
        ("iyr", re.compile(r"201\d|2020")),
        ("eyr", re.compile(r"202\d|2030")),
        ("hgt", re.compile(r"((1[5-8]\d)|(19[0-3]))cm|((59|[67]\d)|7[0-6])in")),
        ("hcl", re.compile(r"#[0-9a-f]{6}")),
        ("ecl", re.compile(r"amb|blu|brn|gry|grn|hzl|oth")),
        ("pid", re.compile(r"\d{9}"))
    ]

    def test_tag(passport, tag, pattern: re.Pattern):
        if tag not in passport:
            return False
        if pattern.match(passport[tag]):
            return True
        return False

    def test_passport_adv(passport):
        result = []
        for (tag_name, pattern) in TAGS_REGEX:
            result.append(test_tag(passport, tag_name, pattern))
        return False not in result

    counted = 0
    for passport in parsed_passports:
        if test_passport_adv(passport):
            counted += 1

    return counted


print(solve_p1())
print(solve_p2())

import re
from typing import Tuple


def parse_data() -> Tuple[dict[str, Tuple[int, int, int, int]], list[int], list[list[int]]]:
    with open("input.txt") as file:
        constraints = {}
        my_ticket = []
        other_tickets = []

        part = 1
        for line in file:
            _line = line.rstrip()
            if _line == "your ticket:":
                part = 2
            elif _line == "nearby tickets:":
                part = 3
            elif not _line:
                continue
            else:
                if part == 1:
                    constraint_name, constraint_ranges = _line.split(":")
                    min1, max1, min2, max2 = map(int, re.split("-|or", constraint_ranges))
                    constraints[constraint_name] = (min1, max1, min2, max2)
                elif part == 2:
                    my_ticket = list(map(int, _line.split(",")))
                elif part == 3:
                    other_tickets.append(list(map(int, _line.split(","))))

        return constraints, my_ticket, other_tickets


def check_ticket_valid(ticket: list[int], constraints: dict[str, Tuple[int, int, int, int]]) -> Tuple[bool, int]:
    counter = 0
    valid = True

    for number in ticket:
        any_constraint = []
        for constraint in constraints:
            (min1, max1, min2, max2) = constraints[constraint]
            if min1 <= number <= max1 or min2 <= number <= max2:
                any_constraint.append(True)
        if True not in any_constraint:
            counter += number
            valid = False

    return valid, counter


def solve_p1(constraints: dict[str, Tuple[int, int, int, int]], tickets: list[list[int]]) -> int:
    counter = 0
    for ticket in tickets:
        _, ratio = check_ticket_valid(ticket, constraints)
        counter += ratio
    return counter


def solve_p2(constraints: dict[str, Tuple[int, int, int, int]], my_ticket: list[int], tickets: list[list[int]]) -> int:
    only_valid = []
    for ticket in tickets:
        valid, _ = check_ticket_valid(ticket, constraints)
        if valid:
            only_valid.append(ticket)

    # Generate sets for every field
    fields = {}
    for i in range(len(my_ticket)):
        possible = set()
        for constraint in constraints:
            possible.add(constraint)
        fields[i] = possible

    # Intersect sets with possibilities iteratively
    for ticket in only_valid:
        for i in range(len(ticket)):
            possible_here = set()
            for constraint in constraints:
                (min1, max1, min2, max2) = constraints[constraint]
                if min1 <= ticket[i] <= max1 or min2 <= ticket[i] <= max2:
                    possible_here.add(constraint)
            fields[i] = fields[i].intersection(possible_here)

    fields_list: list[Tuple[int, set]] = []
    for k, v in fields.items():
        fields_list.append((k, v))

    # Clean up sets to have unique values
    sorted_fields = sorted(fields_list, key=lambda elem: len(elem[1]))
    for i in range(len(sorted_fields)):
        _, s1 = sorted_fields[i]
        for j in range(i+1, len(sorted_fields)):
            k2, s2 = sorted_fields[j]
            sorted_fields[j] = (k2, s2.difference(s1))

    # Extract single values from sets
    fields_final = {}
    for k, s in sorted(sorted_fields, key=lambda elem: elem[0]):
        fields_final[k] = next(iter(s))

    result = 1
    for k, v in fields_final.items():
        if v[:9] == "departure":
            result *= my_ticket[k]

    return result


CONSTRAINTS, MY_TICKET, OTHER_TICKETS = parse_data()
print(solve_p1(CONSTRAINTS, OTHER_TICKETS))
print(solve_p2(CONSTRAINTS, MY_TICKET, OTHER_TICKETS))

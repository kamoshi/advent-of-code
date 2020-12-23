
def move_cups(cups: list[int]) -> list[int]:
    cups_round = cups[:]
    current = cups_round[0]
    taken = [cups_round[1], cups_round[2], cups_round[3]]
    cups_round = cups_round[4:]

    minimum, maximum = min(cups_round), max(cups_round)

    destination = current
    while (destination := destination - 1) in taken or destination < minimum:
        if destination < minimum:
            destination = maximum + 1

    for i in range(len(cups_round)):
        if cups_round[i] == destination:
            cups_round = cups_round[:i+1] + taken + cups_round[i+1:]
            break

    cups_round.append(current)
    return cups_round


def solve_p1(order: str, n: int) -> str:
    table = [int(x) for x in order]
    for i in range(n):
        table = move_cups(table)

    for i in range(len(table)):
        if table[i] == 1:
            return "".join([str(table[x%len(table)]) for x in range(i+1, len(table)+i)])
    return ""


print(solve_p1("389547612", 100))


# TODO: Part 2 should use different data structure

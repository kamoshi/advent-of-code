from typing import Tuple


def parse_data() -> Tuple[list[int], list[int]]:
    data = ([], [])
    mode = -1
    with open("input.txt") as file:
        for line in file:
            line = line.rstrip()

            if line == "Player 1:":
                mode = 0
            elif line == "Player 2:":
                mode = 1
            elif mode == -1 or not line:
                continue
            else:
                data[mode].append(int(line))
    return data


def solve_p1(player1: list[int], player2: list[int]) -> int:
    stack1 = list(reversed(player1))
    stack2 = list(reversed(player2))

    while len(stack1) > 0 and len(stack2) > 0:
        number1 = stack1.pop()
        number2 = stack2.pop()
        if number1 > number2:
            stack1.insert(0, number1)
            stack1.insert(0, number2)
        else:
            stack2.insert(0, number2)
            stack2.insert(0, number1)

    winning = stack1 if len(stack1) > 0 else stack2
    return sum(x*y for (x, y) in (zip(winning, [x for x in range(1, len(winning)+1)])))


PLAYER1, PLAYER2 = parse_data()
print(solve_p1(PLAYER1, PLAYER2))

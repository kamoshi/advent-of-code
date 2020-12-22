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


def play_game(cache: dict[str, int], player1: list[int], player2: list[int]) -> (int, list[int]):
    previous_rounds: set[str] = set()
    stack1, stack2 = list(reversed(player1)), list(reversed(player2))

    def round_snapshot() -> str:
        return f"{stack1}|{stack2}"

    def normal_round() -> None:
        _number1, _number2 = stack1.pop(), stack2.pop()
        if _number1 > _number2:
            stack1.insert(0, _number1)
            stack1.insert(0, _number2)
        else:
            stack2.insert(0, _number2)
            stack2.insert(0, _number1)

    while len(stack1) > 0 and len(stack2) > 0:
        if (snapshot := round_snapshot()) in previous_rounds:
            return 1, stack1
        else:
            previous_rounds.add(snapshot)

        if len(stack1) > stack1[len(stack1)-1] and len(stack2) > stack2[len(stack2)-1]:
            number1, number2 = stack1.pop(), stack2.pop()
            sub_deck1, sub_deck2 = stack1[1-number1:], stack2[1-number2:]

            # Check cache for winner
            if (sub_snapshot := f"{sub_deck1}|{sub_deck2}") not in cache:
                winner, _ = play_game(cache, sub_deck1, sub_deck2)
                cache[sub_snapshot] = winner
            else:
                winner = cache[sub_snapshot]

            if winner == 1:
                stack1.insert(0, number1)
                stack1.insert(0, number2)
            else:
                stack2.insert(0, number2)
                stack2.insert(0, number1)
        else:
            normal_round()

    if len(stack1) > 0:
        return 1, stack1
    else:
        return 2, stack2


# PART 2 is Broken for now
def solve_p2(player1: list[int], player2: list[int]) -> int:
    cache: dict[str, int] = {}
    winner, winning = play_game(cache, player1, player2)
    return sum(x*y for (x, y) in (zip(winning, [x for x in range(1, len(winning)+1)])))


print(solve_p2(PLAYER1, PLAYER2))

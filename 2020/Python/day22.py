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


def play_game(player1: list[int], player2: list[int], rec: bool = False) -> Tuple[int, list[int]]:
    past_rounds: set[str] = set()
    hand1, hand2 = player1[:], player2[:]

    def snapshot() -> str:
        return f"{hand1}{hand2}"

    while len(hand1) > 0 and len(hand2) > 0:
        if (curr_snapshot := snapshot()) in past_rounds:
            return 1, hand1

        past_rounds.add(curr_snapshot)
        card1, card2 = hand1.pop(0), hand2.pop(0)

        if rec and len(hand1) >= card1 and len(hand2) >= card2:
            winner, _ = play_game(hand1[:card1], hand2[:card2], rec=True)
        else:
            winner = 1 if card1 > card2 else 2

        if winner == 1:
            hand1.append(card1)
            hand1.append(card2)
        else:
            hand2.append(card2)
            hand2.append(card1)

    if len(hand2) == 0:
        return 1, hand1
    else:
        return 2, hand2


def score(hand: list[int]) -> int:
    return sum(x*y for (x, y) in zip(reversed(hand), [x for x in range(1, len(hand)+1)]))


PLAYER1, PLAYER2 = parse_data()
print(score(play_game(PLAYER1, PLAYER2, rec=False)[1]))
print(score(play_game(PLAYER1, PLAYER2, rec=True)[1]))

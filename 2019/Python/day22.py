from enum import Enum
from typing import Tuple


class Command(Enum):
    CUT = 1
    INC = 2
    STA = 4


def parse_data() -> list[Tuple[Command, int]]:
    output = []
    with open("input.txt") as file:
        for line in file:
            line = line.rstrip()
            if not line:
                continue
            elif line[:3] == "cut":
                output.append((Command.CUT, int(line[3:])))
            elif line[:19] == "deal with increment":
                output.append((Command.INC, int(line[19:])))
            elif line[:19] == "deal into new stack":
                output.append((Command.STA, 0))
    return output


class Hand:
    def __init__(self):
        self.hand = []

    def generate(self, n: int) -> 'Hand':
        self.hand = [x for x in range(n)]
        return self

    def deal_into_new_stack(self) -> 'Hand':
        self.hand = list(reversed(self.hand))
        return self

    def cut(self, n: int) -> 'Hand':
        if n >= 0:
            cache = self.hand[:n]
            self.hand = self.hand[n:]
            self.hand += cache
        else:
            cache = self.hand[n:]
            self.hand = self.hand[:n]
            self.hand = cache + self.hand
        return self

    def deal_with_increment(self, n: int) -> 'Hand':
        cache = [0 for _ in range(len(self.hand))]
        ptr_o = -1
        ptr_n = -n
        while (ptr_o := ptr_o + 1) < len(self.hand):
            cache[(ptr_n := ptr_n + n) % len(cache)] = self.hand[ptr_o]
        self.hand = cache
        return self

    def find_card(self, card: int) -> int:
        for i in range(len(self.hand)):
            if self.hand[i] == card:
                return i
        return -1

    def find_position(self, position: int) -> int:
        return self.hand[position]

    def __str__(self):
        return str(self.hand)


def solve_p1(tape: list[Tuple[Command, int]], cards: int) -> int:
    my_hand = Hand()
    my_hand.generate(cards)
    for command, param in tape:
        if command == Command.CUT:
            my_hand.cut(param)
        elif command == Command.INC:
            my_hand.deal_with_increment(param)
        elif command == Command.STA:
            my_hand.deal_into_new_stack()
    return my_hand.find_card(2019)


TAPE = parse_data()
print(solve_p1(TAPE, 10007))


def solve_p2(tape: list[Tuple[Command, int]], cards: int, repeats: int) -> int:
    my_hand = Hand()
    my_hand.generate(cards)
    for _ in range(repeats):
        for command, param in tape:
            if command == Command.CUT:
                my_hand.cut(param)
            elif command == Command.INC:
                my_hand.deal_with_increment(param)
            elif command == Command.STA:
                my_hand.deal_into_new_stack()
    return my_hand.find_position(2020)


# Takes way too long, need better solution
print(solve_p2(TAPE, 119315717514047, 101741582076661))

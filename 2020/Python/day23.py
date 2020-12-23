from itertools import chain
from typing import Tuple


# Implementation uses generic python list -> pretty inefficient; too slow for part 2
def move_cups(cups: list[int]) -> list[int]:
    cups_round = cups[4:]
    current = cups[0]
    taken = [cups[1], cups[2], cups[3]]
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


def to_hash_list(order: str, min_cups: int = 0) -> Tuple[dict[int, int], int, int]:
    hash_list = {}
    order = list(chain(map(int, order), [x for x in range(len(order)+1, min_cups+1)]))
    for i in range(len(order)):
        hash_list[int(order[i])] = int(order[(i+1) % len(order)])
    return hash_list, min(order), max(order)


def get_list_from_1(hash_list: dict[int, int]) -> str:
    out, next_cup = [], 1
    while (next_cup := hash_list[next_cup]) != 1:
        out.append(next_cup)
    return "".join(map(str, out))


# Implementation uses dictionary as a form of a singly-linked list, more efficient
def move_cups_hash(cups: dict[int, int], minimum: int, maximum: int, ptr: int) -> int:
    forward = current = ptr
    taken = [(forward := cups[forward]) for _ in range(3)]
    cups[current] = cups[taken[-1]]  # change pointers away from taken cups

    destination = current
    while (destination := destination - 1) in taken or destination < minimum:
        if destination < minimum:
            destination = maximum + 1

    forward = cups[destination]
    cups[destination], cups[taken[-1]] = taken[0], forward  # change pointers to include the taken cups
    return cups[current]


def solve_p1(order: str, moves: int) -> str:
    hash_list, minimum, maximum = to_hash_list(order, min_cups=0)
    ptr = int(order[0])
    for _ in range(moves):
        ptr = move_cups_hash(hash_list, minimum, maximum, ptr)
    return get_list_from_1(hash_list)


def solve_p2(order: str, moves: int, min_cups: int) -> int:
    hash_list, minimum, maximum = to_hash_list(order, min_cups)
    ptr = int(order[0])
    for _ in range(moves):
        ptr = move_cups_hash(hash_list, minimum, maximum, ptr)
    return hash_list[1] * hash_list[hash_list[1]]


print(solve_p1(order="389547612", moves=100))
print(solve_p2(order="389547612", moves=10000000, min_cups=1000000))

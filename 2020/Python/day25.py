from typing import Tuple


def parse_pks() -> Tuple[int, int]:
    with open("input.txt") as file:
        return int(file.readline()), int(file.readline())


def transform_step(value: int, subject: int) -> int:
    value *= subject
    value %= 20201227
    return value


def transform_loop(loop_value: int, subject: int) -> int:
    value = 1
    for _ in range(loop_value):
        value = transform_step(value, subject)
    return value


def find_loop_value(subject: int, desired_value: int) -> int:
    value = 1
    loop = 0
    while value != desired_value:
        value = transform_step(value, subject)
        loop += 1
    return loop


def solve_p1(public_key1: int, public_key2: int) -> int:
    loop_value1 = find_loop_value(subject=7, desired_value=public_key1)
    encryption_key = transform_loop(loop_value=loop_value1, subject=public_key2)
    return encryption_key


PK1, PK2 = parse_pks()
print(solve_p1(public_key1=PK1, public_key2=PK2))

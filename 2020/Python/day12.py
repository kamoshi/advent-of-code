from enum import Enum
from typing import Tuple


class Direction(Enum):
    NORTH = 1
    EAST = 2
    SOUTH = 4
    WEST = 8


def parse_data() -> list[(str, int)]:
    data = []
    with open("input.txt") as file:
        for line in file:
            char, *number = line.rstrip()
            data.append((char, int("".join(number))))
    return data


def apply_command(curr_pos: Tuple[int, int], curr_dir: Direction, command: Tuple[str, int]) -> ((int, int), Direction):
    curr_x, curr_y = curr_pos
    _char, _number = command

    def find_new_direction(old_direction: Direction, char: str, number: int) -> Direction:
        if char == "L":
            result = (old_direction.value << 4) >> ((number % 360) // 90)
            if result > 8:
                result = result >> 4
            return Direction(result)
        elif char == "R":
            result = old_direction.value << ((number % 360) // 90)
            if result > 8:
                result = result >> 4
            return Direction(result)

    if _char == 'N':
        return (curr_x, curr_y + _number), curr_dir
    elif _char == 'S':
        return (curr_x, curr_y - _number), curr_dir
    elif _char == 'E':
        return (curr_x - _number, curr_y), curr_dir
    elif _char == 'W':
        return (curr_x + _number, curr_y), curr_dir

    elif _char in "LR":
        return curr_pos, find_new_direction(curr_dir, _char, _number)

    elif _char == 'F':
        if curr_dir is Direction.NORTH:
            return (curr_x, curr_y + _number), curr_dir
        elif curr_dir is Direction.SOUTH:
            return (curr_x, curr_y - _number), curr_dir
        elif curr_dir is Direction.EAST:
            return (curr_x - _number, curr_y), curr_dir
        elif curr_dir is Direction.WEST:
            return (curr_x + _number, curr_y), curr_dir


def solve_p1(data: list[Tuple[str, int]]):
    current_direction = Direction.EAST
    current_position = 0, 0
    for command in data:
        new_position, new_direction = apply_command(current_position, current_direction, command)
        current_direction = new_direction
        current_position = new_position
    final_x, final_y = current_position
    return abs(final_x)+abs(final_y)


DATA = parse_data()
print(solve_p1(DATA))

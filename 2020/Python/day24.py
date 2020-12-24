from enum import Enum
from typing import Tuple


class Direction(Enum):
    E = 0
    SE = 1
    SW = 2
    W = 3
    NW = 4
    NE = 5


def convert_string(path: str) -> list[Direction]:
    out, i = [], 0
    while i < len(path):
        char = path[i]
        if char in ['s', 'n']:
            i += 1
            char += path[i]
        if char == "e":
            out.append(Direction.E)
        elif char == "w":
            out.append(Direction.W)
        elif char == "ne":
            out.append(Direction.NE)
        elif char == "nw":
            out.append(Direction.NW)
        elif char == "se":
            out.append(Direction.SE)
        elif char == "sw":
            out.append(Direction.SW)
        i += 1
    return out


def determine_neighbour(row: int, col: int, direction: Direction) -> Tuple[int, int]:
    if direction == Direction.E:
        return row, col+1
    elif direction == Direction.SE:
        return (row+1, col+1) if row % 2 == 0 else (row+1, col)
    elif direction == Direction.SW:
        return (row+1, col) if row % 2 == 0 else (row+1, col-1)
    elif direction == Direction.W:
        return row, col-1
    elif direction == Direction.NW:
        return (row-1, col) if row % 2 == 0 else (row-1, col-1)
    elif direction == Direction.NE:
        return (row-1, col+1) if row % 2 == 0 else (row-1, col)


def find_tile(start_r: int, start_c: int, path: str) -> Tuple[int, int]:
    path = convert_string(path)
    tile_r, tile_c = start_r, start_c
    for direction in path:
        tile_r, tile_c = determine_neighbour(tile_r, tile_c, direction)
    return tile_r, tile_c


def parse_data() -> list[str]:
    out = []
    with open("input.txt") as file:
        for line in file:
            out.append(line.rstrip())
    return out


def solve_p1(data: list[str]) -> int:
    black_tiles: set[Tuple[int, int]] = set()
    for line in data:
        found_tile = find_tile(0, 0, line)
        if found_tile in black_tiles:
            black_tiles.remove(found_tile)
        else:
            black_tiles.add(found_tile)
    return len(black_tiles)


DATA = parse_data()
print(solve_p1(DATA))
